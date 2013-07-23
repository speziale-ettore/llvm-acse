//===- Parser.h - Simple Descendent Parser for LANCE ------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef ACSE_PARSE_PARSER_H
#define ACSE_PARSE_PARSER_H

#include "acse/Lex/Lexer.h"
#include "acse/Parse/AbstractSyntaxTree.h"
#include "acse/Parse/ParsingStack.h"

#include "llvm/ADT/IntrusiveRefCntPtr.h"

namespace acse {

// This trait is used to parse lists with an arbitrary number of different
// separators. A forward declaration is needed because template specializations
// must be declared friend of the Parser class.
template <typename ListTy, typename NodeTy, typename SepTy>
struct ListParseTraits;

// Tag structure used to eliminate separator checks in the case the list does
// not employ them. See specializations of ListParseTraits for more info.
struct ListParseNoSepTag { };

// For parsing expressions, the Parser class uses a table which encodes the
// precedence of tokens. These precedences are fixed at design-time, hence they
// can be allocated on static memory. However, allocating objects on static
// memory is not good, since their initialization order is not guarantee.
//
// Whilst this problem does not effect initialization of primitive data-types --
// performed by the linker while loading the binary -- I decided to represent
// the operator precedence table through an abstract data type just for
// performing some experiments about singleton objects management.
//
// The table itself, is defined by PrecedenceTable::Table. In fact,
// PrecedenceTable is only a wrapper to access to the singleton instance of the
// table. The singleton is created on demand and destroyed as soon the last
// users release it.
//
// The table contains a row for each token, encoding its precedence as a number
// between 0 and 255. Precedence 0 is assigned to all tokens not used by the
// operator precedence parser -- see Parser::ParseExpression.
//
// Actually, since LANCE defines 9 precedence levels for binary expression, the
// actual maximum precedence value is 10 -- remember that precedence 0
// identifies unused tokens.
class PrecedenceTable {
private:
  class Table : public llvm::RefCountedBase<Table> {
  public:
    static const unsigned InvalidPrecedence = 0;

    static const unsigned MinPrecedence = 1;
    static const unsigned MaxPrecedence = 255;

  public:
    static Table *Get() {
      if(!Instance)
        Instance = new Table();

      return Instance;
    }

  private:
    static Table *Instance;

  private:
    Table() { Fill(); }

  public:
    ~Table() { Instance = 0; }

  public:
    unsigned operator[](Token::Id id) const { return Data[id]; }

  public:
    void Dump(llvm::raw_ostream &OS = llvm::errs()) const;

  private:
    // This method actually initialize the table -- look here for precedences.
    void Fill();

  private:
    uint8_t Data[Token::Count];
  };

public:
  static const unsigned InvalidPrecedence = Table::InvalidPrecedence;

  static const unsigned MinPrecedence = Table::MinPrecedence;
  static const unsigned MaxPrecedence = Table::MaxPrecedence;

public:
  PrecedenceTable() : Data(Table::Get()) { }

public:
  unsigned operator[](Token::Id Id) const { return (*Data)[Id]; }

public:
  void Dump(llvm::raw_ostream &OS = llvm::errs()) const;

private:
  llvm::IntrusiveRefCntPtr<Table> Data;
};

// After parsing an expression, the Parser class must translate it into an AST.
// A trivial solution to this problem would be using a switch statement:
//
// switch(Tok->GetId()) {
// case Token::Add:
//    return new AddExprAST(...);
// case Token::Sub:
//    return new SubExprAST(...);
// ...
// }
//
// However, in this way the cost of allocating an AST is linear in the number
// of tokens -- given N different iid token types, in the average case N / 2
// comparison are needed to translate the token into an AST.
//
// A super-fast alternative is provided by GCC: it defines an extension that
// allows to take the address of a label. In that way, a branch-table is used to
// jump to the code where a given token type is handled:
//
// static void *Table[Token::Count] = { &&CreateAdd, &&CreateSub, ... };
//
// goto Table[Tok->GetId()];
//
// CreateAdd: return new AddExprAST(...);
// CreateSub: return new SubExprAST(...);
//
// The drawback of this method is that is GCC specific, ans since the aim of
// this project is to avoid any kind of non-portable construct it cannot be
// used.
//
// The current implementation uses a branch table to address function pointers.
// Each function knowns how to build an expression AST for a given token type:
//
// static Builder Table[Token::Count] = { CreateAdd, CreateSub, ... };
//
// return Table[Tok->GetId()](...);
//
// With respect to the GCC extensions, we pay a little bit more in code size --
// we need a function for each token type we want to support -- and we are a
// little bit slower -- the GCC method employs a direct jump + direct call,
// while this method we employs and indirect call.
//
// Since the table used by the ExpressionBuilder should be allocated on static
// memory, I used the same trick applied for PrecedenceTable.
class ExpressionBuilder {
private:
  class Table : public llvm::RefCountedBase<Table> {
  public:
    typedef ExpressionAST *(*Builder)(ExpressionAST *,
                                      Token *,
                                      ExpressionAST *);

  public:
    static Table *Get() {
      if(!Instance)
        Instance = new Table();

      return Instance;
    }

  private:
    static Table *Instance;

  private:
    Table() { Fill(); }

  public:
    ~Table() { Instance = 0; }

  public:
    Builder operator[](const Token *Tok) const { return Data[Tok->GetId()]; }

  private:
    void Fill();

  private:
    Builder Data[Token::Count];
  };

public:
  ExpressionBuilder() : Data(Table::Get()) { }

public:
  ExpressionAST *Create(ExpressionAST *LHS,
                        Token *Oper,
                        ExpressionAST *RHS) const {
    return (*Data)[Oper](LHS, Oper, RHS );
  }

private:
  llvm::IntrusiveRefCntPtr<Table> Data;
};

// This class allows to extract an abstract syntax tree -- AST -- from a
// sequence of LANCE tokens. Parsing is done through an descendent algorithm.
//
// In the design phase, I chose to prefer right-recursive rules over
// left-recursive ones. This because, initially, this should be a LL parser.
// Later I decided to go for tentative parsing, but the grammar was already
// designed. However, using right-recursive rules will enable writing LL and LR
// parser without modifying the grammar, and thus comparisons can be easily
// made.
//
// The kind of recursion used directly influences the associativity of the
// grammar rules: left-recursion induces left-associative rules, while
// right-recursion produces right-associative rules. This can be a problem in
// some section of the grammar where syntactic associativity is bound to a
// semantic concepts, as in the case of arithmetic expressions:
//
// left-recursion: A + B + C = (A + B) + C
// right-recursion: A + B + C = A + (B + C)
//
// From the mathematical point of view, since + is a fully associative operator,
// it does not matter in which order sums are executed. However, in computer
// arithmetic this can be a problem. Let's suppose that A, B, and C are floating
// point numbers: the two results are different in general due to error
// propagation.
//
// Real languages thus strictly declare the associativity of this kind of rules.
// Left-associativity is usually preferred. From the parser implementation point
// of view, either the AST is built using another parsing algorithm -- e.g. LR
// parsing -- or this kind of rules are handled by a specialized parser -- e.g.
// an operator-precedence parser.
//
// Thus, does LANCE employ left-associativity? Well, initially I decided to go
// for right associativity, but I forgot to take into account the amount of
// precedence levels employed by LANCE expressions: 9.
//
// This means that even in the best case -- e.g. parsing an expression made-up
// of just a constant -- the parsing stack depth will be 9 -- the parser spends
// a lot of time just calling functions for non-terminal copy rules.
//
// So, I decided to take a look at a more smarter algorithm just for expressions
// parsing: Pratt's "Top Down Operator Precedence Parsing". As a side effect,
// this algorithm can handle right-associativity -- for, and only for,
// expression parsing LANCE supports right-associativity.
//
// Code organization is straightforward: the Run method tries to parse the token
// stream and to built the AST. Each rule in the grammar defines a parser that
// is implemented in a member function with a standard signature:
//
// XAST *ParseX()
//
// Where X is the name of the rule. Each of those member functions returns
// either an instance to an AST representing the parsed tokens or 0 in case of
// failure.
//
// The only exception to that rule regards the expression parsing code: indeed,
// while two rules are used to represent all expressions -- expression and
// primary_expression -- the second one is only employed for keep the code --
// and the grammar -- clean. The corresponding parsing function returns and
// ExpressionAST, not a PrimaryExpressionAST.
//
// Finally, In the case an error is found in the input file, the ReportError
// member function is called.
class Parser {
public:
  enum ErrorTy {
  #define ERROR(E, M) \
    E,
  #include "acse/Parse/ParserError.def"
  #undef ERROR

    ErrorCount
  };

public:
  Parser(Lexer &Lex) : Lex(Lex),
                       ErrorsFound(false) {
    // From the AST perspective, comments are only junk that do not carry any
    // information about the structure of the input stream: instruct the scanner
    // to automatically filter the input stream such that comments are never
    // shown to the parser.
    Lex.SetEatComments();
  }

private:
  Parser(const Parser &That) LLVM_DELETED_FUNCTION;
  const Parser &operator=(const Parser &That) LLVM_DELETED_FUNCTION;

public:
  bool Run();

  bool Success() const { return !ErrorsFound; }

  AbstractSyntaxTree *GetAST() const { return AST.get(); }
  AbstractSyntaxTree *TakeAST() { return AST.take(); }

private:
  VarDeclarationsAST *ParseVarDeclarations();
  NonEmptyVarDeclarationsAST *ParseNonEmptyVarDeclarations();
  VarDeclarationAST *ParseVarDeclaration();

  DeclarationListAST *ParseDeclarationList();
  DeclarationAST *ParseDeclaration();
  ScalarDeclarationAST *ParseScalarDeclaration();
  ArrayDeclarationAST *ParseArrayDeclaration();
  TypeAST *ParseType();

  ScalarInitializerAST *ParseScalarInitializer();
  ArrayInitializerAST *ParseArrayInitializer();
  InitializerListAST *ParseInitializerList();
  InitializerAST *ParseInitializer();

  StatementsAST *ParseStatements();
  NonEmptyStatementsAST *ParseNonEmptyStatements();
  StatementAST *ParseStatement();

  AssignStatementAST *ParseAssignStatement();
  ReadWriteStatementAST *ParseReadWriteStatement();
  NullStatementAST *ParseNullStatement();
  ControlStatementAST *ParseControlStatement();

  ScalarAssignmentAST *ParseScalarAssignment();
  ArrayAssignmentAST *ParseArrayAssignment();

  ReadStatementAST *ParseReadStatement();
  WriteStatementAST *ParseWriteStatement();

  ExpressionAST *ParseExpression();
  ExpressionAST *ParsePrimaryExpression();

private:
  // Generic list parsing algorithm for list with no separator. Require
  // specialization of the ListParseTraits trait.
  template <typename ListTy, typename NodeTy>
  ListTy *ParseList();

  // Generic list parsing algorithm for lists with a separator. Usage is
  // trivial, just invoke with the triplet of classes you like. Remember to:
  //
  // 1) specialize the ListParseTraits trait
  // 2) add the specialization as a friend of class Parser
  //
  // The template machinery will do all the magic.
  template <typename ListTy, typename NodeTy, typename SepTy>
  ListTy *ParseList();

  bool IsBinaryOperator(const Token *Tok) const {
    // The operator precedence table contains precedences for all tokens. Since
    // 1) the precedence is specified only for tokens used by the operator
    // precedence parser, and 2) that parser handles only binary expression, it
    // turns out that checking whether a given token is a binary operator is a
    // simple comparison.
    return Tok && PrecTable[Tok->GetId()] > PrecedenceTable::MinPrecedence;
  }

  unsigned GetPrecedence(const Token *Tok) const {
    // The operator precedence table contains precedences for all tokens.
    // However, the parser needs a special token to mark the bottom of the
    // parsing stack -- see Parser::ParseExpression.
    //
    // Since the stack contains pointers to tokens, I decided to use the 0
    // pointer to represent that special value. It must have the lowest possible
    // precedence.
    return Tok ? PrecTable[Tok->GetId()] : PrecedenceTable::InvalidPrecedence;
  }

  ExpressionAST *CreateBinaryExpression(ExpressionAST *LHS,
                                        Token *Oper,
                                        ExpressionAST *RHS) const {
    return ExprBuilder.Create(LHS, Oper, RHS);
  }

private:
  void ReportError(ErrorTy Error, llvm::SMLoc Loc);

private:
  Lexer &Lex;
  bool ErrorsFound;

  PrecedenceTable PrecTable;
  ExpressionBuilder ExprBuilder;

  llvm::OwningPtr<AbstractSyntaxTree> AST;

  #define LIST_TRAITS(L, N, S)                          \
  friend struct ListParseTraits<L ## AST, N ## AST, S>;

  LIST_TRAITS(NonEmptyVarDeclarations, VarDeclaration, ListParseNoSepTag)
  LIST_TRAITS(NonEmptyStatements, Statement, ListParseNoSepTag)

  #undef LIST_TRAITS

  #define LIST_TRAITS(L, N, S)                                 \
  friend struct ListParseTraits<L ## AST, N ## AST, S ## AST>;

  LIST_TRAITS(DeclarationList, Declaration, Comma)
  LIST_TRAITS(InitializerList, Initializer, Comma)

  #undef LIST_TRAITS
};

// This class defines a reasonable set of defaults for list parsing.
template <typename ListTy, typename NodeTy, typename SepTy>
struct DefaultListParseTraits {
  typedef NodeTy *(Parser::*NodeParser)();

  static NodeParser GetNodeParser();

  static SepTy *CreateSeparatorAST(typename SepTy::Token *Sep) {
    return new SepTy(Sep);
  }

  static ListTy *CreateListAST(NodeTy *Node, SepTy *Sep, ListTy *List) {
    return new ListTy(Node, Sep, List);
  }

  static ListTy *CreateListAST(NodeTy *Node) {
    return new ListTy(Node);
  }
};

// A reasonable set of defaults for list parsing, suited for list with no
// separators at all.
template <typename ListTy, typename NodeTy>
struct DefaultListParseTraits<ListTy, NodeTy, ListParseNoSepTag> {
  typedef NodeTy *(Parser::*NodeParser)();

  static NodeParser GetNodeParser();

  static ListTy *CreateListAST(NodeTy *Node, ListTy *List) {
    return new ListTy(Node, List);
  }

  static ListTy *CreateListAST(NodeTy *Node) {
    return new ListTy(Node);
  }
};

// This trait is used by the generic list parsing algorithms to get information
// about the actual list type and to build list-specific data structures.
//
// The following members must be defined:
//
// - typedef NodeTy *(Parser::*NodeParser)(): signature of the Parser member
//   function that allows to parse an element of the list
//
// - static NodeParser GetNodeParser(): used to get the Parser member function
//   that parses a list element
//
// - static SepTy *CreateSeparatorAST(typename SepTy::Token *Sep): builds an
//   AST representing the given token
//
// - static ListTy *CreateListAST(NodeTy *Node, SepTy *Sep, ListTy *List):
//   builds a list by concatenating the Node element, through the Sep separator,
//   to the list given as as last argument
//
// - static ListTy *CreateListAST(NodeTy *Node): build a singleton list,
//   containing the given Node
//
// Usually you are not required to implement all these fields. Derive from
// DefaultListParseTraits to access to reasonable set of pre-defined members.
template <typename ListTy, typename NodeTy, typename SepTy>
struct ListParseTraits
  : public DefaultListParseTraits<ListTy, NodeTy, SepTy> { };

// The generic list parser uses the ListParseTraits, hence I had to implement it
// here, after trait declaration.

template <typename ListTy, typename NodeTy>
inline
ListTy *Parser::ParseList() {
  typedef ListParseTraits<ListTy, NodeTy, ListParseNoSepTag> ParseTraits;

  llvm::SmallVector<NodeTy *, 4> Stack;

  // Optimization notice: the parser is obtained via a function pointer,
  // however, the compiler is smart enough to inline all the calls to the trait
  // functions an later performing constant propagation: indirect calls becomes
  // direct calls.
  NodeTy *(Parser::*NodeParser)() = ParseTraits::GetNodeParser();

  // Iterative version of recursive descendent calls.
  while(NodeTy *Node = (this->*NodeParser)())
    Stack.push_back(Node);

  ListTy *List = 0;

  // We reach the innermost parser. Simulate returning from recursive calls
  // by popping elements from the stack and build the tree.
  while(!Stack.empty()) {
    List = ParseTraits::CreateListAST(Stack.back(), List);
    Stack.pop_back();
  }

  return List;
}

template <typename ListTy, typename NodeTy, typename SepTy>
inline
ListTy *Parser::ParseList() {
  typedef ListParseTraits<ListTy, NodeTy, SepTy> ParseTraits;
  typedef typename SepTy::Token SepToken;

  llvm::SmallVector<std::pair<NodeTy *, SepTy *>, 4> Stack;
  NodeTy *Node;

  // Optimization notice: the parser is obtained via a function pointer,
  // however, the compiler is smart enough to inline all the calls to the trait
  // functions an later performing constant propagation: indirect calls becomes
  // direct calls.
  NodeTy *(Parser::*NodeParser)() = ParseTraits::GetNodeParser();

  // We should parse at least one element.
  if((Node = (this->*NodeParser)()))
    Stack.push_back(std::make_pair(Node, static_cast<SepTy *>(0)));

  // While a list element has been parser, try to parse the next one.
  while(Node) {
    llvm::OwningPtr<SepTy> Sep;

    // Separator not found: no more list elements to parse.
    if(!llvm::dyn_cast_or_null<SepToken>(Lex.Peek(0)))
      break;

    Sep.reset(ParseTraits::CreateSeparatorAST(Lex.TakeAs<SepToken>()));

    // We parsed a separator, so there must be an element.
    if((Node = (this->*NodeParser)())) {
      std::pair<NodeTy *, SepTy *> &Prev = Stack.back();
      Prev.second = Sep.take();

      Stack.push_back(std::make_pair(Node, static_cast<SepTy *>(0)));
    }
  }

  ListTy *List = 0;

  // We reached the innermost parser. Simulate returning from recursive calls by
  // popping elements from the stack and build the tree bottom-up.
  while(!Stack.empty()) {
    std::pair<NodeTy *, SepTy *> &Cur = Stack.back();
    List = ParseTraits::CreateListAST(Cur.first, Cur.second, List);

    Stack.pop_back();
  }

  return List;
}

} // End namespace acse.

#endif // ACSE_PARSE_PARSER_H
