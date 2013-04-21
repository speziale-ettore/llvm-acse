//===- Parser.h - Simple Descendent Parser for LANCE ------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef ACSE_LEX_PARSER_H
#define ACSE_LEX_PARSER_H

#include "acse/Lex/Lexer.h"
#include "acse/Parse/AbstractSyntaxTree.h"

namespace acse {

// This trait is used to parse lists with an arbitrary number of different
// separators. A forward declaration is needed because template specializations
// must be declared friend of the Parser class.
template <typename ListTy, typename NodeTy, typename SepTy>
struct ListParseTraits;

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
// Thus, does LANCE employ left-associativity? No, but it does not matter,
// because in LANCE the only case where associativity is important is on
// arithmetic expressions, but since only fixed point arithmetic is supported,
// choosing left-associativity or right-associativity is equivalent: the
// expression value will be the same.
//
// Code organization is straightforward: the Run method tries to parse the token
// stream and to built the AST. Each rule in the grammar defines a parser that
// is implemented in a member function with a standard signature:
//
// XAST *ParseX()
//
// Where X is the name of the rule. Each of those member functions returns
// either an instance to an AST representing the parsed tokens or 0 in case of
// failure. In the case an error is found in the input file, the ReportError
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

  // Since a lot of things we should parse are lists, I defined this template
  // function for parsing them. Indeed, the only difference between lists are:
  //
  // - ListTy: its type
  // - NodeTy: the type of its nodes
  // - NodeParser: the member function used to parse a list element
  //
  // I had to use some C++ black magic, but in this way code is much more
  // readable.
  //
  // TODO: port to new interface and move below.
  template <typename ListTy, typename NodeTy, NodeTy *(Parser::*NodeParser)()>
  ListTy *ParseList() {
    llvm::SmallVector<NodeTy *, 4> Stack;

    // Iterative version of recursive descendent calls.
    while(NodeTy *Node = (this->*NodeParser)())
      Stack.push_back(Node);

    ListTy *List = 0;

    // We reach the innermost parser. Simulate returning from recursive calls
    // by popping elements from the stack and build the tree.
    while(!Stack.empty()) {
      List = new ListTy(Stack.back(), List);
      Stack.pop_back();
    }

    return List;
  }

private:
  // Since I do not want to use C++11, we cannot perform partial specialization
  // of member function templates. The standard trick to achieve the same
  // result with older version of C++ is to declare those function as member of
  // a class, which can then be partially specialized.
  template <typename ListTy, typename NodeTy, typename SepTy>
  struct ParseListHelper {
    static TokenAST *ParseSeparator(Lexer &Lex);
    static ListTy *CreateList(NodeTy *Node, TokenAST *Sep, ListTy *List);
  };

private:
  // Generic list parsing algorithm. Usage is trivial, just invoke with the
  // triplet of classes you like. Remeber to:
  //
  // 1) specialize the ListParseTraits trait
  // 2) add the specialization as a friend of class Parser
  //
  // For supporting multiple separators, properly define the NextSeparator field
  // of ListParseTraits. To stop recursion, set it to ListParseEndTag.
  template <typename ListTy, typename NodeTy, typename SepTy>
  ListTy *ParseList();

  template <typename ListTy, typename NodeTy, typename SepTy>
  TokenAST *ParseListSeparator() {
    return ParseListHelper<ListTy, NodeTy, SepTy>::ParseSeparator(Lex);
  }

  template <typename ListTy, typename NodeTy, typename SepTy>
  ListTy *CreateList(NodeTy *Node, TokenAST *Sep, ListTy *List) {
    return ParseListHelper<ListTy, NodeTy, SepTy>::CreateList(Node, Sep, List);
  }

private:
  void ReportError(ErrorTy Error, llvm::SMLoc Loc);

private:
  Lexer &Lex;
  bool ErrorsFound;

  llvm::OwningPtr<AbstractSyntaxTree> AST;

  friend struct ListParseTraits<DeclarationListAST, DeclarationAST, CommaAST>;
  friend struct ListParseTraits<InitializerListAST, InitializerAST, CommaAST>;
};


// Tag structure used to stop template expansion of separators of a list. See
// specializations of ListParseTraits for more information.
struct ListParseEndTag { };

// This class defines a reasonable set of defaults for list parsing. It supports
// lists with only one kind of separator. If your list elements can be separated
// by more than just one kind of separator, derive from this class and
// specialize the ListParseTraits trait.
template <typename ListTy, typename NodeTy, typename SepTy>
struct DefaultListParseTraits {
  typedef NodeTy *(Parser::*NodeParser)();
  typedef ListParseEndTag NextSeparator;

  static NodeParser GetNodeParser();

  static TokenAST *CreateSeparatorAST(typename SepTy::Token *Sep) {
    return new SepTy(Sep);
  }

  static ListTy *CreateListAST(NodeTy *Node, SepTy *Sep, ListTy *List) {
    return new ListTy(Node, Sep, List);
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
// - typedef YourSeparator NextSeparator: the next separator to try if parsing
//   with the current one fails. Use ListParseEndTag as YourSeparator to end the
//   compile-time recursion
//
// - static NodeParser GetNodeParser(): used to get the Parser member function
//   that parses a list element
//
// - static TokenAST *CreateSeparatorAST(typename SepTy::Token *Sep): builds an
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

// Parse the separator of a list. On fail, try using the next separator.
template <typename ListTy, typename NodeTy, typename SepTy>
inline
TokenAST *Parser::ParseListHelper<ListTy, NodeTy, SepTy>
                ::ParseSeparator(Lexer &Lex) {
  typedef ListParseTraits<ListTy, NodeTy, SepTy> ParseTraits;

  typedef typename SepTy::Token SepTokTy;
  typedef typename ParseTraits::NextSeparator NextSepTy;

  if(llvm::dyn_cast_or_null<SepTokTy>(Lex.Peek(0)))
    return ParseTraits::CreateSeparatorAST(Lex.TakeAs<SepTokTy>());
  else
    return ParseListHelper<ListTy, NodeTy, NextSepTy>::ParseSeparator(Lex);
}

// Create a list holding the given triplet. Since each separator can be bound to
// a different subclass of ListTy, in case the given separator Sep cannot be
// used to create a suitable list node, the next one is considered.
template <typename ListTy, typename NodeTy, typename SepTy>
inline
ListTy *Parser::ParseListHelper<ListTy, NodeTy, SepTy>
              ::CreateList(NodeTy *Node, TokenAST *Sep, ListTy *List) {
  typedef ListParseTraits<ListTy, NodeTy, SepTy> ParseTraits;

  assert((Sep && List || !Sep && !List) && "Missing elements");

  if(!Sep)
    return ParseTraits::CreateListAST(Node);

  else if(SepTy *CastedSep = llvm::dyn_cast_or_null<SepTy>(Sep))
    return ParseTraits::CreateListAST(Node, CastedSep, List);

  else
    return ParseListHelper<ListTy, NodeTy, SepTy>::CreateList(Node, Sep, List);
}

// Partial specialization which enables to stop compile-time recursion of the
// generic list parser algorithms.
template <typename ListTy, typename NodeTy>
struct Parser::ParseListHelper<ListTy, NodeTy, ListParseEndTag> {
  static TokenAST *ParseSeparator(Lexer &Lex) {
    return 0;
  }

  static ListTy *CreateList(NodeTy *Node, TokenAST *Sep, ListTy *List) {
    return 0;
  }
};

template <typename ListTy, typename NodeTy, typename SepTy>
inline
ListTy *Parser::ParseList() {
  typedef ListParseTraits<ListTy, NodeTy, SepTy> ParseTraits;

  llvm::SmallVector<std::pair<NodeTy *, TokenAST *>, 4> Stack;
  NodeTy *Node;

  // Optimization notice: the parser is obtained via a function pointer,
  // however, the compiler is smart enough to inline all the calls to the trait
  // functions an later performing constant propagation: indirect calls becomes
  // direct calls.
  NodeTy *(Parser::*NodeParser)() = ParseTraits::GetNodeParser();

  // We should parse at least one element.
  if((Node = (this->*NodeParser)()))
    Stack.push_back(std::make_pair(Node, static_cast<TokenAST *>(0)));

  // While a list element has been parser, try to parse the next one.
  while(Node) {
    llvm::OwningPtr<TokenAST> Sep(ParseListSeparator<ListTy, NodeTy, SepTy>());

    // Separator not found: no more list elements to parse.
    if(!Sep)
      break;

    // We parsed a separator, so there must be an element.
    if((Node = (this->*NodeParser)())) {
      std::pair<NodeTy *, TokenAST *> &Prev = Stack.back();
      Prev.second = Sep.take();

      Stack.push_back(std::make_pair(Node, static_cast<TokenAST *>(0)));
    }
  }

  ListTy *List = 0;

  // We reached the innermost parser. Simulate returning from recursive calls by
  // popping elements from the stack and build the tree bottom-up.
  while(!Stack.empty()) {
    std::pair<NodeTy *, TokenAST *> &Cur = Stack.back();
    List = CreateList<ListTy, NodeTy, SepTy>(Cur.first, Cur.second, List);

    Stack.pop_back();
  }

  return List;
}

} // End namespace acse.

#endif // ACSE_LEX_PARSER_H
