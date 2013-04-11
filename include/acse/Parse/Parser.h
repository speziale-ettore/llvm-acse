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

  template <typename ListTy,
            typename NodeTy,
            typename SepTy,
            NodeTy *(Parser::*NodeParser)()>
  ListTy *ParseList() {
    llvm::SmallVector<std::pair<NodeTy *, SepTy *>, 4> Stack;

    // We should parse at least one element.
    if(NodeTy *Node = (this->*NodeParser)())
      Stack.push_back(std::make_pair(Node, static_cast<SepTy *>(0)));
    else
      return 0;

    // If current token is the separator, then we expect to parse at least
    // another element. Please notice that the separator is bound to the
    // previously parsed element, not the one we are going to parse.
    while(llvm::dyn_cast_or_null<typename SepTy::Token>(Lex.Peek(0))) {
      SepTy *Sep = new SepTy(Lex.TakeAs<typename SepTy::Token>());

      std::pair<NodeTy *, SepTy *> &Prev = Stack.back();
      Prev.second = Sep;

      // We parsed a separator, so there must be an element.
      if(NodeTy *Node = (this->*NodeParser)())
        Stack.push_back(std::make_pair(Node, static_cast<SepTy *>(0)));
    }

    ListTy *List = 0;

    // We reach the innermost parser. Simulate returning from recursive calls by
    // popping elements from the stack and build the tree bottom-up.
    while(!Stack.empty()) {
      std::pair<NodeTy *, SepTy *> &Cur = Stack.back();
      List = new ListTy(Cur.first, Cur.second, List);
      Stack.pop_back();
    }

    return List;
  }

  void ReportError(ErrorTy Error, llvm::SMLoc Loc);

private:
  Lexer &Lex;
  bool ErrorsFound;

  llvm::OwningPtr<AbstractSyntaxTree> AST;
};

} // End namespace acse.

#endif // ACSE_LEX_PARSER_H
