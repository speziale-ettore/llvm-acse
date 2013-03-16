//===- Parser.h - Simple LL Parser for LANCE --------------------*- C++ -*-===//
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

#include "llvm/ADT/OwningPtr.h"

namespace acse {

// This class allows to extract an abstract syntax tree -- AST -- from a
// sequence of LANCE tokens. Parsing is done through an LL algorithm. From the
// language theory point of view, this enforces us to prefer right-recursive
// rules other than left-recursive rules during grammar design -- LL parsers
// cannot handle left-recursion.
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
// of view, either the AST is built by a LR parser or this kind of rules are
// handled by a specialized parser -- e.g. an operator-precedence parser.
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

  Parser(const Parser &That) LLVM_DELETED_FUNCTION;
  const Parser &operator=(const Parser &That) LLVM_DELETED_FUNCTION;

public:
  bool Run();

  bool Success() const { return !ErrorsFound; }

  ProgramAST *GetAST() const { return AST.get(); }
  ProgramAST *TakeAST() { return AST.take(); }

private:
  VarDeclarationsAST *ParseVarDeclarations();
  NonEmptyVarDeclarationsAST *ParseNonEmptyVarDeclarations();
  VarDeclarationAST *ParseVarDeclaration();

  DeclarationListAST *ParseDeclarationList();
  DeclarationAST *ParseDeclaration();
  ScalarDeclarationAST *ParseScalarDeclaration();
  ArrayDeclarationAST *ParseArrayDeclaration();
  InitializerAST *ParseInitializer();

  StatementsAST *ParseStatements();

  void ReportError(ErrorTy Error, llvm::SMLoc Loc);

private:
  Lexer &Lex;
  bool ErrorsFound;

  llvm::OwningPtr<ProgramAST> AST;
};

} // End namespace acse.

#endif // ACSE_LEX_PARSER_H
