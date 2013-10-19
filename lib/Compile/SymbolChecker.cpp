//===- SymbolChecker.h - Check Symbols in the AST ---------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "acse/Compile/SymbolsChecker.h"
#include "acse/IR/AbstractSyntaxTreeVisitor.h"

#include "llvm/ADT/StringSet.h"

using namespace acse;

namespace {

class AbstractSyntaxTreeChecker
  : public PreOrderAbstractSyntaxTreeVisitor<AbstractSyntaxTreeChecker> {
public:
  AbstractSyntaxTreeChecker(const AbstractSyntaxTree &AST)
    : PreOrderAbstractSyntaxTreeVisitor<AbstractSyntaxTreeChecker>(AST) { }

public:
  bool CheckAST() {
    Symbols.clear();
    ErrorsFound = false;

    // Visit all the declaration in the AST. If an error is found, the
    // ErrorsFound variable will be set to true at the point where the failure
    // is detected.
    Visit();

    return !ErrorsFound;
  }

  NextAction VisitScalarDeclaration(const ScalarDeclarationAST &Decl) {
    if(!Symbols.insert(Decl.GetScalarName())) {
      // TODO: report error.
      ErrorsFound = true;
    }

    return SkipChildren;
  }

  NextAction VisitArrayDeclaration(const ArrayDeclarationAST &Decl) {
    // An array can be declared only once.
    if(!Symbols.insert(Decl.GetArrayName())) {
      // TODO: report error.
      ErrorsFound = true;

    // In the case the array is provided with an initializer ...
    } else if(Decl.HasInitializer()) { 
      const ArrayInitializerAST *Init = Decl.GetInitializer();

      // ... its size must not exceed the array size.
      if(Decl.GetArraySize() < Init->GetInitializerSize()) {
        // TODO: report error.
        ErrorsFound = true;
      }
    }

    return SkipChildren;
  }

  // Since this visitor is only interested on declarations, do not visit any
  // statement -- nothing do to here.
  NextAction VisitStatements(const StatementsAST &Stmts) {
    return Terminate;
  }

private:
  llvm::StringSet<> Symbols;
  bool ErrorsFound;
};

} // End anonymous namespace.

bool SymbolsChecker::Run(AbstractSyntaxTree &AST) {
  AbstractSyntaxTreeChecker Checker(AST);
  return Checker.CheckAST();
}
