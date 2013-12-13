//===- IRTranslator.cpp - Translate LANCE IR to LLVM IR ---------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "acse/Compile/IRTranslator.h"
#include "acse/IR/AbstractSyntaxTreeVisitor.h"

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"

using namespace acse;

namespace {

class AbstractSyntaxTreeTranslator
  : public PreOrderAbstractSyntaxTreeVisitor<AbstractSyntaxTreeTranslator> {
public:
  AbstractSyntaxTreeTranslator(const AbstractSyntaxTree &AST)
    : PreOrderAbstractSyntaxTreeVisitor<AbstractSyntaxTreeTranslator>(AST),
      Ctx(llvm::getGlobalContext()) { }

public:
  bool TranslateAST(llvm::Module *&Module) {
    ErrorsFound = false;

    // Save the module such as the visitor can access to it at every step.
    this->Module = &Module;

    // Start visiting the AST in pre-order. This allow to handle most of the
    // nodes in the AST. For some constructs -- e.g. expressions -- a post-order
    // visitor is needed -- we will switch when needed.
    Visit();

    return !ErrorsFound;
  }

public:
  NextAction VisitProgram(const ProgramAST &Prog) {
    // Create a new module to hold the whole translation unit.
    *Module = new llvm::Module("", Ctx);

    // Set the module target.
    (*Module)->setTargetTriple(LLVM_DEFAULT_TARGET_TRIPLE);

    return Continue;
  }

  NextAction VisitVarDeclaration(const VarDeclarationAST &Decl) {
    // Set the type of the declarations now. It will be read by all subsequent
    // scalar and array declarations. For scalar, it will be used as the type of
    // the declared variable, for arrays it will be used as the element type.
    //
    // Since ACSE supports only two types, a full type system is not needed.
    DeclTy = llvm::Type::getInt32Ty(Ctx);

    return Continue;
  }

  NextAction VisitScalarDeclaration(const ScalarDeclarationAST &Decl) {
    llvm::Type *Ty = DeclTy;

    (*Module)->getOrInsertGlobal(Decl.GetScalarName(), Ty);

    return Continue;
  }

  NextAction VisitArrayDeclaration(const ArrayDeclarationAST &Decl) {
    llvm::Type *Ty = llvm::ArrayType::get(DeclTy, Decl.GetArraySize());

    (*Module)->getOrInsertGlobal(Decl.GetArrayName(), Ty);

    return Continue;
  }

private:
  llvm::LLVMContext &Ctx;

  bool ErrorsFound;

  llvm::Module **Module;
  llvm::Type *DeclTy;
};

} // End anonymous namespace.

bool IRTranslator::Run(AbstractSyntaxTree &AST) {
  AbstractSyntaxTreeTranslator Translator(AST);
  return Translator.TranslateAST(Module);
}
