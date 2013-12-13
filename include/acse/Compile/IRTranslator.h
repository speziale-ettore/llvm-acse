//===- IRTranslator.h - Translate LANCE IR to LLVM IR -----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef ACSE_COMPILE_IRTRANSLATOR_H
#define ACSE_COMPILE_IRTRANSLATOR_H

#include "acse/Compile/Pass.h"

#include "llvm/IR/Module.h"

namespace acse {

// TODO: comment
class IRTranslator : public Pass {
public:
  IRTranslator(llvm::Module *&Module) : Module(Module) { }

private:
  IRTranslator(const IRTranslator &That)
  LLVM_DELETED_FUNCTION;

  const IRTranslator &operator=(const IRTranslator &That)
  LLVM_DELETED_FUNCTION;

public:
  virtual bool Run(AbstractSyntaxTree &AST);

private:
  llvm::Module *&Module;
};

} // End namespace acse.

#endif // ACSE_COMPILE_IRTRANSLATOR_H
