//===- Pass.h - An ACSE Compiler Pass ---------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef ACSE_COMPILE_PASS_H
#define ACSE_COMPILE_PASS_H

#include "acse/IR/AbstractSyntaxTree.h"

namespace acse {

class Pass {
protected:
  Pass() { }

private:
  Pass(const Pass &That) LLVM_DELETED_FUNCTION;
  const Pass &operator=(const Pass &That) LLVM_DELETED_FUNCTION;

public:
  virtual bool Run(AbstractSyntaxTree &AST) = 0;
};

} // End namespace acse.

#endif // ACSE_COMPILE_PASS_H
