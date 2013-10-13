//===- SymbolChecker.h - Check Symbols in the AST ---------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef ACSE_COMPILE_SYMBOLSCHECKER_H
#define ACSE_COMPILE_SYMBOLSCHECKER_H

#include "acse/Compile/Pass.h"

namespace acse {

// This pass checks symbol declarations. Indeed, from the grammar point of view
// it is possible to 1) declare the same symbols multiple times, 2) declare an
// array which initializer is bigger than the array itself. Handling these
// features from the grammar point of view, is not feasible -- the language
// would not be context free -- thus compilers often checks these properties
// while parsing. Due to its aim to be a teaching compiler, ACSE performs 1 and
// 2 using specialized passes, thus pointing out the impossibility of
// performing them at parsing time due to language constraints.
class SymbolsChecker : public Pass {
public:
  SymbolsChecker() { }

private:
  SymbolsChecker(const SymbolsChecker &That)
  LLVM_DELETED_FUNCTION;

  const SymbolsChecker &operator=(const SymbolsChecker &That)
  LLVM_DELETED_FUNCTION;

public:
  virtual bool Run(AbstractSyntaxTree &AST);
};

} // End namespace acse.

#endif // ACSE_COMPILE_SYMBOLSCHECKER_H
