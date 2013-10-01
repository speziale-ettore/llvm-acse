//===- CompilerInstance.h - A Compiler for LANCE ----------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef ACSE_COMPILE_COMPILERINSTANCE_H
#define ACSE_COMPILE_COMPILERINSTANCE_H

#include "llvm/Support/SourceMgr.h"

namespace acse {

// TODO: comment.
class CompilerInstance {
public:
  CompilerInstance(llvm::SourceMgr &Srcs, llvm::raw_ostream &Dst)
    : Srcs(Srcs),
      Dst(Dst) { }

private:
  CompilerInstance(const CompilerInstance &That)
  LLVM_DELETED_FUNCTION;

  const CompilerInstance &operator=(const CompilerInstance &That)
  LLVM_DELETED_FUNCTION;

public:
  bool Run();

  bool Success() const { return !ErrorsFound; }

private:
  llvm::SourceMgr &Srcs;
  llvm::raw_ostream &Dst;

  bool ErrorsFound;
};

} // End namespace acse.

#endif // ACSE_COMPILE_COMPILERINSTANCE_H
