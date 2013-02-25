//===- Lexer.h - Simple Scanner for LANCE -----------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef ACSE_LEX_LEXER_H
#define ACSE_LEX_LEXER_H

#include "llvm/Support/SourceMgr.h"

namespace acse {

class Lexer {
public:
  Lexer(llvm::SourceMgr &Srcs);

  Lexer(const Lexer &That) LLVM_DELETED_FUNCTION;
  const Lexer &operator=(const Lexer &That) LLVM_DELETED_FUNCTION;

private:
  llvm::SourceMgr &Srcs;
};

} // End namespace acse.

#endif // ACSE_LEX_LEXER_H
