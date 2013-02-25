//===- Lexer.cpp - Simple Scanner for LANCE ---------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "acse/Lex/Lexer.h"

using namespace acse;

Lexer::Lexer(llvm::SourceMgr &Srcs) : Srcs(Srcs) {
  assert(Srcs.getNumBuffers() == 1 && "ACSE scanner works with one file");
}
