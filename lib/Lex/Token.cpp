//===- Token.cpp - LANCE Token Definitions ----------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "acse/Lex/Token.h"

using namespace acse;

llvm::raw_ostream &acse::operator<<(llvm::raw_ostream &OS,
                                    Token::Id Identifier) {
  switch(Identifier) {
  #define TOKEN(I) \
  case Token::I:   \
    OS << # I;     \
    break;
  #include "acse/Lex/Token.def"
  #undef TOKEN

  default:
    OS << "Token-" << static_cast<unsigned>(Identifier);
  }

  return OS;
}
