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
  #define SIMPLE(R, C) \
  case Token::C:       \
    OS << # C;         \
    break;
  #define KEYWORD(R, C) \
  case Token::C:        \
    OS << # C;          \
    break;
  #define TYPED(C) \
  case Token::C:   \
    OS << # C;     \
    break;
  #include "acse/Lex/Tokens.def"
  #undef SIMPLE
  #undef KEYWORD
  #undef TYPED

  default:
    OS << "TOKEN-" << static_cast<unsigned>(Identifier);
  }

  return OS;
}
