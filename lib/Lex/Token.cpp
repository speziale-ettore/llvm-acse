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

  // Comments.
  TOKEN(LineComment)
  TOKEN(MultiLineComment)

  // Braces and co.
  TOKEN(LBrace)
  TOKEN(RBrace)
  TOKEN(LSquare)
  TOKEN(RSquare)
  TOKEN(LPar)
  TOKEN(RPar)

  // Separators/terminators.
  TOKEN(SemiColon)
  TOKEN(Colon)
  TOKEN(Comma)
  TOKEN(Assign)

  // Algebraic operators.
  TOKEN(Add)
  TOKEN(Sub)
  TOKEN(Mul)
  TOKEN(Div)
  TOKEN(Mod)

  // Relational operators.
  TOKEN(Less)
  TOKEN(LessOrEqual)
  TOKEN(Equal)
  TOKEN(NotEqual)
  TOKEN(GreaterOrEqual)
  TOKEN(Greater)

  // Bitwise operators.
  TOKEN(BAnd)
  TOKEN(BOr)
  TOKEN(BNot)
  TOKEN(LShift)
  TOKEN(RShift)

  // Logical operators.
  TOKEN(LAnd)
  TOKEN(LOr)

  // Keywords.
  TOKEN(Int)
  TOKEN(If)
  TOKEN(Else)
  TOKEN(Do)
  TOKEN(While)
  TOKEN(Read)
  TOKEN(Write)

  // Typed.
  TOKEN(Number)
  TOKEN(Identifier)

  #undef TOKEN

  default:
    OS << "Token-" << static_cast<unsigned>(Identifier);
  }

  return OS;
}
