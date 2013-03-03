//===- Lexer.cpp - Simple Scanner for LANCE ---------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "acse/Lex/Lexer.h"

#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/MemoryBuffer.h"

using namespace acse;

//
// Lexer implementation.
//

Lexer::Lexer(llvm::SourceMgr &Srcs) : Srcs(Srcs) {
  assert(Srcs.getNumBuffers() == 1 && "ACSE scanner works with only one file");

  const llvm::MemoryBuffer *Src = Srcs.getMemoryBuffer(0);

  Start = Src->getBufferStart();
  End = Src->getBufferEnd();
}

bool Lexer::ScanSimple() {
  ScanSpaces();

  // Will hold the token + its length.
  llvm::StringRef Token;

  #define SIMPLE(R, C)                                                         \
  {                                                                            \
  if(Start.startswith(Token = R)) {                                            \
    CachedTokens.push_back(new C ## Tok(Start, Start.begin() + Token.size())); \
    Start = Start.drop_front(Token.size());                                    \
    return true;                                                               \
  }                                                                            \
  }
  #define KEYWORD(R, C)
  #define TYPED(C)
  #include "acse/Lex/Tokens.def"
  #undef SIMPLE
  #undef KEYWORD
  #undef TYPED

  return false;
}

bool Lexer::ScanNumber() {
  ScanSpaces();

  llvm::StringRef::iterator I = Start.begin(),
                            J = I,
                            E = Start.end();

  // At least one digit ...
  if(!IsDigit(*J))
    return false;

  // If the number start with '0' it must be '0'.
  if(*J == '0') {
    llvm::StringRef::iterator K = ++J;

    for(; K != E && IsDigit(*K); ++K)
      ;

    if(K != J) {
      ReportError(MalformedNumber);
      return false;
    }

  // Otherwise, other optional digits are allowed.
  } else {
    for(++J; J != E && IsDigit(*J); ++J)
      ;
  }

  llvm::StringRef Token(I, J - I);
  NumberTok::NumberTy N;

  Token.getAsInteger(NumberTok::Radix, N);

  CachedTokens.push_back(new NumberTok(I, J, N));
  Start = Start.drop_front(J - I);

  return true;
}

bool Lexer::ScanIdentifier() {
  ScanSpaces();

  llvm::StringRef::iterator I = Start.begin(),
                            J = I,
                            E = Start.end();

  // Must start with an ASCII letter or underscore.
  if(!IsLetter(*J) && *J != '_')
    return false;

  // Optional list of ASCII letters, ASCII digits, and underscores.
  for(++J; J != E && (IsLetter(*J) || IsDigit(*J) || *J == '_'); ++J)
    ;

  llvm::StringRef Token(I, J - I);

  // Consume the token from the input stream.
  Start = Start.drop_front(Token.size());

  // Keywords are a special kind of identifiers: check if we have scanned one of
  // them an generate the corresponding representation.
  #define SIMPLE(R, C)
  #define KEYWORD(R, C)                                               \
  if(Token == R) {                                                    \
    CachedTokens.push_back(new C ## Tok(Token.begin(), Token.end())); \
    return true;                                                      \
  }
  #define TYPED(C)
  #include "acse/Lex/Tokens.def"
  #undef SIMPLE
  #undef KEYWORD
  #undef TYPED

  // The scanned token was not a keyword: an identifier had been scanned.
  CachedTokens.push_back(new IdentifierTok(Token.begin(), Token.end()));
  return true;
}

void Lexer::ReportError(ErrorTy Error) const {
  llvm::StringRef Msg;

  switch(Error) {
  #define ERROR(E, M) \
  case E: \
    Msg = M; \
    break;
  ERROR(ExpectedToken, "expected token");
  ERROR(MalformedNumber, "malformed number");
  #undef ERROR

  default:
    Msg = "scanner error";
    break;
  }

  Srcs.PrintMessage(llvm::SMLoc::getFromPointer(Start.data()),
                    llvm::SourceMgr::DK_Error,
                    Msg);
}
