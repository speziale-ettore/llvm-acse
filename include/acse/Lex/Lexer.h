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

#include "acse/Lex/Token.h"

#include "llvm/ADT/ilist.h"
#include "llvm/Support/SourceMgr.h"

namespace acse {

class Lexer {
public:
  enum ErrorTy {
    NoError,

    ExpectedToken,
    MalformedNumber
  };

public:
  Lexer(llvm::SourceMgr &Srcs);

  Lexer(const Lexer &That) LLVM_DELETED_FUNCTION;
  const Lexer &operator=(const Lexer &That) LLVM_DELETED_FUNCTION;

public:
  void Pop() {
    assert(!CachedTokens.empty() && "No tokens to consume");

    CachedTokens.pop_front();
  }

  const Token &Current() {
    if(CachedTokens.empty())
      ScanToken();

    assert(!CachedTokens.empty() && "No tokens to read");

    return CachedTokens.front();
  }

  bool EndOfStream() {
    if(CachedTokens.empty())
      ScanToken();

    return CachedTokens.empty();
  }

  const llvm::SourceMgr &GetSources() const {
    return Srcs;
  }

private:
  void ScanToken() {
    // Keywords are modelled as special identifiers, so they are scanned
    // together with identifiers.
    if(ScanSimple() || ScanNumber() || ScanIdentifier())
      return;

    // Report error iff the end of the stream has not been reached.
    if(Start != End)
      ReportError(ExpectedToken);
  }

  bool ScanSimple();
  bool ScanNumber();
  bool ScanIdentifier();

  void ScanSpaces() {
    llvm::StringRef::size_type N = Start.find_first_not_of("\t\n\v\f\r ");

    if(N != llvm::StringRef::npos)
      Start = llvm::StringRef(Start.data() + N);
    else
      Start = End;
  }

  // We cannot use std::isalpha because it is LOCALE-sensitive, while LANCE it
  // is not -- do a custom check.
  bool IsLetter(char C) const {
    return ('a' <= C && C <= 'z') || ('A' <= C && C <= 'Z');
  }

  // The std::isdigit function should not be LOCALE-sensitive, but use a custom
  // implementation just to be sure.
  bool IsDigit(char C) const {
    return '0' <= C && C <= '9';
  }

  void ReportError(ErrorTy Error) const;

private:
  llvm::SourceMgr &Srcs;

  llvm::StringRef Start;
  llvm::StringRef End;

  llvm::ilist<Token> CachedTokens;
};

} // End namespace acse.

#endif // ACSE_LEX_LEXER_H
