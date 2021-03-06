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
#include "llvm/Support/Casting.h"
#include "llvm/Support/SourceMgr.h"

namespace acse {

// The Lexer is the basic building block of every compiler front-end. Its duty
// is to identify tokens in the input stream. Sometimes, it is said that "the
// stream is tokenized by the lexer".
//
// Every lexer implements the following operations in order to access to the
// token stream:
//
// - get the current token: this allows a client to understand the current
//   position in the token stream
// - consume current token: this allows a client to proceed analyzing the
//   stream; this operation is usually called "pop".
// - get the n-th next token: this allows a client observing which tokens will
//   follow the current one, and thus deciding which actions to do next; this
//   operation is usually called "peek"
//
// All other operations are needed to correctly manage the token stream, but the
// core operations are the one reported above!
class Lexer {
public:
  enum ErrorTy {
  #define ERROR(E, M) \
    E,
  #include "acse/Lex/LexerError.def"
  #undef ERROR

    ErrorCount
  };

public:
  Lexer(llvm::SourceMgr &Srcs);

private:
  Lexer(const Lexer &That) LLVM_DELETED_FUNCTION;
  const Lexer &operator=(const Lexer &That) LLVM_DELETED_FUNCTION;

public:
  void SetEatComments(bool EatComments = true) {
    this->EatComments = EatComments;
  }

  bool EatsComments() const {
    return this->EatComments;
  }

public:
  // Pop removes consumes the current token, destroying it.
  void Pop() {
    delete Take();
  }

  // Take acts almost exactly as Pop, but the current token is not destroyed
  // after removal from the scanning queue. Indeed, its ownership is transferred
  // to the caller.
  Token *Take() {
    assert(!CachedTokens.empty() && "No tokens to consume");

    return CachedTokens.remove(CachedTokens.begin());
  }

  // Template version of take -- just software engineering.
  template <typename Ty>
  Ty *TakeAs() { return llvm::cast<Ty>(Take()); }

  // Getting the current token is the most common operations: it must be FTL.
  const Token &Current() {
    const Token *Cur = Peek(0);

    assert(Cur && "No tokens to read");

    return *Cur;
  }

  // Default behaviour of stream peak is usually to return the token following
  // the current one, thus since the current token is at position '0', the token
  // to peek is at position '1'.
  //
  // The scanner employs an 'llvm::ilist' which is optimized for sequential
  // access, not for random access. This is not a performance penalty because
  // most of the time, the number of peek operations is lesser than the number
  // of other operations. Moreover, when we are going to perform a peek, we
  // usually look at the character following the current one.
  const Token *Peek(unsigned N = 1) {
    // The cache contains enough elements to fulfill the request.
    if(N < CachedTokens.size())
      return CachedTokens.begin();

    unsigned I, E;

    // Otherwise, try reading the needed tokens ...
    for(I = CachedTokens.size(), E = N + 1; I != E && ScanToken(); ++I)
      ;

    // ... but some tokens are missing!
    if(I != E)
      return 0;

    llvm::ilist<Token>::const_iterator J = CachedTokens.begin();

    // Now, since the llvm::ilist iterator is not a random access iterator, we
    // must advance it using an explicit loop.
    for(I = 0, E = N; I != E; ++I, ++J)
      ;

    return J;
  }

  bool EndOfStream() {
    if(CachedTokens.empty())
      ScanToken();

    return CachedTokens.empty();
  }

  bool Success() const {
    return Start == End && CachedTokens.empty();
  }

  const llvm::SourceMgr &GetSources() const {
    return Srcs;
  }

  llvm::SMLoc GetCurrentLoc() const {
    // No cached tokens. Location must be build from raw pointer.
    if(CachedTokens.empty())
      return llvm::SMLoc::getFromPointer(Start.data());

    // Otherwise, there is at least a token in the cache.
    const Token *Cur = CachedTokens.begin();

    // Current location is the token start location.
    return Cur->GetLocation();
  }

private:
  bool ScanToken() {
    // Try scan a comment. If it should not be ignored, then it acts like an
    // ordinary token, hence we have scanned something.
    if(ScanComment() && !EatComments)
      return true;

    // Try eating the next comments, if requested.
    while(EatComments && ScanComment())
      ;

    // Now, we can start scanning. Keywords are modelled as special identifiers,
    // so they are scanned together with identifiers.
    if(ScanSimple() || ScanNumber() || ScanIdentifier())
      return true;

    // Report error iff the end of the stream has not been reached.
    if(Start != End)
      ReportError(ExpectedToken);

    return false;
  }

  bool ScanComment();
  bool ScanSimple();
  bool ScanNumber();
  bool ScanIdentifier();

  void ScanSpaces() {
    llvm::StringRef::size_type N = Start.find_first_not_of("\t\n\v\f\r ");

    if(N != llvm::StringRef::npos)
      Start = Start.drop_front(N);
    else
      Start = End;
  }

  bool IsNewLine(char C) const {
    switch(C) {
    case '\n': // Line feed.
    case '\v': // Vertical tab.
    case '\f': // Form feed.
      return true;

    default:
      return false;
    }

    llvm_unreachable("Not reachable");
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

  // Control whether the scanner should not consider comment tokens.
  bool EatComments;
};

} // End namespace acse.

#endif // ACSE_LEX_LEXER_H
