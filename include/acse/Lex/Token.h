//===- Token.h - LANCE Token Definitions ------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef ACSE_LEX_TOKEN_H
#define ACSE_LEX_TOKEN_H

#include "llvm/ADT/ilist_node.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SMLoc.h"

namespace acse {

class Token : public llvm::ilist_node<Token> {
public:
  enum Id {
    #define SIMPLE(R, C) \
    C,
    #define KEYWORD(R, C) \
    C,
    #define TYPED(C) \
    C,
    #include "acse/Lex/Tokens.def"
    #undef SIMPLE
    #undef KEYWORD
    #undef TYPED

    // Special tokens.
    Invalid,
    Count
  };

public:
  Token() : MyIdentifier(Invalid) { }

protected:
  Token(Id Identifier, llvm::StringRef Start, llvm::StringRef End)
    : MyIdentifier(Identifier),
      Range(llvm::SMLoc::getFromPointer(Start.data()),
            llvm::SMLoc::getFromPointer(End.data())) { }

public:
  Id GetId() const {
    return MyIdentifier;
  }

  llvm::SMLoc GetLocation() const {
    return Range.Start;
  }

  llvm::StringRef GetSpelling() const {
    llvm::SMLoc Start = Range.Start,
                End = Range.End;

    return llvm::StringRef(Start.getPointer(),
                           End.getPointer() - Start.getPointer());
  }

private:
  Id MyIdentifier;
  llvm::SMRange Range;
};

llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, Token::Id Identifier);

class SimpleTok : public Token {
protected:
  SimpleTok(Token::Id Identifier, llvm::StringRef Start, llvm::StringRef End)
    : Token(Identifier, Start, End) { }
};

class KeywordTok : public Token {
protected:
  KeywordTok(Token::Id Identifier, llvm::StringRef Start, llvm::StringRef End)
    : Token(Identifier, Start, End) { }
};

#define SIMPLE(R, C)                                   \
class C ## Tok : public SimpleTok {                    \
public:                                                \
  C ## Tok(llvm::StringRef Start, llvm::StringRef End) \
    : SimpleTok(Token::C, Start, End) { }              \
};
#define KEYWORD(R, C)                                  \
class C ## Tok : public KeywordTok {                   \
public:                                                \
  C ## Tok(llvm::StringRef Start, llvm::StringRef End) \
    : KeywordTok(Token::C, Start, End) { }             \
};
#define TYPED(C)
#include "acse/Lex/Tokens.def"
#undef SIMPLE
#undef KEYWORD
#undef TYPED

class NumberTok : public Token {
public:
  typedef uint32_t NumberTy;

public:
  static const unsigned Radix = 10;

public:
  NumberTok(llvm::StringRef Start, llvm::StringRef End, NumberTy N)
    : Token(Number, Start, End),
      N(N) { }

public:
  NumberTy Value() const { return N; }

private:
  NumberTy N;
};

class IdentifierTok : public Token {
public:
  IdentifierTok(llvm::StringRef Start, llvm::StringRef End)
    : Token(Identifier, Start, End) { }
};

} // End namespace acse.

#endif // ACSE_LEX_TOKEN_H
