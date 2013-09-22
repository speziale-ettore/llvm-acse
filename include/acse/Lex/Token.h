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
    // For each token kind there is a corresponding subclass of Token. The
    // convention is that given a token kind A, the corresponding class is named
    // ATok.
    #define TOKEN(I) \
    I,
    #include "acse/Lex/Token.def"
    #undef TOKEN

    // This enum value represents the number of tokens accepted by ACSE.
    Count,

    // This value is needed to identify an invalid token. An invalid token is
    // not used by ACSE during parsing. It is only used when the default
    // constructor of Token is called. In this case, we cannot give a token
    // identifier, hence the built token is unknown.
    //
    // Default constructor is only needed because we are going to put Token
    // instances inside a collection -- it requires the element type having a
    // default constructor.
    Invalid
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

class CommentTok : public Token {
protected:
  CommentTok(Id Identifier, llvm::StringRef Start, llvm::StringRef End)
    : Token(Identifier, Start, End) { }
};

class KeywordTok : public Token {
protected:
  KeywordTok(Id Identifier, llvm::StringRef Start, llvm::StringRef End)
    : Token(Identifier, Start, End) { }
};

// Most tokens are very trivial. Use a macro to automatically generate a class
// for each of them.
#define TOKEN_CLASS(I, S)                              \
class I ## Tok : public S {                            \
public:                                                \
  static inline bool classof(const Token *Tok) {       \
    return Tok->GetId() == I;                          \
  }                                                    \
                                                       \
public:                                                \
  I ## Tok(llvm::StringRef Start, llvm::StringRef End) \
    : S(I, Start, End) { }                             \
};

#define TOKEN(I) TOKEN_CLASS(I, CommentTok)

// Comments.
TOKEN(LineComment)
TOKEN(MultiLineComment)

#undef TOKEN
#define TOKEN(I) TOKEN_CLASS(I, Token)

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

#undef TOKEN
#define TOKEN(I) TOKEN_CLASS(I, KeywordTok)

// Keywords.
TOKEN(Int)
TOKEN(If)
TOKEN(Else)
TOKEN(Do)
TOKEN(While)
TOKEN(Return)
TOKEN(Read)
TOKEN(Write)

#undef TOKEN
#undef TOKEN_CLASS

class NumberTok : public Token {
public:
  typedef uint32_t NumberTy;

public:
  static inline bool classof(const Token *Tok) {
    return Tok->GetId() == Number;
  }

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
  static inline bool classof(const Token *Tok) {
    return Tok->GetId() == Identifier;
  }

public:
  IdentifierTok(llvm::StringRef Start, llvm::StringRef End)
    : Token(Identifier, Start, End) { }
};

} // End namespace acse.

#endif // ACSE_LEX_TOKEN_H
