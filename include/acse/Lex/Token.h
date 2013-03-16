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
  // For each token kind there is a corresponding subclass of Token. The
  // convention is that given a token kind A, the corresponding class is named
  // ATok.
  enum Id {
    // Comments.
    //
    // A C99/C++ style comment. All text starting from '//' until the end of the
    // line is considered a comment, and represented using a single token.
    LineComment,
    // A C/C++ style multi-line comment. All text surrounded by '/*' and '*/' is
    // considered a comment. The comment can span multiple lines, and it is
    // represented using a single token.
    MultiLineComment,

    // Braces and co.
    LBrace,  // '{'
    RBrace,  // '}'
    LSquare, // '['
    RSquare, // ']'
    LPar,    // '('
    RPar,    // ')'

    // Separators/terminators.
    SemiColon, // ';'
    Colon,     // ':'
    Comma,     // ','
    Assign,    // '='

    // Algebraic operators.
    Add, // '+'
    Sub, // '-'
    Mul, // '*'
    Div, // '/'
    Mod, // '%'

    // Relational operators.
    Less,           // '<'
    LessOrEqual,    // '<='
    Equal,          // '=='
    NotEqual,       // '!='
    GreaterOrEqual, // '>='
    Greater,        // '>'

    // Bitwise operators.
    BAnd,   // '&'
    BOr,    // '|'
    BNot,   // '!'
    LShift, // '<<'
    RShift, // '>>'

    // Logical operators.
    LAnd, // '&&'
    LOr,  // '||'

    // Keywords.
    If,    // 'if'
    Else,  // 'else'
    Do,    // 'do'
    While, // 'while'
    Read,  // 'read'
    Write, // 'write'

    // Typed.
    //
    // A number is a list of digits. The list must be non empty and the first
    // digit should be different than '0': [1-9][0-9]*. The number is actually
    // interpreted as a 32-bit unsigned integer expressed in base-10.
    //
    // Please notice that negative numbers are not allowed. For instance '-42'
    // actually consists of two tokens: Sub and Number.
    Number,
    // An identifier is a non-empty lists of alphanumeric characters and '_'.
    // The first character cannot be a digit: [a-zA-Z_][a-zA-Z_0-9]*.
    Identifier,

    // Special.
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
TOKEN(If)
TOKEN(Else)
TOKEN(Do)
TOKEN(While)
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
