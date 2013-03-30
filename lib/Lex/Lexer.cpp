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

Lexer::Lexer(llvm::SourceMgr &Srcs) : Srcs(Srcs),
                                      EatComments(false) {
  assert(Srcs.getNumBuffers() == 1 && "ACSE scanner works with only one file");

  const llvm::MemoryBuffer *Src = Srcs.getMemoryBuffer(0);

  Start = Src->getBufferStart();
  End = Src->getBufferEnd();
}

bool Lexer::ScanComment() {
  ScanSpaces();

  llvm::StringRef::iterator I = Start.begin(),
                            J = I,
                            E = Start.end();

  // First char is not a '/'.
  if(J == E || *J != '/')
    return false;

  // There is not a second char.
  if(++J == E)
    return false;

  // Second char is a '/': the comment spans only one line.
  if(*J == '/') {
    bool InsideComment = true;

    for(++J; J != E && InsideComment; ++J)
      InsideComment = !IsNewLine(*J);

    // Actually build the token only if comments should not be filtered.
    if(!EatComments)
      CachedTokens.push_back(new LineCommentTok(I, J));
    Start = Start.drop_front(J - I);

    return true;

  // Second char is a '*': the comment spans multiple lines.
  } else if(*J == '*') {
    bool InsideComment = true;

    for(++J; J != E && (J + 1) != E && InsideComment; ++J)
      InsideComment = *J != '*' || *(J + 1) != '/';

    if(InsideComment) {
      ReportError(UnterminatedMultiLineComment);
      return false;
    }

    // Add '1' because in the case of match, at the loop exit J points to '/'.
    ++J;

    // Actually build the token only if comments should not be filtered.
    if(!EatComments)
      CachedTokens.push_back(new MultiLineCommentTok(I, J));
    Start = Start.drop_front(J - I);

    return true;
  }

  return false;
}

bool Lexer::ScanSimple() {
  ScanSpaces();

  llvm::StringRef::iterator I = Start.begin(),
                            J = I,
                            E = Start.end();

  // Nothing to scan.
  if(J == E)
    return false;

  switch(*J) {
  // Braces and co.
  case '{':
    CachedTokens.push_back(new LBraceTok(I, ++J));
    break;
  case '}':
    CachedTokens.push_back(new RBraceTok(I, ++J));
    break;
  case '[':
    CachedTokens.push_back(new LSquareTok(I, ++J));
    break;
  case ']':
    CachedTokens.push_back(new RSquareTok(I, ++J));
    break;
  case '(':
    CachedTokens.push_back(new LParTok(I, ++J));
    break;
  case ')':
    CachedTokens.push_back(new RParTok(I, ++J));
    break;

  // Separators/terminators.
  case ';':
    CachedTokens.push_back(new SemiColonTok(I, ++J));
    break;
  case ':':
    CachedTokens.push_back(new ColonTok(I, ++J));
    break;
  case ',':
    CachedTokens.push_back(new CommaTok(I, ++J));
    break;
  // Check next char in order to check whether the token is Assign or Equal.
  case '=':
    if(++J == E || *J != '=')
      CachedTokens.push_back(new AssignTok(I, J));
    else // if(*J == '=' )
      CachedTokens.push_back(new EqualTok(I, ++J));
    break;

  // Algebraic operators.
  case '+':
    CachedTokens.push_back(new AddTok(I, ++J));
    break;
  case '-':
    CachedTokens.push_back(new SubTok(I, ++J));
    break;
  case '*':
    CachedTokens.push_back(new MulTok(I, ++J));
    break;
  // Since the comment scanner is invoked before this scanner, there is no for
  // the current character to be the prefix of a comment token.
  case '/':
    CachedTokens.push_back(new DivTok(I, ++J));
    break;
  case '%':
    CachedTokens.push_back(new ModTok(I, ++J));
    break;

  // Relation operators.
  //
  // Could be the prefix of LessOrEqual or LShift.
  case '<':
    if(++J == E || (*J != '=' && *J != '<'))
      CachedTokens.push_back(new LessTok(I, J));
    else if(*J == '=')
      CachedTokens.push_back(new LessOrEqualTok(I, ++J));
    else // if(*J == '<')
      CachedTokens.push_back(new LShiftTok(I, ++J));
    break;
  // Equal has been already handled.
  //
  // Could be a standalone bitwise not.
  case '!':
    if(++J == E || *J != '=')
      CachedTokens.push_back(new BNotTok(I, J));
    else // if(*J == '=')
      CachedTokens.push_back(new NotEqualTok(I, ++J));
    break;
  // Could be the prefix of GreaterOrEqual or RShift.
  case '>':
    if(++J == E || (*J != '=' && *J != '>'))
      CachedTokens.push_back(new GreaterTok(I, J));
    else if(*J == '=')
      CachedTokens.push_back(new GreaterOrEqualTok(I, ++J));
    else // if(*J == '>')
      CachedTokens.push_back(new RShiftTok(I, ++J));
    break;

  // Bitwise operators.
  //
  // Could be the prefix of LAnd.
  case '&':
    if(++J == E || *J != '&')
      CachedTokens.push_back(new BAndTok(I, J));
    else // if(*J == '&')
      CachedTokens.push_back(new LAndTok(I, ++J));
    break;
  // Could be the prefix of LOr.
  case '|':
    if(++J == E || *J != '|')
      CachedTokens.push_back(new BOrTok(I, J));
    else // if(*J == '|')
      CachedTokens.push_back(new LOrTok(I, ++J));
    break;
  // BNot already scanned.
  // LShift already scanned.
  // RShift already scanned.

  // Logical operators.
  //
  // LAnd already scanned.
  // LOR already scanned.

  // Nothing to scan here.
  default:
    return false;
  }

  Start = Start.drop_front(J - I);

  return true;
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
  #define TOKEN(R, C)                                               \
  if(Token == R) {                                                    \
    CachedTokens.push_back(new C ## Tok(Token.begin(), Token.end())); \
    return true;                                                      \
  }

  TOKEN("int", Int)
  TOKEN("if", If)
  TOKEN("else", Else)
  TOKEN("do", Do)
  TOKEN("while", While)
  TOKEN("read", Read)
  TOKEN("write", Write)

  #undef TOKEN

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
  #include "acse/Lex/LexerError.def"
  #undef ERROR

  default:
    Msg = "scanner error";
    break;
  }

  Srcs.PrintMessage(llvm::SMLoc::getFromPointer(Start.data()),
                    llvm::SourceMgr::DK_Error,
                    Msg);
}
