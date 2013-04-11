//===- Parser.cpp - Simple Descendent Parser for LANCE ----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "acse/Parse/Parser.h"

using namespace acse;

// program
//   : var_declarations statements
bool Parser::Run() {
  llvm::OwningPtr<VarDeclarationsAST> VarDecls;
  llvm::OwningPtr<StatementsAST> Stmts;

  // Reset state before parsing.
  AST.reset();
  ErrorsFound = false;

  VarDecls.reset(ParseVarDeclarations());
  Stmts.reset(ParseStatements());

  // Check if there is something left to parse in the stream.
  ErrorsFound = ErrorsFound || !Lex.Success();
 
  if(!ErrorsFound) {
    ProgramAST *Prog = new ProgramAST(VarDecls.take(), Stmts.take());
    AST.reset(new AbstractSyntaxTree(Prog));
  }

  return Success();
}

// var_declarations
//   : non_empty_var_declarations
//   | empty
VarDeclarationsAST *Parser::ParseVarDeclarations() {
  VarDeclarationsAST *VarDecls = 0;

  // Tentative parsing: try parsing the first alternative -- i.e. the most
  // common. On failure, fall-back to the second -- less common case.
  if(NonEmptyVarDeclarationsAST *NonEmpty = ParseNonEmptyVarDeclarations())
    VarDecls = new VarDeclarationsAST(NonEmpty);
  else
    VarDecls = new VarDeclarationsAST(new EmptyAST());

  return VarDecls;
}

// non_empty_var_declarations
//   : var_declaration non_empty_var_declarations
//   | var_declaration
NonEmptyVarDeclarationsAST *Parser::ParseNonEmptyVarDeclarations() {
  typedef NonEmptyVarDeclarationsAST List;
  typedef VarDeclarationAST Node;

  return ParseList<List, Node, &Parser::ParseVarDeclaration>();
}

// var_declaration
//   : type declaration_list *SemiColon*
VarDeclarationAST *Parser::ParseVarDeclaration() {
  llvm::OwningPtr<TypeAST> Type;
  llvm::OwningPtr<DeclarationListAST> DeclList;
  llvm::OwningPtr<SemiColonAST> SemiColon;

  // Try parsing a type.
  Type.reset(ParseType());

  // Failed parsing a type. We are not parsing a var_declaration.
  if(!Type)
    return 0;

  // It is now sure we are parsing a var_declaration: try parsing a
  // declaration_list.
  DeclList.reset(ParseDeclarationList());

  // Failed to parse a declaration_list. This is an error in the input stream.
  if(!DeclList)
    return 0;

  // Terminator not found: this is an error.
  if(!llvm::dyn_cast_or_null<SemiColonTok>(Lex.Peek(0))) {
    ReportError(DeclarationListMustEndWithSemiColon, Lex.GetCurrentLoc());
    return 0;
  }

  // Semicolon found: consume it.
  SemiColon.reset(new SemiColonAST(Lex.TakeAs<SemiColonTok>()));

  return new VarDeclarationAST(Type.take(), DeclList.take(), SemiColon.take());
}

// declaration_list
//   : declaration *Comma* declaration_list
//   | declaration
DeclarationListAST *Parser::ParseDeclarationList() {
  typedef DeclarationListAST List;
  typedef DeclarationAST Node;
  typedef CommaAST Sep;

  return ParseList<List, Node, Sep, &Parser::ParseDeclaration>();
}

// declaration
//   : scalar_declaration
//   | array_declaration
DeclarationAST *Parser::ParseDeclaration() {
  // Tentative parsing here is not enough, because the tokens forming a
  // scalar declaration are actually the prefix of an array declaration. In
  // order to be greedy, we must look ahead and decide here which kind of
  // declaration we are going to parse.
  //
  // If the next-next token is not a '[', we must parse a scalar declaration.
  if(!llvm::dyn_cast_or_null<LSquareTok>(Lex.Peek(1))) {
    if(ScalarDeclarationAST *Decl = ParseScalarDeclaration())
      return new DeclarationAST(Decl);

  // Otherwise, it must be an array declaration.
  } else {
    if(ArrayDeclarationAST *Decl = ParseArrayDeclaration())
      return new DeclarationAST(Decl);
  }

  // Parsing failed.
  return 0;
}

// scalar_declaration
//   : *Identifier*
//   | *Identifier* *Assign* scalar_initializer
ScalarDeclarationAST *Parser::ParseScalarDeclaration() {
  llvm::OwningPtr<IdentifierAST> Id;
  llvm::OwningPtr<AssignAST> Assign;

  // First token must be an identifier.
  if(!llvm::dyn_cast_or_null<IdentifierTok>(Lex.Peek(0)))
    return 0;

  Id.reset(new IdentifierAST(Lex.TakeAs<IdentifierTok>()));

  // If current token is not an assignment, then the declaration ends here.
  if(!llvm::dyn_cast_or_null<AssignTok>(Lex.Peek(0)))
    return new ScalarDeclarationAST(Id.take());

  Assign.reset(new AssignAST(Lex.TakeAs<AssignTok>()));

  // Otherwise there must be an initializer.
  if(ScalarInitializerAST *Init = ParseScalarInitializer())
    return new ScalarDeclarationAST(Id.take(), Assign.take(), Init);

  return 0;
}

// array_declaration
//   : *Identifier* *LSquare* *Number* *RSquare*
//   | *Identifier* *LSquare* *Number* *RSquare* *Assign* array_initializer
//   | *Identifier* *LSquare* *RSquare* *Assign* array_initializer
ArrayDeclarationAST *Parser::ParseArrayDeclaration() {
  llvm::OwningPtr<IdentifierAST> Id;
  llvm::OwningPtr<LSquareAST> OpenSquare;
  llvm::OwningPtr<NumberAST> Size;
  llvm::OwningPtr<RSquareAST> ClosedSquare;
  llvm::OwningPtr<AssignAST> Assign;

  // First token must be an identifier.
  if(!llvm::dyn_cast_or_null<IdentifierTok>(Lex.Peek(0))) {
    ReportError(ExpectedIdentifier, Lex.GetCurrentLoc());
    return 0;
  }

  Id.reset(new IdentifierAST(Lex.TakeAs<IdentifierTok>()));

  // Then there should be the first bracket.
  if(!llvm::dyn_cast_or_null<LSquareTok>(Lex.Peek(0))) {
    ReportError(ExpectedLSquare, Lex.GetCurrentLoc());
    return 0;
  }

  OpenSquare.reset(new LSquareAST(Lex.TakeAs<LSquareTok>()));

  // The array size is explicitly specified.
  if(llvm::dyn_cast_or_null<NumberTok>(Lex.Peek(0)))
    Size.reset(new NumberAST(Lex.TakeAs<NumberTok>()));

  // Now we expect a closed bracket.
  if(!llvm::dyn_cast_or_null<RSquareTok>(Lex.Peek(0))) {
    ReportError(ExpectedRSquare, Lex.GetCurrentLoc());
    return 0;
  }

  ClosedSquare.reset(new RSquareAST(Lex.TakeAs<RSquareTok>()));

  // Assignment token is not found: explicit initializer is missing. Please
  // notice that the size of the array must be specified, otherwise we cannot
  // compute at compile-time the size of the array.
  //
  // Deferring the check now allows to emit a more meaningful error message.
  // Indeed, an automatically generated parser will detect the missing
  // initialized instead of the missing size, so the error message can be a
  // little-bit misleading.
  if(!llvm::dyn_cast_or_null<AssignTok>(Lex.Peek(0))) {
    if(!Size) {
      ReportError(ExpectedArraySize, OpenSquare->GetStartLoc());
      return 0;
    }

    return new ArrayDeclarationAST(Id.take(),
                                   OpenSquare.take(),
                                   Size.take(),
                                   ClosedSquare.take());
  }

  Assign.reset(new AssignAST(Lex.TakeAs<AssignTok>()));

  // Initializer found. In real compiler, the length of the initializer list
  // must be compared with the size of the array. However, it is a semantic
  // action, and this is a teaching compiler, so I preferred to do that later
  // in order to keep syntactic and semantic analysis clearly separated.
  if(ArrayInitializerAST *Init = ParseArrayInitializer()) {
    if(Size)
      return new ArrayDeclarationAST(Id.take(),
                                     OpenSquare.take(),
                                     Size.take(),
                                     ClosedSquare.take(),
                                     Assign.take(),
                                     Init);
    else
      return new ArrayDeclarationAST(Id.take(),
                                     OpenSquare.take(),
                                     ClosedSquare.take(),
                                     Assign.take(),
                                     Init);
  }

  return 0;
}

// type
//   : *Int*
TypeAST *Parser::ParseType() {
  TypeAST *Type = 0;

  if(llvm::dyn_cast_or_null<IntTok>(Lex.Peek(0)))
    Type = new TypeAST(new IntAST(Lex.TakeAs<IntTok>()));

  return Type;
}

// array_initializer
//   : *LBrace* initializer_list *RBrace*
ArrayInitializerAST *Parser::ParseArrayInitializer() {
  llvm::OwningPtr<LBraceAST> OpenBrace;
  llvm::OwningPtr<InitializerListAST> Init;
  llvm::OwningPtr<RBraceAST> ClosedBrace;

  // First token must be an open brace ...
  if(!llvm::dyn_cast_or_null<LBraceTok>(Lex.Peek(0))) {
    ReportError(ExpectedLBrace, Lex.GetCurrentLoc());
    return 0;
  }

  OpenBrace.reset(new LBraceAST(Lex.TakeAs<LBraceTok>()));

  // ... followed by a list of initializers ...
  if(InitializerListAST *List = ParseInitializerList())
    Init.reset(List);
  else
    return 0;

  // ... and, at last, a closed brace ends.
  if(!llvm::dyn_cast_or_null<RBraceTok>(Lex.Peek(0))) {
    ReportError(ExpectedRBrace, Lex.GetCurrentLoc());
    return 0;
  }

  ClosedBrace.reset(new RBraceAST(Lex.TakeAs<RBraceTok>()));

  return new ArrayInitializerAST(OpenBrace.take(),
                                 Init.take(),
                                 ClosedBrace.take());
}

// scalar_initializer
//   : initializer
ScalarInitializerAST *Parser::ParseScalarInitializer() {
  ScalarInitializerAST *Init = 0;

  // Just for symmetry with respect to the array initializer.
  if(InitializerAST *Init = ParseInitializer())
    return new ScalarInitializerAST(Init);

  return Init;
}

// initializer_list
//   : initializer *Comma* initializer_list
//   | initializer
InitializerListAST *Parser::ParseInitializerList() {
  typedef InitializerListAST List;
  typedef InitializerAST Node;
  typedef CommaAST Sep;

  return ParseList<List, Node, Sep, &Parser::ParseInitializer>();
}

// initializer
//   : *Number*
InitializerAST *Parser::ParseInitializer() {
  InitializerAST *Init = 0;

  // Actually this parser is just a wrapper of a number. I decided to introduce
  // a custom non-terminal rule because maybe in the future I will allow
  // initialization with non-constant values.
  if(llvm::dyn_cast_or_null<NumberTok>(Lex.Peek(0)))
    Init = new InitializerAST(new NumberAST(Lex.TakeAs<NumberTok>()));

  return Init;
}

// statements
//   : non_empty_statements
//   | empty
StatementsAST *Parser::ParseStatements() {
  StatementsAST *Stmts = 0;

  // Try parsing some statements.
  if(NonEmptyStatementsAST *NonEmpty = ParseNonEmptyStatements())
    Stmts = new StatementsAST(NonEmpty);

  // No statements to parse.
  else
    Stmts = new StatementsAST(new EmptyAST());

  return Stmts;
}

// non_empty_statements
//   : statement non_empty_statements
//   | statement
NonEmptyStatementsAST *Parser::ParseNonEmptyStatements() {
  typedef NonEmptyStatementsAST List;
  typedef StatementAST Node;

  return ParseList<List, Node, &Parser::ParseStatement>();
}

// statement
//   : assign_statement *SemiColon*
//   | read_write_statement *SemiColon*
//   | null_statement *SemiColon*
//   | control_statement
StatementAST *Parser::ParseStatement() {
  const Token *CurTok = Lex.Peek(0);

  // No token available, hence nothing to parse.
  if(!CurTok)
    return 0;

  StatementAST *Stmt = 0;

  // The actual parser to spawn must be chosen between a lot of alternatives.
  // Instead of trying all of them, it is better to look ahead a select the
  // right parser to spawn.
  //
  // Automatically generated parser usually performs a lot of ahead lookup. We
  // do not, because this will make the code unreadable, which is not a
  // desirable property for an hand-written parsers.
  //
  // Here, in order to keep the code as clean as possible, I checked the token
  // id instead of checking its class during the look ahead.
  switch(CurTok->GetId()) {

  // The left hand side of an assignment is always an identifier. Indeed, even
  // in the case of array assignment, the first token is the name of the array,
  // that is an identifier.
  case Token::Identifier: {
    llvm::OwningPtr<AssignStatementAST> Assign(ParseAssignStatement());

    if(Assign && llvm::dyn_cast_or_null<SemiColonTok>(Lex.Peek(0)))
      Stmt = new StatementAST(Assign.take(),
                              new SemiColonAST(Lex.TakeAs<SemiColonTok>()));

    break;
  }

  // Read and write statements are identified by specific keywords, hence it is
  // trivial predicting if one of them is expected.
  case Token::Read:
  case Token::Write: {
    llvm::OwningPtr<ReadWriteStatementAST> ReadWrite(ParseReadWriteStatement());

    if(ReadWrite && llvm::dyn_cast_or_null<SemiColonTok>(Lex.Peek(0)))
      Stmt = new StatementAST(ReadWrite.take(),
                              new SemiColonAST(Lex.TakeAs<SemiColonTok>()));

    break;
  }

  // A null statement is represented by nothing, hence we must check for its
  // following token -- the semicolon -- in order to detect whether we are going
  // to parse it.
  case Token::SemiColon:
    if(NullStatementAST *Null = ParseNullStatement())
      Stmt = new StatementAST(Null,
                              new SemiColonAST(Lex.TakeAs<SemiColonTok>()));
    break;

  // Control statements always start with a keyword: check is trivial.
  case Token::If:
  case Token::Do:
  case Token::While:
    if(ControlStatementAST *Control = ParseControlStatement())
      Stmt = new StatementAST(Control);
    break;

  // Current token does not allow to make a choice about which parser to spawn.
  default:
    break;
  }

  return Stmt;
}

AssignStatementAST *Parser::ParseAssignStatement() {
  return 0;
}

ReadWriteStatementAST *Parser::ParseReadWriteStatement() {
  return 0;
}

// null_statement
//   : empty
NullStatementAST *Parser::ParseNullStatement() {
  // A null statement simply expands to 0 tokens. I preferred introducing a
  // custom non-terminal rule to point out that this is a null statement.
  return new NullStatementAST(new EmptyAST());
}

ControlStatementAST *Parser::ParseControlStatement() {
  return 0;
}

void Parser::ReportError(ErrorTy Error, llvm::SMLoc Loc) {
  llvm::StringRef Msg;

  switch(Error) {
  #define ERROR(E, M) \
  case E: \
    Msg = M; \
    break;
  #include "acse/Parse/ParserError.def"
  #undef ERROR

  default:
    Msg = "parser error";
    break;
  }

  // Print the error message.
  const llvm::SourceMgr &Srcs = Lex.GetSources();
  Srcs.PrintMessage(Loc, llvm::SourceMgr::DK_Error, Msg);

  // Remember that we incur into an error, so at the end of the parsing process
  // we can detected whether we built a valid AST.
  ErrorsFound = true;
}
