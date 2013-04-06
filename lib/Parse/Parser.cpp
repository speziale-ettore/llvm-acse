//===- Parser.cpp - Simple LL Parser for LANCE ------------------*- C++ -*-===//
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

  // LL parsing: by checking the current character we know which alternative should
  // be chosen. In that case, we should check for tokens defining types.
  if(llvm::dyn_cast<IntTok>(Lex.Peek(0))) {
    if(NonEmptyVarDeclarationsAST *NonEmpty = ParseNonEmptyVarDeclarations())
      VarDecls = new VarDeclarationsAST(NonEmpty);
    else
      ReportError(ExpectedVarDeclarations, Lex.GetCurrentLoc());

  // Nothing to parse, but that is allowed by the language.
  } else {
    VarDecls = new VarDeclarationsAST(new EmptyAST());
  }

  return VarDecls;
}

// non_empty_var_declarations
//   : var_declaration non_empty_var_declarations
//   | var_declaration
NonEmptyVarDeclarationsAST *Parser::ParseNonEmptyVarDeclarations() {
  llvm::SmallVector<VarDeclarationAST *, 4> Stack;

  // Iterative version of LL right recursive calls.
  while(VarDeclarationAST *VarDecl = ParseVarDeclaration())
    Stack.push_back(VarDecl);

  NonEmptyVarDeclarationsAST *VarDecls = 0;

  // We reach the innermost parser -- the last declaration in the current line.
  // Simulate returning from LL recursive calls by popping elements from the
  // stack and build the tree.
  while(!Stack.empty()) {
    VarDecls = new NonEmptyVarDeclarationsAST(Stack.back(), VarDecls);
    Stack.pop_back();
  }

  return VarDecls;
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

  // Failed to parse a declaration_list; we are sure this is an error in the
  // input stream -- signal it to the user.
  if(!DeclList) {
    ReportError(ExpectedDeclarationList, Lex.GetCurrentLoc());
    return 0;
  }

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
  llvm::SmallVector<std::pair<DeclarationAST *, CommaAST *>, 4> Stack;

  // We should parse at least one declaration.
  if(DeclarationAST *Decl = ParseDeclaration())
    Stack.push_back(std::make_pair(Decl, static_cast<CommaAST *>(0)));
  else
    return 0;

  // If current token is a comma, then we expect to parse at least another
  // declaration. Please notice that the comma is bound to the previously parsed
  // declaration, not the one we are going to parse.
  while(llvm::dyn_cast_or_null<CommaTok>(Lex.Peek(0))) {
    CommaAST *Comma = new CommaAST(Lex.TakeAs<CommaTok>());

    std::pair<DeclarationAST *, CommaAST *> &Prev = Stack.back();
    Prev.second = Comma;

    // We parsed a comma, so there must be a declaration.
    if(DeclarationAST *Decl = ParseDeclaration())
      Stack.push_back(std::make_pair(Decl, static_cast<CommaAST *>(0)));
    else
      ReportError(ExpectedDeclaration, Lex.GetCurrentLoc());
  }

  DeclarationListAST *Decls = 0;

  // We reach the innermost parser -- the last declaration. Simulate returning
  // from LL recursive calls by popping elements from the stack and build the
  // tree bottom-up.
  while(!Stack.empty()) {
    std::pair<DeclarationAST *, CommaAST *> &Cur = Stack.back();
    Decls = new DeclarationListAST(Cur.first, Cur.second, Decls);
    Stack.pop_back();
  }

  return Decls;
}

// declaration
//   : scalar_declaration
//   | array_declaration
DeclarationAST *Parser::ParseDeclaration() {
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

  // Otherwise there must be an initializer ...
  if(ScalarInitializerAST *Init = ParseScalarInitializer())
    return new ScalarDeclarationAST(Id.take(), Assign.take(), Init);

  // ... but there is not!
  ReportError(ExpectedInitializer, Lex.GetCurrentLoc());

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

  ReportError(ExpectedInitializer, Lex.GetCurrentLoc());

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
  if(InitializerListAST *List = ParseInitializerList()) {
    Init.reset(List);

  } else {
    ReportError(ExpectedInitializerList, Lex.GetCurrentLoc());
    return 0;
  }

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
  llvm::SmallVector<std::pair<InitializerAST *, CommaAST *>, 4> Stack;

  // We should parse at least one initializer.
  if(InitializerAST *Init = ParseInitializer()) {
    Stack.push_back(std::make_pair(Init, static_cast<CommaAST *>(0)));

  } else {
    ReportError(ExpectedInitializer, Lex.GetCurrentLoc());
    return 0;
  }

  // If current token is a comma, then we expect to parse at least another
  // initialization. Please notice that the comma is bound to the previously
  // parsed declaration, not the one we are going to parse.
  while(llvm::dyn_cast_or_null<CommaTok>(Lex.Peek(0))) {
    CommaAST *Comma = new CommaAST(Lex.TakeAs<CommaTok>());

    std::pair<InitializerAST *, CommaAST *> &Prev = Stack.back();
    Prev.second = Comma;

    // We parsed a comma, so there must be an initializer.
    if(InitializerAST *Init = ParseInitializer())
      Stack.push_back(std::make_pair(Init, static_cast<CommaAST *>(0)));
    else
      ReportError(ExpectedInitializer, Lex.GetCurrentLoc());
  }

  InitializerListAST *Inits = 0;

  // We reach the innermost parser -- the last initializer. Simulate returning
  // from LL recursive calls by popping elements from the stack and build the
  // tree bottom-up.
  while(!Stack.empty()) {
    std::pair<InitializerAST *, CommaAST *> &Cur = Stack.back();
    Inits = new InitializerListAST(Cur.first, Cur.second, Inits);
    Stack.pop_back();
  }

  return Inits;
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

StatementsAST *Parser::ParseStatements() {
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
