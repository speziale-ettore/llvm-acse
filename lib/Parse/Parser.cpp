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

// program: var_declarations statements
bool Parser::Run() {
  llvm::OwningPtr<VarDeclarationsAST> VarDecls;
  llvm::OwningPtr<StatementsAST> Stmts;

  // Reset state before parsing.
  AST.reset();
  ErrorsFound = false;

  VarDecls.reset(ParseVarDeclarations());
  Stmts.reset(ParseStatements());

  // Check if there is something left to parse in the stream.
  ErrorsFound = ErrorsFound && Lex.Success();
 
  if(!ErrorsFound)
    AST.reset(new ProgramAST(VarDecls.take(), Stmts.take()));

  return Success();
}

// var_declarations: non_empty_var_declarations
//                 | empty
VarDeclarationsAST *Parser::ParseVarDeclarations() {
  // We have parsed a non-empty var declaration list: propagate.
  if(NonEmptyVarDeclarationsAST *VarDecls = ParseNonEmptyVarDeclarations())
    return new VarDeclarationsAST(VarDecls);

  // Nothing have been parsed, but that is allowed by the parser.
  else
    return new VarDeclarationsAST(new EmptyAST());
}

// non_empty_var_declarations: var_declaration non_empty_var_declarations
//                           | var_declaration
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

// var_declaration: *Identifier* declaration_list *SemiColon*
VarDeclarationAST *Parser::ParseVarDeclaration() {
  llvm::OwningPtr<IdentifierAST> Id;
  llvm::OwningPtr<DeclarationListAST> DeclList;
  llvm::OwningPtr<SemiColonAST> SemiColon;

  // Nothing to parse here, return immediately.
  if(!llvm::dyn_cast_or_null<IdentifierTok>(Lex.Peek(0)))
    return 0;

  // LL parsing: by checking the next token we known which parser to spawn. In
  // that case we expect an identifier, if not found we cannot parse anything.
  if(!llvm::dyn_cast_or_null<IdentifierTok>(Lex.Peek(1)))
    return 0;

  // It is now sure we are parsing a var_declarations: consume all the tokens
  // and prepare ...
  Id.reset(new IdentifierAST(Lex.TakeAs<IdentifierTok>()));

  // ... to try parsing a declaration list.
  DeclList.reset(ParseDeclarationList());

  // Failed to parse a declaration-list; we are sure this is an error in the
  // input stream -- signal it to the user.
  if(!DeclList) {
    ReportError(ExpectedDeclarationList, Id->GetEndLoc());
    return 0;
  }

  // Terminator not found: this is an error.
  if(!llvm::dyn_cast_or_null<SemiColonTok>(Lex.Peek(0))) {
    ReportError(DeclarationListMustEndWithSemiColon, DeclList->GetEndLoc());
    return 0;
  }

  // Semicolon found: consume it.
  SemiColon.reset(new SemiColonAST(Lex.TakeAs<SemiColonTok>()));

  return new VarDeclarationAST(Id.take(), DeclList.take(), SemiColon.take());
}

// declaration_list: declaration *Comma* declaration_list
//                 | declaration
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
      ReportError(ExpectedDeclaration, Comma->GetEndLoc());
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

// declaration: scalar_declaration
//            | array_declaration
DeclarationAST *Parser::ParseDeclaration() {
  // If the next token is not a '[', we must parse a scalar declaration.
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

// scalar_declaration: *Identifier*
//                   | *Identifier* *Assign* initializer
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

  // Otherwise there must be an initializer list ...
  if(InitializerAST *Init = ParseInitializer())
    return new ScalarDeclarationAST(Id.take(), Assign.take(), Init);

  // ... but there is not!
  ReportError(ExpectedInitializer, Assign->GetEndLoc());

  return 0;
}

ArrayDeclarationAST *Parser::ParseArrayDeclaration() {
  return 0;
}

// initializer: *Number*
InitializerAST *Parser::ParseInitializer() {
  InitializerAST *Init = 0;

  // Actually this parser is just a wrapper of a number. I decided to introduce
  // a custom non-terminal rule for two reasons: 1) maybe in the future I will
  // allow initialization with non-constant values, and 2) for symmetry with
  // respect to the array initialization list.
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