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

namespace acse {

// Specialize FrameTraits for framers used in expressions parsing.
template <>
struct FrameTraits<ParsingFrame<Token *, ExpressionAST *> > {
  static void Dispose(ParsingFrame<Token *, ExpressionAST *> &Frame) {
    if(Token *Tok = Frame.GetFirst())
      delete Tok;

    if(ExpressionAST *Expr = Frame.GetSecond())
      delete Expr;
  }
};

// Specialize FramePrintTraits for frames used in expressions parsing.
template <>
struct FramePrintTraits<ParsingFrame<Token *, ExpressionAST *> > {
  static
  unsigned GetWidth(const ParsingFrame<Token *, ExpressionAST *> &Frame) {
    std::string TokLine, ExprLine;
    llvm::raw_string_ostream TS(TokLine), ES(ExprLine);

    Token *Tok = Frame.GetFirst();
    ExpressionAST *Expr = Frame.GetSecond();

    if(Tok) {
      TS << Tok->GetId();
      TS.flush();
    }

    if(Expr) {
      ES << Expr->GetId();
      ES.flush();
    }

    return std::max(TokLine.size(), ExprLine.size());
  }

  static
  std::string GetText(const ParsingFrame<Token *, ExpressionAST *> &Frame) {
    std::string TokLine, ExprLine, Text;
    llvm::raw_string_ostream OS(Text);

    Token *Tok = Frame.GetFirst();
    ExpressionAST *Expr = Frame.GetSecond();

    // Since the stack is printed with the top frame in the first line, it is
    // better emitting frame text in the reverse order. In that way, the
    // expression right operand is followed by the operator, which is then
    // followed by the expression left operand -- stored in the previous frame.
    if(Expr) {
      OS << Expr->GetId() << "\n";
    }
    if(Tok) {
      OS << Tok->GetId() << "\n";
    }

    return OS.str();
  }
};

} // End acse namespace.

namespace {

// Utility function to create a TokenAST representing an operator for a given
// Token.
template <typename OperTy>
OperTy *MakeOperatorAST(Token *Oper) {
  typedef typename OperTy::Token OperTokTy;

  return new OperTy(llvm::cast<OperTokTy>(Oper));
}

// Utility function to create an AST for a binary expression starting from the
// operand ASTs and the operator token.
template <typename ExprTy, typename OperTy>
ExpressionAST *MakeExpressionAST(ExpressionAST *LHS,
                                 Token *Oper,
                                 ExpressionAST *RHS)
{
  return new ExprTy(LHS, MakeOperatorAST<OperTy>(Oper), RHS);
}

} // End anonymous namespace.

PrecedenceTable::Table *PrecedenceTable::Table::Instance = 0;

// Fill the precedence table. Please notice that all precedences are > 0 since
// precedence 0 is a special value used by the parsing algorithm to detect the
// root expression.
void PrecedenceTable::Table::Fill() {
  std::memset(Data, Table::MinPrecedence, sizeof(uint8_t) * Token::Count);

  uint8_t CurPrecedence = Table::MinPrecedence;

  #define NEXT_PRECEDENCE(O)        \
  Data[Token::O] = ++CurPrecedence;

  #define SAME_PRECEDENCE(O)      \
  Data[Token::O] = CurPrecedence;

  // Logical operators.
  NEXT_PRECEDENCE(LOr);
  NEXT_PRECEDENCE(LAnd);

  // Bitwise operators.
  NEXT_PRECEDENCE(BOr);
  NEXT_PRECEDENCE(BAnd);
  NEXT_PRECEDENCE(LShift);
  SAME_PRECEDENCE(RShift);

  // Relational operators.
  NEXT_PRECEDENCE(Equal);
  SAME_PRECEDENCE(NotEqual);
  NEXT_PRECEDENCE(Less);
  SAME_PRECEDENCE(LessOrEqual);
  SAME_PRECEDENCE(GreaterOrEqual);
  SAME_PRECEDENCE(Greater);

  // Algebraic operators.
  NEXT_PRECEDENCE(Add);
  SAME_PRECEDENCE(Sub);
  NEXT_PRECEDENCE(Mul);
  SAME_PRECEDENCE(Div);
  SAME_PRECEDENCE(Mod);

  #undef NEXT_PRECEDENCE
  #undef SAME_PRECEDENCE
}

// TODO: introduce TableTraits, and use a generic algorithm to print out any
// table in the compiler -- e.g. operator precedence table and symbol table.
void PrecedenceTable::Table::Dump(llvm::raw_ostream &OS) const {
  for(unsigned I = 0, E = Token::Count; I != E; ++I)
    OS << Token::Id(I) << " -> " << unsigned(Data[I]) << "\n";
}

void PrecedenceTable::Dump(llvm::raw_ostream &OS) const {
  Data->Dump(OS);
}

ExpressionBuilder::Table *ExpressionBuilder::Table::Instance = 0;

void ExpressionBuilder::Table::Fill() {
  std::memset(Data, 0, sizeof(Builder) * Token::Count);

  #define OPERATOR(O)                                         \
  Data[Token::O] = MakeExpressionAST<O ## ExprAST, O ## AST>;

  // Logical operators.
  OPERATOR(LOr);
  OPERATOR(LAnd);

  // Bitwise operators.
  OPERATOR(BOr);
  OPERATOR(BAnd);
  OPERATOR(LShift);
  OPERATOR(RShift);

  // Relational operators.
  OPERATOR(Equal);
  OPERATOR(NotEqual);
  OPERATOR(Less);
  OPERATOR(LessOrEqual);
  OPERATOR(GreaterOrEqual);
  OPERATOR(Greater);

  // Algebraic operators.
  OPERATOR(Add);
  OPERATOR(Sub);
  OPERATOR(Mul);
  OPERATOR(Div);
  OPERATOR(Mod);

  #undef OPERATOR
}

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
  return ParseList<NonEmptyVarDeclarationsAST, VarDeclarationAST>();
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
  return ParseList<DeclarationListAST, DeclarationAST, CommaAST>();
}

// declaration
//   : scalar_declaration
//   | array_declaration
DeclarationAST *Parser::ParseDeclaration() {
  // Tentative parsing here is not enough, because the tokens forming a
  // scalar declaration are actually the prefix of an array declaration. In
  // order to be greedy, we must look ahead and decide here which kind of
  // declaration we are going to parse.

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
  ScalarInitializerAST *ScalarInit = 0;

  // Just for symmetry with respect to the array initializer.
  if(InitializerAST *Init = ParseInitializer())
    ScalarInit = new ScalarInitializerAST(Init);

  return ScalarInit;
}

// initializer_list
//   : initializer *Comma* initializer_list
//   | initializer
InitializerListAST *Parser::ParseInitializerList() {
  return ParseList<InitializerListAST, InitializerAST, CommaAST>();
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
  return ParseList<NonEmptyStatementsAST, StatementAST>();
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

// assign_statement
//   : scalar_assignment
//   | array_assignment
AssignStatementAST *Parser::ParseAssignStatement() {
  // Tentative parsing here is not enough, because the tokens forming a scalar
  // assignment are actually the prefix of an array assignment. In order to be
  // greedy, we must look ahead and decide here which kind of assignment we are
  // going to parse.

  // If the next-next token is not a '[', we must parse a scalar assignment.
  if(!llvm::dyn_cast_or_null<LSquareTok>(Lex.Peek(1))) {
    if(ScalarAssignmentAST *Assign = ParseScalarAssignment())
      return new AssignStatementAST(Assign);

  // Otherwise, it must be an array declaration.
  } else {
    if(ArrayAssignmentAST *Assign = ParseArrayAssignment())
      return new AssignStatementAST(Assign);
  }

  // Parsing failed.
  return 0;
}

// read_write_statement
//   : read_statement
//   | write_statement
ReadWriteStatementAST *Parser::ParseReadWriteStatement() {
  ReadWriteStatementAST *Stmt = 0;

  // Try first with a 'read_statement' ...
  if(ReadStatementAST *Read = ParseReadStatement())
    Stmt = new ReadWriteStatementAST(Read);

  // ... and then with a 'write_statement'.
  else if(WriteStatementAST *Write = ParseWriteStatement())
    Stmt = new ReadWriteStatementAST(Write);

  return Stmt;
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

// scalar_assignment
//   : *Identifier* *Assign* expression
ScalarAssignmentAST *Parser::ParseScalarAssignment() {
  llvm::OwningPtr<IdentifierAST> LHS;
  llvm::OwningPtr<AssignAST> Assign;

  ScalarAssignmentAST *Stmt = 0;

  // LHS must be an identifier.
  if(!llvm::dyn_cast_or_null<IdentifierTok>(Lex.Peek(0))) {
    ReportError(ExpectedIdentifier, Lex.GetCurrentLoc());
    return 0;
  }

  LHS.reset(new IdentifierAST(Lex.TakeAs<IdentifierTok>()));

  // Assignment token expected.
  if(!llvm::dyn_cast_or_null<AssignTok>(Lex.Peek(0))) {
    ReportError(ExpectedAssign, Lex.GetCurrentLoc());
    return 0;
  }

  Assign.reset(new AssignAST(Lex.TakeAs<AssignTok>()));

  // At last we expect and expression as the RHS.
  if(ExpressionAST *RHS = ParseExpression())
    Stmt = new ScalarAssignmentAST(LHS.take(), Assign.take(), RHS);

  return Stmt;
}

// array_assignment
//   : *Identifier* *LSquare* expression *RSquare* *Assign* expression
ArrayAssignmentAST *Parser::ParseArrayAssignment() {
  llvm::OwningPtr<IdentifierAST> Id;
  llvm::OwningPtr<LSquareAST> OpenSquare;
  llvm::OwningPtr<ExpressionAST> Subscript;
  llvm::OwningPtr<RSquareAST> ClosedSquare;
  llvm::OwningPtr<AssignAST> Assign;

  ArrayAssignmentAST *Stmt = 0;

  // First we need the array identifier.
   if(!llvm::dyn_cast_or_null<IdentifierTok>(Lex.Peek(0))) {
    ReportError(ExpectedIdentifier, Lex.GetCurrentLoc());
    return 0;
  }

  Id.reset(new IdentifierAST(Lex.TakeAs<IdentifierTok>()));

  // Then we need the subscript. It must be prefixed by square.
  if(!llvm::dyn_cast_or_null<LSquareTok>(Lex.Peek(0))) {
    ReportError(ExpectedLSquare, Lex.GetCurrentLoc());
    return 0;
  }

  OpenSquare.reset(new LSquareAST(Lex.TakeAs<LSquareTok>()));

  // The subscript itself is an expression.
  Subscript.reset(ParseExpression());

  if(!Subscript)
    return 0;

  // The subscript is then followed by a square.
  if(!llvm::dyn_cast_or_null<RSquareTok>(Lex.Peek(0))) {
    ReportError(ExpectedRSquare, Lex.GetCurrentLoc());
    return 0;
  }

  ClosedSquare.reset(new RSquareAST(Lex.TakeAs<RSquareTok>()));

  // Assignment token expected.
  if(!llvm::dyn_cast_or_null<AssignTok>(Lex.Peek(0))) {
    ReportError(ExpectedAssign, Lex.GetCurrentLoc());
    return 0;
  }

  Assign.reset(new AssignAST(Lex.TakeAs<AssignTok>()));

  // At last we expect an expression as the RHS.
  if(ExpressionAST *RHS = ParseExpression())
    Stmt = new ArrayAssignmentAST(Id.take(),
                                  OpenSquare.take(),
                                  Subscript.take(),
                                  ClosedSquare.take(),
                                  Assign.take(),
                                  RHS);
  return Stmt;
}

// read_statement
//   : *Read* *LPar* *Identifier* *RPar*
ReadStatementAST *Parser::ParseReadStatement() {
  llvm::OwningPtr<ReadAST> Read;
  llvm::OwningPtr<LParAST> LPar;
  llvm::OwningPtr<IdentifierAST> Id;
  llvm::OwningPtr<RParAST> RPar;

  // First token must be the keyword.
  if(!llvm::dyn_cast_or_null<ReadTok>(Lex.Peek(0)))
    return 0;

  Read.reset(new ReadAST(Lex.TakeAs<ReadTok>()));

  // Second must be the opening paren.
  if(!llvm::dyn_cast_or_null<LParTok>(Lex.Peek(0))) {
    ReportError(ExpectedLPar, Lex.GetCurrentLoc());
    return 0;
  }

  LPar.reset(new LParAST(Lex.TakeAs<LParTok>()));

  // Between parens, there must be the identifier to store the read value to.
  if(!llvm::dyn_cast_or_null<IdentifierTok>(Lex.Peek(0))) {
    ReportError(ExpectedIdentifier, Lex.GetCurrentLoc());
    return 0;
  }

  Id.reset(new IdentifierAST(Lex.TakeAs<IdentifierTok>()));

  // And at last, the closing paren.
  if(!llvm::dyn_cast_or_null<RParTok>(Lex.Peek(0))) {
    ReportError(ExpectedRPar, Lex.GetCurrentLoc());
    return 0;
  }

  RPar.reset(new RParAST(Lex.TakeAs<RParTok>()));

  return new ReadStatementAST(Read.take(), LPar.take(), Id.take(), RPar.take());
}

// write_statement
//   : *Write* *LPar* expression *RPar*
WriteStatementAST *Parser::ParseWriteStatement() {
  llvm::OwningPtr<WriteAST> Write;
  llvm::OwningPtr<LParAST> LPar;
  llvm::OwningPtr<ExpressionAST> Expr;
  llvm::OwningPtr<RParAST> RPar;

  // First token must be the keyword.
  if(!llvm::dyn_cast_or_null<WriteTok>(Lex.Peek(0)))
    return 0;

  Write.reset(new WriteAST(Lex.TakeAs<WriteTok>()));

  // Second must be the opening paren.
  if(!llvm::dyn_cast_or_null<LParTok>(Lex.Peek(0))) {
    ReportError(ExpectedLPar, Lex.GetCurrentLoc());
    return 0;
  }

  LPar.reset(new LParAST(Lex.TakeAs<LParTok>()));

  // Now, try to parse an expression.
  Expr.reset(ParseExpression());

  if(!Expr)
    return 0;

  // At last, the closing paren.
  if(!llvm::dyn_cast_or_null<RParTok>(Lex.Peek(0))) {
    ReportError(ExpectedRPar, Lex.GetCurrentLoc());
    return 0;
  }

  RPar.reset(new RParAST(Lex.TakeAs<RParTok>()));

  return new WriteStatementAST(Write.take(),
                               LPar.take(),
                               Expr.take(),
                               RPar.take());
}

// expression
//   : expression *LAnd* expression
//   | expression *LOr* expression
//   | expression *BAnd* expression
//   | expression *BOr* expression
//   | expression *LShift* expression
//   | expression *RShift* expression
//   | expression *Less* expression
//   | expression *LessOrEqual* expression
//   | expression *Equal* expression
//   | expression *NotEqual* expression
//   | expression *GreaterOrEqual* expression
//   | expression *Greater* expression
//   | expression *Add* expression
//   | expression *Sub* expression
//   | expression *Mul* expression
//   | expression *Div* expression
//   | expression *Mod* expression
//   | primary_expression
ExpressionAST *Parser::ParseExpression() {
  typedef ParsingFrame<Token *, ExpressionAST *> IncompleteExpr;

  ParsingStack<IncompleteExpr, 8> Stack;

  ExpressionAST *Expr = ParsePrimaryExpression();

  // We need at lease one expression to start parsing.
  if(!Expr)
    return 0;

  // Initialize the parsing stack.
  Stack.push(MakeParsingFrame(static_cast<Token *>(0), Expr));

  const Token *TopTok = 0;
  const Token *CurTok = Lex.Peek(0);

  // Operator precedence parsing for binary expressions is very simple. The
  // algorithm keep a stack. On the top of the stack there is an operator,
  // together with its right operand.
  while(IsBinaryOperator(CurTok)) {
    // If the current token has an higher precedence that the one on the top of
    // the stack, top that means we have to evaluate it first.
    while(GetPrecedence(TopTok) < GetPrecedence(CurTok)) {
      llvm::OwningPtr<Token> Oper(Lex.Take());

      Expr = ParsePrimaryExpression();

      if(!Expr)
        return 0;

      // Thus, push a new frame on the parsing stack.
      Stack.push(MakeParsingFrame(Oper.take(), Expr));

      TopTok = CurTok;
      CurTok = Lex.Peek(0);
    }

    // The other case is when the precedence of the current token do not exceed
    // the one of the token at the top of the stack. That means that the
    // operand on the top of the stack is linked to the former operator.
    while(GetPrecedence(TopTok) >= GetPrecedence(CurTok)) {
      IncompleteExpr *CurExpr = Stack.end() - 1;
      IncompleteExpr *TopExpr = Stack.end() - 2;

      // We can thus reduce a binary expression tree. The left operand is found
      // on the last but one entry of the stack.
      ExpressionAST *LHS = TopExpr->GetSecond();
      Token *Oper = CurExpr->GetFirst();
      ExpressionAST *RHS = CurExpr->GetSecond();

      // Which, in turn, see its right operand replaced with the new expression
      // AST.
      TopExpr->SetSecond(CreateBinaryExpression(LHS, Oper, RHS));

      Stack.pop();

      TopTok = TopExpr->GetFirst();
    }
  }

  // At the end, the stack should store only one entry, which actually is the
  // root of a whole expression AST.
  assert(Stack.size() == 1 && "Corrupted expressions stack");

  IncompleteExpr *TopExpr = Stack.end() - 1;
  ExpressionAST *FinalExpr = TopExpr->GetSecond();

  // Remember to remove the last element of the stack, otherwise it will be
  // freed by the stack destructor.
  Stack.pop();

  return FinalExpr;
}

// primary_expression
//   : *LPar* expression *RPar*
//   | *BNot* expression
//   | *Sub* expression
//   | *Identifier*
//   | *Identifier* *LSquare* expression *RSquare*
//   | *Number*
ExpressionAST *Parser::ParsePrimaryExpression() {
  const Token *CurTok = Lex.Peek(0);

  // No token available, hence nothing to parse.
  if(!CurTok)
    return 0;

  ExpressionAST *Expr = 0;

  switch(CurTok->GetId()) {
  case Token::LPar: {
    llvm::OwningPtr<LParAST> LPar(new LParAST(Lex.TakeAs<LParTok>()));
    llvm::OwningPtr<ExpressionAST> InnerExpr(ParseExpression());

    if(InnerExpr && llvm::dyn_cast_or_null<RParTok>(Lex.Peek(0))) {
      RParAST *RPar = new RParAST(Lex.TakeAs<RParTok>());
      Expr = new NestedExprAST(LPar.take(), InnerExpr.take(), RPar);
    }

    break;
  }

  case Token::BNot: {
    llvm::OwningPtr<BNotAST> BNot(new BNotAST(Lex.TakeAs<BNotTok>()));

    if(ExpressionAST *InnerExpr = ParseExpression())
      Expr = new BNotExprAST(BNot.take(), InnerExpr);

    break;
  }

  case Token::Sub: {
    llvm::OwningPtr<SubAST> Sub(new SubAST(Lex.TakeAs<SubTok>()));

    if(ExpressionAST *InnerExpr = ParseExpression())
      Expr = new MinusExprAST(Sub.take(), InnerExpr);

    break;
  }

  case Token::Identifier: {
    llvm::OwningPtr<IdentifierAST> Id;

    Id.reset(new IdentifierAST(Lex.TakeAs<IdentifierTok>()));

    if(!llvm::dyn_cast_or_null<LSquareTok>(Lex.Peek(0)))
      Expr = new ScalarIdentifierExprAST(Id.take());

    else {
      llvm::OwningPtr<LSquareAST> OpenSquare;
      llvm::OwningPtr<ExpressionAST> Subscript;
      llvm::OwningPtr<RSquareAST> ClosedSquare;

      if(!llvm::dyn_cast_or_null<LSquareTok>(Lex.Peek(0))) {
        ReportError(ExpectedLSquare, Lex.GetCurrentLoc());
        return 0;
      }

      OpenSquare.reset(new LSquareAST(Lex.TakeAs<LSquareTok>()));

      Subscript.reset(ParseExpression());

      if(!Subscript)
        return 0;

      if(!llvm::dyn_cast_or_null<RSquareTok>(Lex.Peek(0))) {
        ReportError(ExpectedRSquare, Lex.GetCurrentLoc());
        return 0;
      }

      ClosedSquare.reset(new RSquareAST(Lex.TakeAs<RSquareTok>()));

      Expr = new ArrayIdentifierExprAST(Id.take(),
                                        OpenSquare.take(),
                                        Subscript.take(),
                                        ClosedSquare.take());
    }

    break;
  }

  case Token::Number: {
    Expr = new NumberExprAST(new NumberAST(Lex.TakeAs<NumberTok>()));
    break;
  }

  default:
    break;
  }

  return Expr;
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

// Specialization of list parser templates. For each AST which is actually a
// list, the ListParseTraits template must be specialized in order to tell to
// the generic list parsing function ParseList which parser to use for parsing a
// list element.
namespace acse {

#define LIST_TRAITS(L, N, S)                                        \
template <>                                                         \
struct ListParseTraits<L ## AST, N ## AST, S>                       \
  : public DefaultListParseTraits<L ## AST, N ## AST, S> {          \
  static NodeParser GetNodeParser() { return &Parser::Parse ## N; } \
};

LIST_TRAITS(NonEmptyVarDeclarations, VarDeclaration, ListParseNoSepTag)
LIST_TRAITS(NonEmptyStatements, Statement, ListParseNoSepTag)

#undef LIST_TRAITS

#define LIST_TRAITS(L, N, S) \
template <>                                                         \
struct ListParseTraits<L ## AST, N ## AST, S ## AST>                \
  : public DefaultListParseTraits<L ## AST, N ## AST, S ## AST> {   \
  static NodeParser GetNodeParser() { return &Parser::Parse ## N; } \
};

LIST_TRAITS(DeclarationList, Declaration, Comma)
LIST_TRAITS(InitializerList, Initializer, Comma)

#undef LIST_TRAITS

} // End namespace acse.
