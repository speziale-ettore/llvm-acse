//===- CompilerInstance.cpp - A Compiler for LANCE --------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "acse/Compile/CompilerInstance.h"
#include "acse/Compile/Pass.h"
#include "acse/IR/AbstractSyntaxTreeVisitor.h"
#include "acse/Parse/Parser.h"

#include "llvm/Support/CommandLine.h"

using namespace acse;

namespace {

llvm::cl::opt<bool>
SyntaxOnly("fsyntax-only",
           llvm::cl::desc("Stop compilation after parsing"),
           llvm::cl::init(false));

llvm::cl::opt<bool>
EmitIR("emit-ir",
       llvm::cl::desc("Emit LLVM IR bytecode"),
       llvm::cl::init(false));

llvm::cl::opt<bool>
EmitAssembly("S",
             llvm::cl::desc("Emit target assembly"),
             llvm::cl::init(false));

class Pipeline {
public:
  Pipeline(AbstractSyntaxTree *AST) : AST(AST) {
    if(SyntaxOnly)
      return;

    // TODO: add semantic check passes.
    // TODO: add pass to LLVM-IR.

    if(EmitIR)
      return;

    // TODO: add machine code pass.
  }

private:
  Pipeline(const Pipeline &That) LLVM_DELETED_FUNCTION;
  const Pipeline &operator=(const Pipeline &That) LLVM_DELETED_FUNCTION;

public:
  bool Run(llvm::raw_ostream &OS) {
    bool ErrorsFound = false;

    return ErrorsFound;
  }

private:
  AbstractSyntaxTree *AST;
  llvm::SmallVector<Pass *, 4> Stages;
};

} // End anonymous namespace.

bool CompilerInstance::Run() {
  Lexer Lex(Srcs);

  Parser Parse(Lex);
  ErrorsFound = !Parse.Run();

  if(!ErrorsFound) {
    Pipeline Pipe(Parse.TakeAST());
    ErrorsFound = !Pipe.Run(Dst);
  }

  return !ErrorsFound;
}
