//===- CompilerInstance.cpp - A Compiler for LANCE --------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "acse/Compile/CompilerInstance.h"
#include "acse/Compile/IRTranslator.h"
#include "acse/Compile/SymbolsChecker.h"
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
EmitAssembly("S",
             llvm::cl::desc("Emit readable assembly"),
             llvm::cl::init(false));

class Pipeline {
public:
  Pipeline(AbstractSyntaxTree *AST) : AST(AST), Module(0) {
    if(SyntaxOnly)
      return;

    Stages.push_back(new SymbolsChecker());
    Stages.push_back(new IRTranslator(Module));
  }

  ~Pipeline() {
    delete Module;
  }

private:
  Pipeline(const Pipeline &That) LLVM_DELETED_FUNCTION;
  const Pipeline &operator=(const Pipeline &That) LLVM_DELETED_FUNCTION;

public:
  bool Run(llvm::raw_ostream &OS) {
    typedef llvm::SmallVector<Pass *, 4>::const_iterator iterator;

    bool ErrorsFound = false;

    for(iterator I = Stages.begin(), E = Stages.end();
        I != E && !ErrorsFound;
        ++I)
      ErrorsFound = !(*I)->Run(*AST);

    return !ErrorsFound;
  }

  llvm::Module &GetModule() {
    return *Module;
  }

private:
  AbstractSyntaxTree *AST;
  llvm::Module *Module;

  llvm::SmallVector<Pass *, 4> Stages;
};

} // End anonymous namespace.

bool CompilerInstance::Run() {
  Lexer Lex(Srcs);

  Parser Parse(Lex);
  ErrorsFound = !Parse.Run();

  if(ErrorsFound)
    return false;

  Pipeline Pipe(Parse.TakeAST());
  ErrorsFound = !Pipe.Run(Dst);

  if(ErrorsFound)
    return false;

  if(EmitAssembly)
    Dst << Pipe.GetModule();
  else
    llvm_unreachable("Not yet implemented");

  return true;
}
