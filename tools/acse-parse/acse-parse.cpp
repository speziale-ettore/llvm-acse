//===- acse-parse.cpp - Standalone LANCE Parser -----------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "acse/Parse/Parser.h"

#include "llvm/ADT/OwningPtr.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/system_error.h"
#include "llvm/Support/ToolOutputFile.h"

using namespace acse;

static llvm::cl::opt<std::string>
InputFileName(llvm::cl::Positional,
              llvm::cl::desc("<input LANCE file>"),
              llvm::cl::init("-"),
              llvm::cl::value_desc("filename"));

static llvm::cl::opt<std::string>
OutputFileName("o",
               llvm::cl::desc("Override output filename"),
               llvm::cl::init("-"),
               llvm::cl::value_desc("filename"));

static llvm::cl::opt<bool>
ViewAST("view-ast",
        llvm::cl::desc("View AST with dotty"),
        llvm::cl::init(false));

int main(int argc, char *argv[]) {
  // Setup pretty stack trace printers.
  llvm::PrettyStackTraceProgram X(argc, argv);
  llvm::sys::PrintStackTraceOnErrorSignal();

  // Automatically call llvm_shutdown on exit -- release resources used by
  // ManagedStatic instances.
  llvm::llvm_shutdown_obj Y;

  llvm::cl::ParseCommandLineOptions(argc, argv, "ACSE standalone parser");

  llvm::OwningPtr<llvm::MemoryBuffer> Input;
  llvm::OwningPtr<llvm::tool_output_file> Output;

  llvm::error_code ErrorCode;
  std::string ErrorInfo;

  ErrorCode = llvm::MemoryBuffer::getFileOrSTDIN(InputFileName, Input);
  if(ErrorCode) {
    llvm::errs() << "Error opening input file '" << InputFileName << "'\n";
    return EXIT_FAILURE;
  }

  Output.reset(new llvm::tool_output_file(OutputFileName.c_str(), ErrorInfo));
  if(!ErrorInfo.empty()) {
    llvm::errs() << ErrorInfo << "\n";
    return EXIT_FAILURE;
  }

  llvm::SourceMgr Srcs;
  Srcs.AddNewSourceBuffer(Input.take(), llvm::SMLoc());

  Lexer Lex(Srcs);
  Parser Parse(Lex);

  Parse.Run();

  if(!Parse.Success())
    return EXIT_FAILURE;

  AbstractSyntaxTree *AST = Parse.GetAST();

  if(ViewAST)
    AST->View();
  else
    AST->Dump(Output->os());

  Output->keep();

  return EXIT_SUCCESS;
}
