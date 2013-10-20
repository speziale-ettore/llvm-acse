//===- AbstractSyntaxTreeNode.cpp - LANCE AST Internal Nodes ----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "acse/IR/AbstractSyntaxTreeNode.h"
#include "acse/IR/AbstractSyntaxTreeVisitor.h"

using namespace acse;

//
// InitializerLIST implementation.
//

namespace {

class InitializerListASTSize
  : public PreOrderAbstractSyntaxTreeVisitor<InitializerListASTSize> {
public:
  typedef InitializerListAST::SizeTy SizeTy;
public:
  InitializerListASTSize(const InitializerListAST &InitList)
    : PreOrderAbstractSyntaxTreeVisitor<InitializerListASTSize>(InitList) { }

public:
  SizeTy GetListSize() {
    Size = 0;

    // Visit all initializer in the list to get initializer list length.
    Visit();

    return Size;
  }

  NextAction VisitInitializer(const InitializerAST &Init) {
    ++Size;

    return SkipChildren;
  }

private:
  SizeTy Size;
};

} // End anonymous namespace.

InitializerListAST::SizeTy InitializerListAST::GetInitializerSize() const {
  InitializerListASTSize Size(*this);
  return Size.GetListSize();
}
