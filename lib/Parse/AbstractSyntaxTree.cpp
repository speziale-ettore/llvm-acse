//===- AbstractSyntaxTree.h - LANCE Abstract Syntax Tree --------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "acse/Parse/AbstractSyntaxTree.h"

using namespace acse;

//
// AbstractSyntaxTreeNode implementation.
//

// TODO: compute the start/end locations by composing subtree locations.
llvm::SMLoc AbstractSyntaxTreeNode::GetStartLoc() const {
  return StartLoc;
}

// TODO: compute the start/end locations by composing subtree locations.
llvm::SMLoc AbstractSyntaxTreeNode::GetEndLoc() const {
  return EndLoc;
}

//
// AbstractSyntaxTree implementation.
//

void AbstractSyntaxTree::Dump(llvm::raw_ostream &OS) const { }
