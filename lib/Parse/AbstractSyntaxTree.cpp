//===- AbstractSyntaxTree.h - LANCE Abstract Syntax Tree --------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "acse/Parse/AbstractSyntaxTree.h"

#include "llvm/Support/GraphWriter.h"

using namespace acse;

namespace llvm {

template <>
struct DOTGraphTraits<const AbstractSyntaxTreeNode *>
  : public DefaultDOTGraphTraits {
  DOTGraphTraits(bool IsSimple = false)
    : DefaultDOTGraphTraits(IsSimple) { }

  static inline std::string getNodeLabel(const AbstractSyntaxTreeNode *Node,
                                         const AbstractSyntaxTreeNode *Root) {
    std::string Label;

    llvm::raw_string_ostream OS(Label);
    OS << Node->GetId();

    return Label;
  }
};

template <>
struct DOTGraphTraits<const AbstractSyntaxTree *>
  : public DOTGraphTraits<const AbstractSyntaxTreeNode *> {
  DOTGraphTraits(bool IsSimple = false)
    : DOTGraphTraits<const AbstractSyntaxTreeNode *>() { }

  static inline std::string getGraphName(const AbstractSyntaxTree *AST) {
    return "Abstract Syntax Tree";
  }

  static inline std::string getNodeLabel(const AbstractSyntaxTreeNode *Node,
                                         const AbstractSyntaxTree *AST) {
    typedef DOTGraphTraits<const AbstractSyntaxTreeNode *> Super;
    return Super::getNodeLabel(Node, AST->GetRoot());
  }
};

} // End namespace llvm.

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

llvm::raw_ostream &acse::operator<<(llvm::raw_ostream &OS,
                                    AbstractSyntaxTreeNode::Id Identifier) {
  switch(Identifier) {
  #define AST(I)                  \
  case AbstractSyntaxTreeNode::I: \
    OS << # I;                    \
    break;

  // Special ASTs.
  AST(Empty)

  // Braces and co.
  AST(LBrace)
  AST(RBrace)
  AST(LSquare)
  AST(RSquare)
  AST(LPar)
  AST(RPar)

  // Separators/terminators.
  AST(SemiColon)
  AST(Colon)
  AST(Comma)
  AST(Assign)

  // Algebraic operators.
  AST(Add)
  AST(Sub)
  AST(Mul)
  AST(Div)
  AST(Mod)

  // Relational operators.
  AST(Less)
  AST(LessOrEqual)
  AST(Equal)
  AST(NotEqual)
  AST(GreaterOrEqual)
  AST(Greater)

  // Bitwise operators.
  AST(BAnd)
  AST(BOr)
  AST(BNot)
  AST(LShift)
  AST(RShift)

  // Logical operators.
  AST(LAnd)
  AST(LOr)

  // Keywords.
  AST(Int)
  AST(If)
  AST(Else)
  AST(Do)
  AST(While)
  AST(Read)
  AST(Write)

  // Typed.
  AST(Number)
  AST(Identifier)

  // Non-terminal rule ASTs: grammar root.
  AST(Program)

  // Non-terminal rule ASTs: variable declarations.
  AST(VarDeclarations)
  AST(NonEmptyVarDeclarations)
  AST(VarDeclaration)

  // Non-terminal rule ASTs: declarations.
  AST(DeclarationList)
  AST(Declaration)
  AST(ScalarDeclaration)
  AST(Type)
  AST(Initializer)

  #undef AST

  default:
    OS << "AST-" << static_cast<unsigned>(Identifier);
  }

  return OS;
}

//
// AbstractSyntaxTree implementation.
//

void AbstractSyntaxTree::Dump(llvm::raw_ostream &OS) const { }

void AbstractSyntaxTree::View() const {
  llvm::ViewGraph(this, "abstract-syntax-tree");
}
