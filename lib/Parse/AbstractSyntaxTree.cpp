//===- AbstractSyntaxTree.h - LANCE Abstract Syntax Tree --------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "acse/Parse/AbstractSyntaxTree.h"
#include "acse/Parse/AbstractSyntaxTreeVisitor.h"

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

namespace {

class AbstractSyntaxTreePrinter
  : public PreOrderAbstractSyntaxTreeVisitor<AbstractSyntaxTreePrinter> {
public:
  AbstractSyntaxTreePrinter(const AbstractSyntaxTree &AST,
                            llvm::raw_ostream &OS)
    : PreOrderAbstractSyntaxTreeVisitor(AST),
      OS(OS) { }

public:
  #define AST(I)                               \
  NextAction Visit ## I(const I ## AST &AST) { \
    PrintInEdge();                             \
    PrintRule(AST);                            \
    PrintOutEdge();                            \
                                               \
    return Continue;                           \
  }
  #include "acse/Parse/AbstractSyntaxTreeNode.def"
  #undef AST

private:
  void PrintInEdge() {
    iterator I = begin(), E = end();

    // Root has not predecessors: nothing to print.
    if(I == E)
      return;

    OS << " ";
    for(iterator J = I++; I != E; ++I, ++J) {
      if(!J->IsRightmostChild(&*I))
        OS << "|";
      else
        OS << " ";
      OS << "    ";
    }

    OS << "+-> ";
  }

  void PrintRule(const AbstractSyntaxTreeNode &AST) {
    if(llvm::isa<ExpressionAST>(AST))
      OS << "Expression";
    else
      OS << AST.GetId();
  }

  void PrintOutEdge() {
    OS << "\n";
  }

private:
  llvm::raw_ostream &OS;
};

} // End anonymous namespace.

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
  #include "acse/Parse/AbstractSyntaxTreeNode.def"
  #undef AST
  }

  return OS;
}

//
// AbstractSyntaxTree implementation.
//

void AbstractSyntaxTree::Dump(llvm::raw_ostream &OS) const {
  AbstractSyntaxTreePrinter Printer(*this, OS);
  Printer.Visit();
}

void AbstractSyntaxTree::View() const {
  llvm::ViewGraph(this, "abstract-syntax-tree");
}
