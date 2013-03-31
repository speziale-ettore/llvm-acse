//===- AbstractSyntaxTree.h - LANCE Abstract Syntax Tree --------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef ACSE_PARSE_ABSTRACTSYNTAXTREE_H
#define ACSE_PARSE_ABSTRACTSYNTAXTREE_H

#include "acse/Parse/AbstractSyntaxTreeNode.h"

#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/OwningPtr.h"

namespace acse {

// A common practice in C++ data structures design is to divide the
// implementation of the core data structure from the interface exposed to the
// user. In the context of the AST, the class AbstractSyntaxTreeNode implements
// the core structure of the tree, while AbstractSyntaxTree is the interface.
//
// This enable to put inside AbstractSyntaxTreeNode only data and member
// functions needed to build a working tree. High-level accessors methods, such
// as printing or viewing the AST are implemented inside AbstractSyntaxTree.
//
// In more sophisticated designs, this is also a good place where implement
// high-level queries about the tree contents and structure, and possibly
// caching their results.
class AbstractSyntaxTree {
public:
  typedef llvm::df_iterator<AbstractSyntaxTreeNode *> iterator;
  typedef llvm::df_iterator<const AbstractSyntaxTreeNode *> const_iterator;

public:
  iterator begin() {
    if(Root)
      return llvm::df_begin(Root.get());
    else
      return llvm::df_end(Root.get());
  }

  iterator end() {
    return llvm::df_end(Root.get());
  }

  const_iterator begin() const {
    const AbstractSyntaxTreeNode *CRoot =
      const_cast<const AbstractSyntaxTreeNode *>(Root.get());

    if(CRoot)
      return llvm::df_begin(CRoot);
    else
      return llvm::df_end(CRoot);
  }

  const_iterator end() const {
    const AbstractSyntaxTreeNode *CRoot =
      const_cast<const AbstractSyntaxTreeNode *>(Root.get());

    return llvm::df_end(CRoot);
  }

public:
  AbstractSyntaxTree(ProgramAST *Root) : Root(Root) { }

private:
  AbstractSyntaxTree(const AbstractSyntaxTree &That)
    LLVM_DELETED_FUNCTION;
  const AbstractSyntaxTree &operator=(const AbstractSyntaxTree &That)
    LLVM_DELETED_FUNCTION;

public:
  ProgramAST *GetRoot() const { return llvm::cast<ProgramAST>(Root.get()); }

public:
  void Dump(llvm::raw_ostream &OS = llvm::errs()) const;
  void View() const;

private:
  llvm::OwningPtr<AbstractSyntaxTreeNode> Root;
};

} // End namespace acse.

// Since we defined AbstractSyntaxTree to be our entry point to the AST, it must
// acts like a graph. However, with respect to AbstractSyntaxTreeNode, we do not
// have a set of sub-trees to visit. Indeed, AbstractSyntaxTree acts like a
// proxy, hence the real graph is the node AbstractSyntaxTree identifies as the
// root.
//
// Moreover, since it is an entry point, it is useful iterating over all nodes
// in the AST. Please notice that this feature is not supported in
// AbstractSyntaxTreeNode, basically because when you are dealing with an
// internal node you are usually interested on its immediate successors, not on
// all its successors.
namespace llvm {

using namespace acse;

template <>
struct GraphTraits<AbstractSyntaxTree *>
  : public GraphTraits<AbstractSyntaxTreeNode *> {
  typedef AbstractSyntaxTree::iterator nodes_iterator;

  static inline
  NodeType *getEntryNode(AbstractSyntaxTree *AST) {
    return AST->GetRoot();
  }

  static inline nodes_iterator nodes_begin(AbstractSyntaxTree *AST) {
    return AST->begin();
  }

  static inline nodes_iterator nodes_end(AbstractSyntaxTree *AST) {
    return AST->end();
  }
};

template <>
struct GraphTraits<const AbstractSyntaxTree *>
  : public GraphTraits<const AbstractSyntaxTreeNode *> {
  typedef AbstractSyntaxTree::const_iterator nodes_iterator;

  static inline
  NodeType *getEntryNode(const AbstractSyntaxTree *AST) {
    return AST->GetRoot();
  }

  static inline nodes_iterator nodes_begin(const AbstractSyntaxTree *AST) {
    return AST->begin();
  }

  static inline nodes_iterator nodes_end(const AbstractSyntaxTree *AST) {
    return AST->end();
  }
};

} // End namespace llvm.

#endif // ACSE_PARSE_ABSTRACTSYNTAXTREE_H
