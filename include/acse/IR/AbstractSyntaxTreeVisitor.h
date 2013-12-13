//===- AbstractSyntaxTreeVisitor.h - Tree Visitors --------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef ACSE_IR_ABSTRACTSYNTAXTREEVISITOR_H
#define ACSE_IR_ABSTRACTSYNTAXTREEVISITOR_H

#include "acse/IR/AbstractSyntaxTree.h"

#include "llvm/Support/ErrorHandling.h"

namespace acse {

template <typename DerivedTy>
class PreOrderAbstractSyntaxTreeVisitor {
public:
  typedef llvm::df_iterator<const AbstractSyntaxTreeNode *> DepthFirstIterator;

  enum NextAction {
    Continue,
    SkipChildren,
    Terminate
  };

  // TODO: explain why the iterator do not implement all needed members.
  class iterator {
  public:
    typedef const AbstractSyntaxTreeNode value_type;
    typedef ptrdiff_t difference_type;

    typedef value_type *pointer;
    typedef value_type &reference;

    typedef std::random_access_iterator_tag iterator_category;

  public:
    static inline iterator begin(const DepthFirstIterator &Base) {
      return iterator(0, Base);
    }

    static inline iterator end(const DepthFirstIterator &Base) {
      unsigned Predecessors = Base.getPathLength();
 
      // In the case the path is non-empty, we have to discard the last node --
      // i.e. current node being visited -- because it is not a predecessor.
      if(Predecessors)
        --Predecessors;

      return iterator(Predecessors, Base);
    }

  public:
    iterator(const iterator &That) : Cur(That.Cur),
                                     Base(That.Base) { }

    const iterator &operator=(const iterator &That) {
      if(this != &That) {
        Cur = That.Cur;
        Base = That.Base;
      }

      return *this;
    }

  private:
    iterator(unsigned Cur, const DepthFirstIterator &Base) : Cur(Cur),
                                                             Base(Base) { }

  public:
    bool operator==(const iterator &That) const {
      return Cur == That.Cur && Base == That.Base;
    }

    bool operator!=(const iterator &That) const {
      return Cur != That.Cur || Base != That.Base;
    }

  public:
    bool operator<(const iterator &That) const {
      return Cur < That.Cur;
    }

    bool operator<=(const iterator &That) const {
      return Cur <= That.Cur;
    }

    bool operator>=(const iterator &That) const {
      return Cur >= That.Cur;
    }

    bool operator>(const iterator &That) const {
      return Cur > That.Cur;
    }

  public:
    iterator &operator++() {
      ++Cur;
      return *this;
    }

    iterator operator++(int Ign) {
      iterator Prev = *this;
      ++*this;
      return Prev;
    }

    iterator &operator--() {
      --Cur;
      return *this;
    }

    iterator operator--(int Ign) {
      iterator Prev = *this;
      --*this;
      return Prev;
    }

  public:
    iterator &operator+=(difference_type N) {
      Cur += N;
      return *this;
    }

    iterator &operator-=(difference_type N) {
      Cur -= N;
      return *this;
    }

    iterator operator+(difference_type N) const {
      return iterator(Cur + N, Base);
    }

    iterator operator-(difference_type N) const {
      return iterator(Cur - N, Base);
    }

    value_type &operator[](difference_type N) const {
      return Base.getPath(Cur + N);
    }

  public:
    reference operator*() const { return *Base.getPath(Cur); }
    pointer operator->() const { return Base.getPath(Cur); }

  private:
    unsigned Cur;
    DepthFirstIterator Base;

    friend class PreOrderAbstractSyntaxTreeVisitor;
  };

public:
  iterator begin() const {
    return iterator::begin(I);
  }

  iterator end() const {
    return iterator::end(I);
  }

protected:
  PreOrderAbstractSyntaxTreeVisitor(const AbstractSyntaxTree &AST)
    : Root(AST.GetRoot()),
      I(llvm::df_begin(Root)),
      E(llvm::df_end(Root)) { }

  PreOrderAbstractSyntaxTreeVisitor(const AbstractSyntaxTreeNode &AST)
    : Root(&AST),
      I(llvm::df_begin(Root)),
      E(llvm::df_end(Root)) { }

public:
  void Visit() {
    NextAction Next = Continue;

    while(I != E && Next != Terminate) {
      Next = Dispatch(**I);

      if(Next == Continue)
        I.operator++();
      else if(Next == SkipChildren)
        I.skipChildren();
    }
  }

  #define AST(I)                               \
  NextAction Visit ## I(const I ## AST &AST) { \
    return Continue;                           \
  }
  #include "acse/IR/AbstractSyntaxTreeNode.def"
  #undef AST

private:
  NextAction Dispatch(const AbstractSyntaxTreeNode &AST) {
    DerivedTy *This = static_cast<DerivedTy *>(this);

    #define AST(I)                                              \
    if(const I ## AST *Casted = llvm::dyn_cast<I ## AST>(&AST)) \
      return This->Visit ## I(*Casted);
    #include "acse/IR/AbstractSyntaxTreeNode.def"
    #undef AST

    llvm_unreachable("Unknown AST node type");
  }

private:
  const AbstractSyntaxTreeNode *Root;

  DepthFirstIterator I;
  DepthFirstIterator E;
};

template <typename DerivedTy>
class PostOrderAbstractSyntaxTreeVisitor {
public:
  typedef llvm::po_iterator<const AbstractSyntaxTreeNode *> DepthFirstIterator;

  enum NextAction {
    Continue,
    Terminate
  };

protected:
  PostOrderAbstractSyntaxTreeVisitor(const AbstractSyntaxTree &AST)
    : Root(AST.GetRoot()),
    I(llvm::po_begin(Root)),
    E(llvm::po_end(Root)) { }

  PostOrderAbstractSyntaxTreeVisitor(const AbstractSyntaxTreeNode &AST)
    : Root(&AST),
    I(llvm::po_begin(Root)),
    E(llvm::po_end(Root)) { }

public:
  void Visit() {
    NextAction Next = Continue;

    for(; I != E && Next != Terminate; ++I)
      Next = Dispatch(**I);
  }

  #define AST(I)                               \
  NextAction Visit ## I(const I ## AST &AST) { \
    return Continue;                           \
  }
  #include "acse/IR/AbstractSyntaxTreeNode.def"
  #undef AST

private:
  NextAction Dispatch(const AbstractSyntaxTreeNode &AST) {
    DerivedTy *This = static_cast<DerivedTy *>(this);

    #define AST(I)                                              \
    if(const I ## AST *Casted = llvm::dyn_cast<I ## AST>(&AST)) \
      return This->Visit ## I(*Casted);
    #include "acse/IR/AbstractSyntaxTreeNode.def"
    #undef AST

    llvm_unreachable("Unknown AST node type");
  }

private:
  const AbstractSyntaxTreeNode *Root;

  DepthFirstIterator I;
  DepthFirstIterator E;
};

} // End namespace acse.

#endif // ACSE_IR_ABSTRACTSYNTAXTREEVISITOR_H
