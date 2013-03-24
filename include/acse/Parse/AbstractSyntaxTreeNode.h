//===- AbstractSyntaxTreeNode.h - LANCE AST Internal Nodes ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef ACSE_LEX_ABSTRACTSYNTAXTREENODE_H
#define ACSE_LEX_ABSTRACTSYNTAXTREENODE_H

#include "acse/Lex/Token.h"

#include "llvm/ADT/GraphTraits.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/Support/Casting.h"

// TODO: put grammar rule on top of each corresponding node type.
namespace acse {

// The abstract syntax tree -- AST -- is the central structure of every compiler
// front-end. Its main duty is to represent with the maximum precision the
// contents of a structured token stream. For the compiler point of view, this
// usually results on storing in the AST the relationship between the AST nodes
// and some semantic values -- e.g. the name of identifiers, or the values of
// constants.
//
// However, in order to provide enhanced features -- e.g. precise error
// reporting -- it is needed to represent inside the AST also other information
// such as the range of the input file represented by a given AST node. Since
// these information are not critical to the compilation process -- e.g. you
// have to know input locations only in the case of errors  -- they are computed
// lazily.
//
// From the coding point of view, since this is a central structure we must aim
// at performance. In these cases, a standard trick is to inline as much as
// possible member functions. However, from the software engineering point of
// view, it would be good having an hierarchy of classes, each of them
// representing a specific AST. This would require using virtual calls, which
// represents a cost from both the space consumption and running time point of
// view. Fortunately, we can use some tricks to get abstraction without loosing
// performance.
//
// The first trick is about identifying the class of each node in the AST. The
// standard C++ mechanism can be easily replaced by the custom casting framework
// of LLVM. The second trick is about virtual calls. Let's consider the problem
// of getting the sub-trees of an AST: if the AST represents an internal node,
// then there maybe subtrees, but if the AST is actually a leaf, for sure there
// are not.
//
// To solve this problem a virtual call should be needed: each node type compute
// differently the set of subtrees. To avoid the usage of virtual calls, we must
// follow a standard technique:
//
// 1) all storage needed by all subclasses is declared in the root class
// 2) the root knowns about the structure of subclasses, thus it knows what is
//    actually stored inside its fields
// 3) the root class defines generic member functions to access to those fields
// 4) the subclasses does not declares any extra storage; they only defines
//    member functions to easily access data stored in the root class
//
// With respect to our previous example, we have that the AST root class
// declares a set of pointers to reference sub-trees. In the case the AST is
// actually an internal node, each pointer points to a different sub-tree. In
// the case it is a leaf, only one pointer is used and it references to
// corresponding token. Thus, getting the sub-trees of node is trivial.
class AbstractSyntaxTreeNode {
public:
  // Every parsed entity is represented by a different subclass of
  // AbstractSyntaxTreeNode. Every entity X is represented by class XAST.
  enum Id {
    // Special ASTs.
    Empty,

    // Braces and co.
    LBrace,
    RBrace,
    LSquare,
    RSquare,
    LPar,
    RPar,

    // Separators/terminators.
    SemiColon,
    Colon,
    Comma,
    Assign,

    // Algebraic operators.
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Relational operators.
    Less,
    LessOrEqual,
    Equal,
    NotEqual,
    GreaterOrEqual,
    Greater,

    // Bitwise operators.
    BAnd,
    BOr,
    BNot,
    LShift,
    RShift,

    // Logical operators.
    LAnd,
    LOr,

    // Keywords.
    If,
    Else,
    Do,
    While,
    Read,
    Write,

    // Typed.
    Number,
    Identifier,

    // Non-terminal rule ASTs: grammar root.
    Program,

    // Non-terminal rule ASTs: variable declarations.
    VarDeclarations,
    NonEmptyVarDeclarations,
    VarDeclaration,

    // Non-terminal rule ASTs: declarations.
    DeclarationList,
    Declaration,
    ScalarDeclaration,
    Initializer,

    // Special enum values.
    MinTokenId = LBrace,
    MaxTokenId = Identifier
  };

private:
  union NodeDataPtr {
    AbstractSyntaxTreeNode *AST;
    Token *Tok;
  };

  typedef llvm::SmallVector<NodeDataPtr, 4> NodeData;

public:
  // This iterator allows to access all sub-trees referenced by a node. Since in
  // our design, each AbstractSyntaxTreeNode can reference either a set of
  // AbstractSyntaxTreeNode or an Token, this iterator is in charge of
  // automatically access to the right object -- i.e. AbstractSyntaxTreeNode --
  // when dereferenced.
  //
  // Please notice that I have tried doing some black magic with inheritance and
  // templates, but I failed defining a base class for both iterator and
  // constant iterator -- it looks like that writing two separate classes was
  // the only viable solution.
  class iterator {
  public:
    typedef AbstractSyntaxTreeNode *value_type;
    typedef ptrdiff_t difference_type;

    typedef value_type *pointer;
    typedef value_type &reference;

    typedef std::forward_iterator_tag iterator_category;

  public:
    iterator() { }
    iterator(const iterator &That) : Cur(That.Cur) { }

    const iterator &operator=(const iterator &That) {
      if(this != &That)
        Cur = That.Cur;
      return *this;
    }

  private:
    iterator(const NodeData::iterator &I) : Cur(I) { }

  public:
    bool operator==(const iterator &That) const { return Cur == That.Cur; }
    bool operator!=(const iterator &That) const { return Cur != That.Cur; }

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

  public:
    reference operator*() const { return Cur->AST; }
    pointer operator->() const { return &Cur->AST; }

  private:
    NodeData::iterator Cur;

    friend class AbstractSyntaxTreeNode;

    // Since a const_iterator can be copy-constructed from an iterator, we must
    // grant him access to private fields in order to perform its
    // initialization.
    friend class const_iterator;
  };

  // A const_iterator behaves almost exactly as an iterator, but with 3 major
  // differences:
  //
  // 1) it does not allow modifying returned elements
  // 2) it cannot be used for modifying the associated container -- e.g. it
  //    cannot be used as are reference point for and insert operation
  // 3) it can be copy-constructed from an iterator of the same class
  //
  // Sometimes it is possible exploiting inheritance and/or templates in order
  // to factorize as much as possible code for both iterator and const_iterator.
  // I tried, but I failed writing a concise generic description for iterators
  // of this class.
  class const_iterator {
  public:
    typedef const AbstractSyntaxTreeNode *value_type;
    typedef ptrdiff_t difference_type;

    typedef value_type *pointer;
    typedef value_type &reference;

    typedef std::forward_iterator_tag iterator_category;

  public:
    const_iterator() { }
    const_iterator(const iterator &That) : Cur(That.Cur) { }
    const_iterator(const const_iterator &That) : Cur(That.Cur) { }

    const const_iterator &operator=(const iterator &That) {
      Cur = That.Cur;
      return *this;
    }

    const const_iterator &operator=(const const_iterator &That) {
      if(this != &That)
        Cur = That.Cur;
      return *this;
    }

  private:
    const_iterator(const NodeData::const_iterator &I) : Cur(I) { }

  public:
    bool operator==(const const_iterator &That) const {
      return Cur == That.Cur;
    }

    bool operator!=(const const_iterator &That) const {
      return Cur != That.Cur;
    }

  public:
    const_iterator &operator++() {
      ++Cur;
      return *this;
    }

    const_iterator operator++(int Ign) {
      const_iterator Prev = *this;
      ++*this;
      return Prev;
    }

  public:
    reference operator*() const { return const_cast<reference>(Cur->AST); }
    pointer operator->() const { return const_cast<pointer>(&Cur->AST); }

  private:
    NodeData::const_iterator Cur;

    friend class AbstractSyntaxTreeNode;
  };

public:
  iterator begin();
  iterator end();

  const_iterator begin() const;
  const_iterator end() const;

protected:
  // This constructor insert all non-null input trees inside the current tree.
  // Please notice that this is an "internal" constructor, so a null tree just
  // means that this particular node has not a subtree.
  //
  // For instance, suppose a subclass invokes this constructor in this way:
  //
  // AbstractSyntaxTreeNode(T0, 0)
  //
  // This simply means that the subclass represents a node with only one
  // sub-tree. From the implementation point of view, the following call is
  // equivalent:
  //
  // AbstractSyntaxTreeNode(0, T0)
  //
  // However, it is discouraged -- it is not very clear!
  AbstractSyntaxTreeNode(Id Identifier,
                         AbstractSyntaxTreeNode *T0 = 0,
                         AbstractSyntaxTreeNode *T1 = 0,
                         AbstractSyntaxTreeNode *T2 = 0)
    : MyIdentifier(Identifier) {
    NodeDataPtr Ptr;

    #define TREE_INSERT(T) \
    if(T) {                \
      Ptr.AST = T;         \
      Data.push_back(Ptr); \
    }

    TREE_INSERT(T0)
    TREE_INSERT(T1)
    TREE_INSERT(T2)

    #undef TREE_INSERT
  }

  // This constructor should be used to create leaf nodes.
  AbstractSyntaxTreeNode(Id Identifier, Token *T) : MyIdentifier(Identifier) {
    NodeDataPtr Ptr;

    Ptr.Tok = T;
    Data.push_back(Ptr);
  }

private:
  AbstractSyntaxTreeNode(const AbstractSyntaxTreeNode &That)
    LLVM_DELETED_FUNCTION;
  const AbstractSyntaxTreeNode &operator=(const AbstractSyntaxTreeNode &That)
    LLVM_DELETED_FUNCTION;

public:
  ~AbstractSyntaxTreeNode();

public:
  Id GetId() const { return MyIdentifier; }

  llvm::SMLoc GetStartLoc() const;
  llvm::SMLoc GetEndLoc() const;

protected:
  // This field should be protected because subclasses should access to their
  // storage -- remember that all storage used by all classes is managed by
  // this class only.
  NodeData Data;

private:
  Id MyIdentifier;

  // Locations in the input stream represented by this AST -- computed lazily.
  llvm::SMLoc StartLoc;
  llvm::SMLoc EndLoc;
};

llvm::raw_ostream &operator<<(llvm::raw_ostream &OS,
                              AbstractSyntaxTreeNode::Id Id);

class TokenAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return MinTokenId <= AST->GetId() && AST->GetId() <= MaxTokenId;
  }

protected:
  TokenAST(Id Identifier, Token *Tok)
    : AbstractSyntaxTreeNode(Identifier, Tok) { }
};

// We would like to represents all tokens inside the AST, so it is necessary
// generates a different class for each of them: this macro generates a very
// trivial class for a given token identifier.
#define TOKEN_AST(I) \
class I ## AST : public TokenAST {                                \
public:                                                           \
  static inline bool classof(const AbstractSyntaxTreeNode *AST) { \
    return AST->GetId() == I;                                     \
  }                                                               \
                                                                  \
public:                                                           \
  I ## AST(I ## Tok *Tok) : TokenAST(I, Tok) { }                  \
};

// From a design point of view, it would be better to define classes from the
// root class up to the leaves of the hierarchy. This allows to introduce first
// more abstract concepts and to refine them with the introduction of more
// concrete classes.
//
// Let's suppose we want to apply the same organization in the context of an
// AST: first we define the class related to the top of the grammar, then the
// classes that represents its expansion, then ...
//
// This process generates a file that cannot be compiled. For instance, consider
// classes representing the root of the grammar and its direct expansions:
//
// program: var_declarations statements
//
// program         <-> ProgramAST
// var_declaration <-> VarDeclarationsAST
// statements      <-> StatementsAST
//
// To build a ProgramAST we pass to its constructor a pointer to a
// VarDeclationsAST and another to a StatementsAST. The only way to make code
// compiling, is to forward-declare VarDeclarationsAST and StatementsAST.
//
// However, this approach is not effective. Indeed, inside ProgramAST can be
// necessary to call some VarDeclarationsAST of StatementAST specific member
// functions, which cannot be forward-declared -- inline methods should be
// defined after the definition of all classes.
//
// This turns out that a better solution is to declare all classes in a reverse
// order: first the referred, then the referencing: the amount of code that
// should be written is minimized.

//
// Special ASTs.
//

// Empty expansion of a non-terminal rule.
class EmptyAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == Empty;
  }

public:
  EmptyAST() : AbstractSyntaxTreeNode(Empty) { }
};

//
// Separators/terminators AST.
//

TOKEN_AST(SemiColon)
TOKEN_AST(Colon)
TOKEN_AST(Comma)
TOKEN_AST(Assign)

#undef TOKEN_AST

//
// Typed tokens ASTs.
//

class NumberAST : public TokenAST {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == Number;
  }

public:
  NumberAST(NumberTok *Tok) : TokenAST(Number, Tok) { }
};

class IdentifierAST : public TokenAST {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == Identifier;
  }

public:
  IdentifierAST(IdentifierTok *Tok) : TokenAST(Identifier, Tok) { }
};

//
// Non-terminal rule ASTs.
//

class StatementsAST : public AbstractSyntaxTreeNode { };

class InitializerAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == Initializer;
  }

public:
  InitializerAST(NumberAST *N) : AbstractSyntaxTreeNode(Number, N) { }
};

class ArrayDeclarationAST : public AbstractSyntaxTreeNode { };

// scalar_declaration: *Identifier*
//                   | *Identifier* *Assign* initializer
class ScalarDeclarationAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == ScalarDeclaration;
  }

public:
  ScalarDeclarationAST(IdentifierAST *Id)
    : AbstractSyntaxTreeNode(ScalarDeclaration, Id) { }

  ScalarDeclarationAST(IdentifierAST *Id,
                       AssignAST *Assign,
                       InitializerAST *Init)
    : AbstractSyntaxTreeNode(ScalarDeclaration, Id, Assign, Init) { }
};

// declaration: scalar_declaration
//            | array_declaration
class DeclarationAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == Declaration;
  }

public:
  DeclarationAST(ScalarDeclarationAST *Decl)
    : AbstractSyntaxTreeNode(Declaration, Decl) { }

  DeclarationAST(ArrayDeclarationAST *Decl)
    : AbstractSyntaxTreeNode(Declaration, Decl) { }
};

// declaration_list: declaration *Comma* declaration_list
//                 | declaration
class DeclarationListAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == DeclarationList;
  }

public:
  DeclarationListAST(DeclarationAST *Decl,
                     CommaAST *Comma,
                     DeclarationListAST *DeclList)
    : AbstractSyntaxTreeNode(DeclarationList, Decl, Comma, DeclList) { }
};

// var_declaration: *Identifier* declaration_list *SemiColon*
class VarDeclarationAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == VarDeclaration;
  }

public:
  VarDeclarationAST(IdentifierAST *Id,
                    DeclarationListAST *DeclList,
                    SemiColonAST *Semi)
    : AbstractSyntaxTreeNode(VarDeclaration, Id, DeclList, Semi) { }
};

// non_empty_var_declarations: var_declaration non_empty_var_declarations
//                           | var_declaration
class NonEmptyVarDeclarationsAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == NonEmptyVarDeclarations;
  }

public:
  NonEmptyVarDeclarationsAST(VarDeclarationAST *VarDecl,
                             NonEmptyVarDeclarationsAST *VarDecls = 0)
    : AbstractSyntaxTreeNode(NonEmptyVarDeclarations, VarDecl, VarDecls) { }
};

// var_declarations: non_empty_var_declarations
//                 | empty
class VarDeclarationsAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == VarDeclarations;
  }

public:
  VarDeclarationsAST(NonEmptyVarDeclarationsAST *VarDecls)
    : AbstractSyntaxTreeNode(VarDeclarations, VarDecls) { }

  VarDeclarationsAST(EmptyAST *Eps)
    : AbstractSyntaxTreeNode(VarDeclarations, Eps) { }
};

// program: var_declarations statements
class ProgramAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == Program;
  }

public:
  ProgramAST(VarDeclarationsAST *VarDecls, StatementsAST *Stmts)
    : AbstractSyntaxTreeNode(Program, VarDecls, Stmts) { }
};

} // End namespace acse.

// LLVM provides a nice graph-toolkit. Basically, everything resemble a graph
// can be manipulated by some template algorithms -- e.g. different kind of
// visits.
//
// In order to use these algorithms, we must tell to LLVM that our
// AbstractSyntaxTreeNode is actually a graph. This is done through the
// specialization of the GraphTraits template, which MUST OCCURS inside the llvm
// namespace.
namespace llvm {

using namespace acse;

template <>
struct GraphTraits<AbstractSyntaxTreeNode *> {
  typedef AbstractSyntaxTreeNode NodeType;
  typedef AbstractSyntaxTreeNode::iterator ChildIteratorType;

  static inline
  NodeType *getEntryNode(AbstractSyntaxTreeNode *AST) {
    return AST;
  }

  static inline
  ChildIteratorType child_begin(AbstractSyntaxTreeNode *AST) {
    return AST->begin();
  }

  static inline
  ChildIteratorType child_end(AbstractSyntaxTreeNode *AST) {
    return AST->end();
  }
};

template <>
struct GraphTraits<const AbstractSyntaxTreeNode *> {
  typedef const AbstractSyntaxTreeNode NodeType;
  typedef AbstractSyntaxTreeNode::const_iterator ChildIteratorType;

  static inline
  NodeType *getEntryNode(const AbstractSyntaxTreeNode *AST) {
    return AST;
  }

  static inline
  ChildIteratorType child_begin(const AbstractSyntaxTreeNode *AST) {
    return AST->begin();
  }

  static inline
  ChildIteratorType child_end(const AbstractSyntaxTreeNode *AST) {
    return AST->end();
  }
};

} // End namespace llvm.

// Some AbstractSyntaxTreeNode member functions requires GraphTraits, hence I
// had to write their inline implementations outside class definition.
namespace acse {

inline AbstractSyntaxTreeNode::iterator AbstractSyntaxTreeNode::begin() {
  return iterator(Data.begin());
}

inline AbstractSyntaxTreeNode::iterator AbstractSyntaxTreeNode::end() {
  // Leaf node: it has not any sub-tree, but since the same space used to hold
  // subtree pointers is also used to hold the payload of the leaf, we have to
  // adjust the end of the sub-tree sequence.
  if(llvm::isa<TokenAST>(this))
    return iterator(Data.begin());

  // Internal node, the end iterator is the real end of the underlying array.
  else
    return iterator(Data.end());
}

inline AbstractSyntaxTreeNode::const_iterator
AbstractSyntaxTreeNode::begin() const {
  return const_iterator(Data.begin());
}

inline AbstractSyntaxTreeNode::const_iterator
AbstractSyntaxTreeNode::end() const {
  // Leaf node: it has not any sub-tree, but since the same space used to hold
  // subtree pointers is also used to hold the payload of the leaf, we have to
  // adjust the end of the sub-tree sequence.
  if(llvm::isa<TokenAST>(this))
    return const_iterator(Data.begin());

  // Internal node, the end iterator is the real end of the underlying array.
  else
    return const_iterator(Data.end());
}

inline
AbstractSyntaxTreeNode::~AbstractSyntaxTreeNode() {
  // This node is actually a leaf of the AST, hence there are not nodes to free.
  // We just have to delete the corresponding token.
  if(llvm::isa<TokenAST>(this)) {
    NodeDataPtr Ptr = Data.front();
    delete Ptr.Tok;
  }

  // Visit the AST in depth-first, post-order mode. In this way, we can free the
  // memory used by a node at each step of the iteration -- after visiting the
  // current node, nobody will access it!
  //
  // Please notice that for leaves of the AST, the start and the end of the
  // sequence is the same, hence the loop is totally skipped.
  for(llvm::po_iterator<AbstractSyntaxTreeNode *> I = llvm::po_begin(this),
                                                  E = llvm::po_end(this);
                                                  I != E;
                                                  ++I) {
    // If the current node is an internal node, we have to delete all its
    // sub-trees, in order to avoid recursive destructor calls!
    if(!llvm::isa<TokenAST>(*I)) {
      NodeData &Data = I->Data;
      Data.clear();
    }

    // Free the current node only if it is different from the node we are
    // currently destroying.
    if(*I != this) delete *I;
  }
}

} // End namespace acse.

#endif // ACSE_LEX_ABSTRACTSYNTAXTREENODE_H
