//===- AbstractSyntaxTree.h - LANCE Abstract Syntax Tree --------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef ACSE_LEX_ABSTRACTSYNTAXTREE_H
#define ACSE_LEX_ABSTRACTSYNTAXTREE_H

#include "acse/Lex/Token.h"

#include "llvm/ADT/SmallVector.h"
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
class AbstractSyntaxTree {
public:
  // Every parsed entity is represented by a different subclass of
  // AbstractSyntaxTree. Every entity X is represented by class XAST.
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
    AbstractSyntaxTree *AST;
    Token *Tok;
  };

  typedef llvm::SmallVector<NodeDataPtr, 4> NodeData;

public:
  template <typename Ty, typename IterTy>
  class subtree_iterator_base
    : public std::iterator<std::bidirectional_iterator_tag, Ty> {
  public:
    subtree_iterator_base() { }

    subtree_iterator_base(const subtree_iterator_base &That) : Cur(That.Cur) { }

    const subtree_iterator_base &operator=(const subtree_iterator_base &That) {
      return Cur = That.Cur;
    }

  private:
    subtree_iterator_base(const IterTy &I) : Cur(I) { }

  public:
    bool operator==(const subtree_iterator_base &That) const {
      return Cur == That.Cur;
    }

    bool operator!=(const subtree_iterator_base &That) const {
      return Cur != That.Cur;
    }

  public:
    subtree_iterator_base &operator++() {
      ++Cur;
      return *this;
    }

    subtree_iterator_base operator++(int Ign) {
      subtree_iterator Old = *this;
      ++*this;
      return *Old;
    }

    subtree_iterator_base &operator--() {
      --Cur;
      return *this;
    }

    subtree_iterator_base operator--(int Ign) {
      subtree_iterator Old = *this;
      --*this;
      return Old;
    }

  public:
    Ty &operator*() const {
      return *Cur->AST;
    }

    Ty *operator->() const {
      return Cur->AST;
    }

  private:
    IterTy Cur;

    friend class AbstractSyntaxTree;
  };

  typedef subtree_iterator_base<AbstractSyntaxTree,
                                NodeData::iterator>
          subtree_iterator;
  typedef subtree_iterator_base<const AbstractSyntaxTree,
                                NodeData::const_iterator>
          const_subtree_iterator;

public:
  subtree_iterator subtree_begin();
  subtree_iterator subtree_end();

  const_subtree_iterator subtree_begin() const;
  const_subtree_iterator subtree_end() const;

protected:
  // This constructor insert all non-null input trees inside the current tree.
  // Please notice that this is an "internal" constructor, so a null tree just
  // means that this particular node has not a subtree.
  //
  // For instance, suppose a subclass invokes this constructor in this way:
  //
  // AbstractSyntaxTree(T0, 0)
  //
  // This simply means that the subclass represents a node with only one
  // sub-tree. From the implementation point of view, the following call is
  // equivalent:
  //
  // AbstractSyntaxTree(0, T0)
  //
  // However, it is discouraged -- it is not very clear!
  AbstractSyntaxTree(Id Identifier,
                     AbstractSyntaxTree *T0 = 0,
                     AbstractSyntaxTree *T1 = 0,
                     AbstractSyntaxTree *T2 = 0)
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
  AbstractSyntaxTree(Id Identifier, Token *T) : MyIdentifier(Identifier) {
    NodeDataPtr Ptr;

    Ptr.Tok = T;
    Data.push_back(Ptr);
  }

private:
  AbstractSyntaxTree(const AbstractSyntaxTree &That)
    LLVM_DELETED_FUNCTION;
  const AbstractSyntaxTree &operator=(const AbstractSyntaxTree &That)
    LLVM_DELETED_FUNCTION;

public:
  ~AbstractSyntaxTree();

public:
  Id GetId() const { return MyIdentifier; }

  llvm::SMLoc GetStartLoc() const;
  llvm::SMLoc GetEndLoc() const;

public:
  void Dump(llvm::raw_ostream &OS = llvm::errs()) const;

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

class TokenAST : public AbstractSyntaxTree {
public:
  static inline bool classof(const AbstractSyntaxTree *AST) {
    return MinTokenId <= AST->GetId() && AST->GetId() <= MaxTokenId;
  }

protected:
  TokenAST(Id Identifier, Token *Tok) : AbstractSyntaxTree(Identifier, Tok) { }
};

// We would like to represents all tokens inside the AST, so it is necessary
// generates a different class for each of them: this macro generates a very
// trivial class for a given token identifier.
#define TOKEN_AST(I) \
class I ## AST : public TokenAST {                            \
public:                                                       \
  static inline bool classof(const AbstractSyntaxTree *AST) { \
    return AST->GetId() == I;                                 \
  }                                                           \
                                                              \
public:                                                       \
  I ## AST(I ## Tok *Tok) : TokenAST(I, Tok) { }              \
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
class EmptyAST : public AbstractSyntaxTree {
public:
  static inline bool classof(const AbstractSyntaxTree *AST) {
    return AST->GetId() == Empty;
  }

public:
  EmptyAST() : AbstractSyntaxTree(Empty) { }
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
  static inline bool classof(const AbstractSyntaxTree *AST) {
    return AST->GetId() == Number;
  }

public:
  NumberAST(NumberTok *Tok) : TokenAST(Number, Tok) { }
};

class IdentifierAST : public TokenAST {
public:
  static inline bool classof(const AbstractSyntaxTree *AST) {
    return AST->GetId() == Identifier;
  }

public:
  IdentifierAST(IdentifierTok *Tok) : TokenAST(Identifier, Tok) { }
};

//
// Non-terminal rule ASTs.
//

class StatementsAST : public AbstractSyntaxTree { };

class InitializerAST : public AbstractSyntaxTree {
public:
  static inline bool classof(AbstractSyntaxTree *AST) {
    return AST->GetId() == Initializer;
  }

public:
  InitializerAST(NumberAST *N) : AbstractSyntaxTree(Number, N) { }
};

class ArrayDeclarationAST : public AbstractSyntaxTree { };

// scalar_declaration: *Identifier*
//                   | *Identifier* *Assign* initializer
class ScalarDeclarationAST : public AbstractSyntaxTree {
public:
  static inline bool classof(AbstractSyntaxTree *AST) {
    return AST->GetId() == ScalarDeclaration;
  }

public:
  ScalarDeclarationAST(IdentifierAST *Id)
    : AbstractSyntaxTree(ScalarDeclaration, Id) { }

  ScalarDeclarationAST(IdentifierAST *Id,
                       AssignAST *Assign,
                       InitializerAST *Init)
    : AbstractSyntaxTree(ScalarDeclaration, Id, Assign, Init) { }
};

// declaration: scalar_declaration
//            | array_declaration
class DeclarationAST : public AbstractSyntaxTree {
public:
  static inline bool classof(AbstractSyntaxTree *AST) {
    return AST->GetId() == Declaration;
  }

public:
  DeclarationAST(ScalarDeclarationAST *Decl)
    : AbstractSyntaxTree(Declaration, Decl) { }

  DeclarationAST(ArrayDeclarationAST *Decl)
    : AbstractSyntaxTree(Declaration, Decl) { }
};

// declaration_list: declaration *Comma* declaration_list
//                 | declaration
class DeclarationListAST : public AbstractSyntaxTree {
public:
  static inline bool classof(AbstractSyntaxTree *AST) {
    return AST->GetId() == DeclarationList;
  }

public:
  DeclarationListAST(DeclarationAST *Decl,
                     CommaAST *Comma,
                     DeclarationListAST *DeclList)
    : AbstractSyntaxTree(DeclarationList, Decl, Comma, DeclList) { }
};

// var_declaration: *Identifier* declaration_list *SemiColon*
class VarDeclarationAST : public AbstractSyntaxTree {
public:
  static inline bool classof(const AbstractSyntaxTree *AST) {
    return AST->GetId() == VarDeclaration;
  }

public:
  VarDeclarationAST(IdentifierAST *Id,
                    DeclarationListAST *DeclList,
                    SemiColonAST *Semi)
    : AbstractSyntaxTree(VarDeclaration, Id, DeclList, Semi) { }
};

// non_empty_var_declarations: var_declaration non_empty_var_declarations
//                           | var_declaration
class NonEmptyVarDeclarationsAST : public AbstractSyntaxTree {
public:
  static inline bool classof(const AbstractSyntaxTree *AST) {
    return AST->GetId() == NonEmptyVarDeclarations;
  }

public:
  NonEmptyVarDeclarationsAST(VarDeclarationAST *VarDecl,
                             NonEmptyVarDeclarationsAST *VarDecls = 0)
    : AbstractSyntaxTree(NonEmptyVarDeclarations, VarDecl, VarDecls) { }
};

// var_declarations: non_empty_var_declarations
//                 | empty
class VarDeclarationsAST : public AbstractSyntaxTree {
public:
  static inline bool classof(const AbstractSyntaxTree *AST) {
    return AST->GetId() == VarDeclarations;
  }

public:
  VarDeclarationsAST(NonEmptyVarDeclarationsAST *VarDecls)
    : AbstractSyntaxTree(VarDeclarations, VarDecls) { }

  VarDeclarationsAST(EmptyAST *Eps)
    : AbstractSyntaxTree(VarDeclarations, Eps) { }
};

// program: var_declarations statements
class ProgramAST : public AbstractSyntaxTree {
public:
  static inline bool classof(const AbstractSyntaxTree *AST) {
    return AST->GetId() == Program;
  }

public:
  ProgramAST(VarDeclarationsAST *VarDecls, StatementsAST *Stmts)
    : AbstractSyntaxTree(Program, VarDecls, Stmts) { }
};

//
// Inline methods that cannot be implemented at declaration time.
//

inline AbstractSyntaxTree::subtree_iterator
AbstractSyntaxTree::subtree_begin() {
  return subtree_iterator(Data.begin());
}

inline AbstractSyntaxTree::subtree_iterator
AbstractSyntaxTree::subtree_end() {
  // Leaf node: it has not any sub-tree, but since the same space used to hold
  // subtree pointers is also used to hold the payload of the leaf, we have to
  // adjust the end of the sub-tree sequence.
  if(llvm::isa<TokenAST>(this))
    return subtree_iterator(Data.begin());

  // Internal node, the end iterator is the real end of the underlying array.
  else
    return subtree_iterator(Data.end());
}

inline AbstractSyntaxTree::const_subtree_iterator
AbstractSyntaxTree::subtree_begin() const {
  return const_subtree_iterator(Data.begin());
}

inline AbstractSyntaxTree::const_subtree_iterator
AbstractSyntaxTree::subtree_end() const {
  // Leaf node: it has not any sub-tree, but since the same space used to hold
  // subtree pointers is also used to hold the payload of the leaf, we have to
  // adjust the end of the sub-tree sequence.
  if(llvm::isa<TokenAST>(this))
    return const_subtree_iterator(Data.begin());

  // Internal node, the end iterator is the real end of the underlying array.
  else
    return const_subtree_iterator(Data.end());
}

// TODO: switch to an iterative algorithm.
inline AbstractSyntaxTree::~AbstractSyntaxTree() {
  NodeData::iterator I = Data.begin();

  // This class provides the storage for all subclasses, thus it is the only
  // that can free node resources. In order to do that, we need to check which
  // is the actual type of the node and free the correct set of resources.
  //
  // Sometimes, you still need to use old C-like tricks in C++.
  if(llvm::isa<TokenAST>(this)) {
    delete I->Tok;
  } else for(NodeData::iterator E = Data.end(); I != E; ++I) {
    delete I->AST;
  }
}

} // End namespace acse.

#endif // ACSE_LEX_ABSTRACTSYNTAXTREE_H
