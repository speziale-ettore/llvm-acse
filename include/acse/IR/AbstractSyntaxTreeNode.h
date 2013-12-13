//===- AbstractSyntaxTreeNode.h - LANCE AST Internal Nodes ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef ACSE_IR_ABSTRACTSYNTAXTREENODE_H
#define ACSE_IR_ABSTRACTSYNTAXTREENODE_H

#include "acse/Lex/Token.h"

#include "llvm/ADT/GraphTraits.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"

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
  enum Id {
    // Every parsed entity is represented by a different subclass of
    // AbstractSyntaxTreeNode. Every entity X is represented by class XAST.
    #define AST(I) \
    I,
    #include "acse/IR/AbstractSyntaxTreeNode.def"
    #undef AST

    // Special enum values.
    MinTokenId = LineComment,
    MaxTokenId = Identifier,

    // Special enum values for expressions.
    MinExprId = AddExpr,
    MaxExprId = NumberExpr,
    MinUnaryExprId = BNotExpr,
    MaxUnaryExprId = MinusExpr,
    MinBinaryExprId = AddExpr,
    MaxBinaryExprId = LOrExpr
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

  size_t size() const;
  bool empty() const;

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
                         AbstractSyntaxTreeNode *T2 = 0,
                         AbstractSyntaxTreeNode *T3 = 0,
                         AbstractSyntaxTreeNode *T4 = 0,
                         AbstractSyntaxTreeNode *T5 = 0,
                         AbstractSyntaxTreeNode *T6 = 0)
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
    TREE_INSERT(T3)
    TREE_INSERT(T4)
    TREE_INSERT(T5)
    TREE_INSERT(T6)

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

public:
  bool IsLeftmostChild(const AbstractSyntaxTreeNode *That) const {
    if(empty())
      return false;

    NodeDataPtr Leftmost = Data.front();

    return Leftmost.AST == That;
  }

  bool IsRightmostChild(const AbstractSyntaxTreeNode *That) const {
    if(empty())
      return false;

    NodeDataPtr Rightmost = Data.back();

    return Rightmost.AST == That;
  }

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
#define TOKEN_AST(I)                                              \
class I ## AST : public TokenAST {                                \
public:                                                           \
  typedef I ## Tok Token;                                         \
                                                                  \
public:                                                           \
  static inline bool classof(const AbstractSyntaxTreeNode *AST) { \
    return AST->GetId() == I;                                     \
  }                                                               \
                                                                  \
public:                                                           \
  I ## AST(I ## Tok *Tok) : TokenAST(I, Tok) { }                  \
};

// This class is roughly the equivalent of the 'expression' non-terminal in the
// grammar. Please notice that this class is abstract. The constructors it
// provides are used by subclasses -- one for each type of expression -- to
// initialize the expression tree.
class ExpressionAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return MinExprId <= AST->GetId() && AST->GetId() <= MaxExprId;
  }

protected:
  // Constructor used for very simple expressions, which do not have and
  // operator and consist of only one operand -- e.g. some primary expressions.
  ExpressionAST(Id Identifier,
                AbstractSyntaxTreeNode *Operand)
    : AbstractSyntaxTreeNode(Identifier, Operand)
      { }

  // Constructor used by unary expressions, made up of an operator followed by
  // an operand.
  ExpressionAST(Id Identifier,
                TokenAST *Operator,
                AbstractSyntaxTreeNode *Operand)
    : AbstractSyntaxTreeNode(Identifier, Operator, Operand)
      { }

  // Constructor for binary infix expressions. They are made up by an operand --
  // e.g. '+' -- surrounded by two operators.
  ExpressionAST(Id Identifier,
                AbstractSyntaxTreeNode *LeftOperand,
                TokenAST *Operator,
                AbstractSyntaxTreeNode *RightOperand)
    : AbstractSyntaxTreeNode(Identifier, LeftOperand, Operator, RightOperand)
      { }

  // Specialized constructor for nested expressions.
  ExpressionAST(Id Identifier,
                TokenAST *Start,
                AbstractSyntaxTreeNode *Operand,
                TokenAST *End)
    : AbstractSyntaxTreeNode(Identifier, Start, Operand, End)
      { }

  // Another specialized constructor. This one is for array expressions.
  ExpressionAST(Id Identifier,
                AbstractSyntaxTreeNode *LeftOperand,
                TokenAST *Start,
                AbstractSyntaxTreeNode *RightOperand,
                TokenAST *End)
    : AbstractSyntaxTreeNode(Identifier, LeftOperand, Start, RightOperand, End)
      { }
};

// An unary expression is made up of an operator followed by an operand. This is
// a simple tag class that should define some useful accessors.
class UnaryExprAST : public ExpressionAST {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return MinUnaryExprId <= AST->GetId() && AST->GetId() <= MaxUnaryExprId;
  }

protected:
  UnaryExprAST(Id Identifier, TokenAST *Operator, ExpressionAST *Operand)
    : ExpressionAST(Identifier, Operator, Operand) { }
};

// A binary expression is made up of an operator surrounded by two operands.
// This is a simple tag class that should define some useful accessors.
class BinaryExprAST : public ExpressionAST {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return MaxBinaryExprId <= AST->GetId() && AST->GetId() <= MaxBinaryExprId;
  }

protected:
  BinaryExprAST(Id Identifier,
                ExpressionAST *LeftOperand,
                TokenAST *Operator,
                ExpressionAST *RightOperand)
    : ExpressionAST(Identifier, LeftOperand, Operator, RightOperand) { }
};

// While the operations available for all binary expressions are more or less
// the same, it is still useful representing each of them by means of a separate
// class. This will allow a visitor to match only the kind of expressions it is
// interested on.
//
// This macro allows to declare a class for a generic binary operator.
#define BINARY_EXPR_AST(I)                                              \
class I ## ExprAST : public BinaryExprAST {                             \
public:                                                                 \
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {       \
    return AST->GetId() == I ## Expr;                                   \
  }                                                                     \
                                                                        \
public:                                                                 \
  I ## ExprAST(ExpressionAST *LeftOperand,                              \
               I ## AST *Operator,                                      \
               ExpressionAST *RightOperand)                             \
    : BinaryExprAST(I ## Expr, LeftOperand, Operator, RightOperand) { } \
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

// The exception to the aforementioned rules are the following two classes. They
// need to be forwarded declared in order to be used by CodeBlockAST.
class StatementsAST;
class StatementAST;

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
// Comments AST.
//

class LineCommentAST : public TokenAST {
public:
  typedef LineCommentTok Token;

public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == LineComment;
  }

public:
  LineCommentAST(LineCommentTok *Tok) : TokenAST(LineComment, Tok) { }
};

class MultiLineCommentAST : public TokenAST {
public:
  typedef MultiLineCommentTok Token;

public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == MultiLineComment;
  }

public:
  MultiLineCommentAST(MultiLineCommentTok *Tok)
    : TokenAST(MultiLineComment, Tok) { }
};

//
// Braces and co. AST.
//

TOKEN_AST(LBrace)
TOKEN_AST(RBrace)
TOKEN_AST(LSquare)
TOKEN_AST(RSquare)
TOKEN_AST(LPar)
TOKEN_AST(RPar)

//
// Separators/terminators AST.
//

TOKEN_AST(SemiColon)
TOKEN_AST(Colon)
TOKEN_AST(Comma)
TOKEN_AST(Assign)

//
// Algebraic operators AST.
//

TOKEN_AST(Add)
TOKEN_AST(Sub)
TOKEN_AST(Mul)
TOKEN_AST(Div)
TOKEN_AST(Mod)

//
// Relational operators AST.
//

TOKEN_AST(Less)
TOKEN_AST(LessOrEqual)
TOKEN_AST(Equal)
TOKEN_AST(NotEqual)
TOKEN_AST(GreaterOrEqual)
TOKEN_AST(Greater)

//
// Bitwise operators AST.
//

TOKEN_AST(BAnd)
TOKEN_AST(BOr)
TOKEN_AST(BNot)
TOKEN_AST(LShift)
TOKEN_AST(RShift)

//
// Logical operators AST.
//

TOKEN_AST(LAnd)
TOKEN_AST(LOr)

//
// Keyword AST.
//

TOKEN_AST(Int)
TOKEN_AST(If)
TOKEN_AST(Else)
TOKEN_AST(Do)
TOKEN_AST(While)
TOKEN_AST(Return)
TOKEN_AST(Read)
TOKEN_AST(Write)

#undef TOKEN_AST

//
// Typed tokens ASTs.
//

class NumberAST : public TokenAST {
public:
  typedef NumberTok Token;
  typedef NumberTok::NumberTy NumberTy;

public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == Number;
  }

public:
  NumberAST(NumberTok *Tok) : TokenAST(Number, Tok) { }

public:
  NumberTy GetValue() const {
    const NumberTok *Tok = llvm::cast<NumberTok>(Data[0].Tok);
    return Tok->Value();
  }
};

class IdentifierAST : public TokenAST {
public:
  typedef IdentifierTok Token;

public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == Identifier;
  }

public:
  IdentifierAST(IdentifierTok *Tok) : TokenAST(Identifier, Tok) { }

public:
  const IdentifierTok *GetToken() const {
    return llvm::cast<IdentifierTok>(Data[0].Tok);
  }

  llvm::StringRef GetName() const {
    const IdentifierTok *Tok = GetToken();
    return Tok->GetSpelling();
  }
};

//
// Non-terminal rule ASTs.
//

// Expressions parsing is one of the most critical section of a compiler
// frontend. Indeed, parsing must be extremely fast -- most of the parsed code
// will be expressions -- and we need to provide a good interface for AST
// navigation. This two requirements do not allow to keep the ACSE invariant to
// have 1 AST class for each grammar rule.
//
// Due to the optimization constraints, the section of the grammar related to
// binary operators is ambiguous by design. Ambiguity is solved by the parsing
// algorithm which uses operator precedences to figure out which rule to expand
// or reduce. This lead to using only the 'expression' non-terminal in the
// binary operator grammar.
//
// On the other hand, having only the ExpressionAST class in the AST is a poor
// design choice because we cannot easily detect the type of an expression.
//
// In order to provide a consistent usable interface, the 1:1 rule to AST class
// invariant is broken here. The static type of every expression handled by the
// parser will be ExpressionAST, while their dynamic type will reflect the real
// expression type.
//
// Please notice that a similar problem arises with primary expressions. They
// are expressions, hence the parser handle ExpressionAST, but in order to
// disambiguate between different primary expressions at AST navigation time,
// each primary expression is represented with a different AST class.

class NestedExprAST : public ExpressionAST {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == NestedExpr;
  }

public:
  NestedExprAST(LParAST *Open, ExpressionAST *Expr, RParAST *Close)
    : ExpressionAST(NestedExpr, Open, Expr, Close) { }
};

class BNotExprAST : public UnaryExprAST {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == BNotExpr;
  }

public:
  BNotExprAST(BNotAST *Operand, ExpressionAST *Operator)
    : UnaryExprAST(BNotExpr, Operand, Operator) { }
};

class MinusExprAST : public UnaryExprAST {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == MinusExpr;
  }

public:
  MinusExprAST(SubAST *Operand, ExpressionAST *Operator)
    : UnaryExprAST(MinusExpr, Operand, Operator) { }
};

class ScalarIdentifierExprAST : public ExpressionAST {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == ScalarIdentifierExpr;
  }

public:
  ScalarIdentifierExprAST(IdentifierAST *Id)
    : ExpressionAST(ScalarIdentifierExpr, Id) { }
};

class ArrayIdentifierExprAST : public ExpressionAST {
public:
  static bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == ArrayIdentifierExpr;
  }

public:
  ArrayIdentifierExprAST(IdentifierAST *Id,
                         LSquareAST *LSquare,
                         ExpressionAST *Subscript,
                         RSquareAST *RSquare)
    : ExpressionAST(ArrayIdentifierExpr, Id, LSquare, Subscript, RSquare) { }
};

class NumberExprAST : public ExpressionAST {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == NumberExpr;
  }

public:
  NumberExprAST(NumberAST *N) : ExpressionAST(NumberExpr, N) { }
};

// Algebraic expressions.
BINARY_EXPR_AST(Add)
BINARY_EXPR_AST(Sub)
BINARY_EXPR_AST(Mul)
BINARY_EXPR_AST(Div)
BINARY_EXPR_AST(Mod)

// Relational expressions.
BINARY_EXPR_AST(Less)
BINARY_EXPR_AST(LessOrEqual)
BINARY_EXPR_AST(Equal)
BINARY_EXPR_AST(NotEqual)
BINARY_EXPR_AST(GreaterOrEqual)
BINARY_EXPR_AST(Greater)

// Bitwise expressions.
BINARY_EXPR_AST(BAnd)
BINARY_EXPR_AST(BOr)
BINARY_EXPR_AST(LShift)
BINARY_EXPR_AST(RShift)

// Logical expressions.
BINARY_EXPR_AST(LAnd)
BINARY_EXPR_AST(LOr)

#undef BINARY_EXPR_AST

// code_block
//   : *LBrace* statements *RBrace*
//   | statement
class CodeBlockAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == CodeBlock;
  }

public:
  CodeBlockAST(LBraceAST *LBrace, StatementsAST *Stmts, RBraceAST *RBrace);
  CodeBlockAST(StatementAST *Stmt);
};

// scalar_assignment
//   : *Identifier* *Assign* expression
class ScalarAssignmentAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == ScalarAssignment;
  }

  ScalarAssignmentAST(IdentifierAST *LHS,
                      AssignAST *Assign,
                      ExpressionAST *RHS)
    : AbstractSyntaxTreeNode(ScalarAssignment, LHS, Assign, RHS) { }
};

// array_assignment
//   : *Identifier* *LSquare* expression *RSquare* *Assign* expression
class ArrayAssignmentAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == ArrayAssignment;
  }

public:
  ArrayAssignmentAST(IdentifierAST *Id,
                     LSquareAST *LSquare,
                     ExpressionAST *Subscript,
                     RSquareAST *RSquare,
                     AssignAST *Assign,
                     ExpressionAST *RHS)
    : AbstractSyntaxTreeNode(ArrayAssignment,
                             LSquare,
                             Subscript,
                             RSquare,
                             Assign,
                             RHS) { }
};

// assign_statement
//   : scalar_assignment
//   | array_assignment
class AssignStatementAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == AssignStatement;
  }

public:
  AssignStatementAST(ScalarAssignmentAST *Assign)
    : AbstractSyntaxTreeNode(AssignStatement, Assign) { }

  AssignStatementAST(ArrayAssignmentAST *Assign)
    : AbstractSyntaxTreeNode(AssignStatement, Assign) { }
};

// read_statement
//   : *Read* *LPar* *Identifier* *RPar*
class ReadStatementAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == ReadStatement;
  }

public:
  ReadStatementAST(ReadAST *Read,
                   LParAST *LPar,
                   IdentifierAST *Id,
                   RParAST *RPar)
    : AbstractSyntaxTreeNode(ReadStatement, Read, LPar, Id, RPar) { }
};

// write_statement
//   : *Write* *LPar* expression *RPar*
class WriteStatementAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == WriteStatement;
  }

public:
  WriteStatementAST(WriteAST *Write,
                    LParAST *LPar,
                    ExpressionAST *Expr,
                    RParAST *RPar)
    : AbstractSyntaxTreeNode(WriteStatement, Write, LPar, Expr, RPar) { }
};

// read_write_statement
//   : read_statement
//   | write_statement
class ReadWriteStatementAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == ReadWriteStatement;
  }

public:
  ReadWriteStatementAST(ReadStatementAST *Read)
    : AbstractSyntaxTreeNode( ReadWriteStatement, Read ) { }

  ReadWriteStatementAST(WriteStatementAST *Write)
    : AbstractSyntaxTreeNode( ReadWriteStatement, Write ) { }
};

// null_statement
//   : empty
class NullStatementAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == NullStatement;
  }

public:
  NullStatementAST(EmptyAST *Eps)
    : AbstractSyntaxTreeNode(NullStatement, Eps) { }
};

// if_statement
//   : *If* *LPar* expression *RPar* code_block
//   | *If* *LPar* expression *RPar* code_block *Else* code_block
class IfStatementAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == IfStatement;
  }

public:
  IfStatementAST(IfAST *If,
                 LParAST *LPar,
                 ExpressionAST *Expr,
                 RParAST *RPar,
                 CodeBlockAST *Taken,
                 ElseAST *Else = 0,
                 CodeBlockAST *NotTaken = 0)
    : AbstractSyntaxTreeNode(IfStatement,
                             If,
                             LPar,
                             Expr,
                             RPar,
                             Taken,
                             Else,
                             NotTaken) {
    assert((Else && NotTaken || !Else && !NotTaken) && "Missing elements");
  }
};

// while_statement
//   : *While* *LPar* expression *RPar* code_block
class WhileStatementAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == WhileStatement;
  }

public:
  WhileStatementAST(WhileAST *While,
                    LParAST *LPar,
                    ExpressionAST *Expr,
                    RParAST *RPar,
                    CodeBlockAST *Body)
    : AbstractSyntaxTreeNode(WhileStatement, LPar, Expr, RPar, Body) { }
};

// do_while_statement
//   : *Do* code_block *While* *RPar* expression *RPar* *SemiColon*
class DoWhileStatementAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == DoWhileStatement;
  }

public:
  DoWhileStatementAST(DoAST *Do,
                      CodeBlockAST *Body,
                      WhileAST *While,
                      LParAST *LPar,
                      ExpressionAST *Expr,
                      RParAST *RPar,
                      SemiColonAST *Semi)
    : AbstractSyntaxTreeNode(DoWhileStatement,
                             Body,
                             While,
                             LPar,
                             Expr,
                             RPar,
                             Semi) { }
};

// return_statement
//   : *Return* *SemiColon*
class ReturnStatementAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == ReturnStatement;
  }

public:
  ReturnStatementAST(ReturnAST *Return, SemiColonAST *Semi)
    : AbstractSyntaxTreeNode(ReturnStatement, Return, Semi) { }
};

// control_statement
//   : if_statement
//   | while_statement
//   | do_statement
//   | return_statement
class ControlStatementAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == ControlStatement;
  }

public:
  ControlStatementAST(IfStatementAST *If)
    : AbstractSyntaxTreeNode(ControlStatement, If) { }

  ControlStatementAST(WhileStatementAST *While)
    : AbstractSyntaxTreeNode(ControlStatement, While) { }

  ControlStatementAST(DoWhileStatementAST *DoWhile)
    : AbstractSyntaxTreeNode(ControlStatement, DoWhile) { }

  ControlStatementAST(ReturnStatementAST *Return)
    : AbstractSyntaxTreeNode(ControlStatement, Return) { }
};

// statement
//   : assign_statement *SemiColon*
//   | read_write_statement *SemiColon*
//   | null_statement *SemiColon*
//   | control_statement
class StatementAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == Statement;
  }

public:
  StatementAST(AssignStatementAST *Stmt, SemiColonAST *Semi)
    : AbstractSyntaxTreeNode(Statement, Stmt, Semi) { }

  StatementAST(ReadWriteStatementAST *Stmt, SemiColonAST *Semi)
    : AbstractSyntaxTreeNode(Statement, Stmt, Semi) { }

  StatementAST(NullStatementAST *Stmt, SemiColonAST *Semi)
    : AbstractSyntaxTreeNode(Statement, Stmt, Semi) { }

  StatementAST(ControlStatementAST *Stmt)
    : AbstractSyntaxTreeNode(Statement, Stmt) { }
};

// non_empty_statements
//   : statement statements
//   | statement
class NonEmptyStatementsAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == NonEmptyStatements;
  }

public:
  NonEmptyStatementsAST(StatementAST *Stmt, NonEmptyStatementsAST *Stmts = 0)
    : AbstractSyntaxTreeNode(NonEmptyStatements, Stmt, Stmts) { }
};

// statements
//   : non_empty_statements
//   | empty
class StatementsAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == Statements;
  }

public:
  StatementsAST(NonEmptyStatementsAST *Stmts)
    : AbstractSyntaxTreeNode(Statements, Stmts) { }

  StatementsAST(EmptyAST *Eps)
    : AbstractSyntaxTreeNode(Statements, Eps) { }
};

// initializer
//   : *Number*
class InitializerAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == Initializer;
  }

public:
  InitializerAST(NumberAST *N) : AbstractSyntaxTreeNode(Initializer, N) { }
};

// initializer_list
//   : initializer *Comma* initializer_list
//   | initializer
class InitializerListAST : public AbstractSyntaxTreeNode {
public:
  typedef NumberAST::NumberTy SizeTy;

public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == InitializerList;
  }

public:
  InitializerListAST(InitializerAST *Init,
                     CommaAST *Comma = 0,
                     InitializerListAST *InitList = 0)
    : AbstractSyntaxTreeNode(InitializerList, Init, Comma, InitList) {
    assert((Comma && InitList || !Comma && !InitList) && "Missing elements");
  }

public:
  const InitializerAST *GetInitializer() const {
    return llvm::cast<InitializerAST>(Data[0].AST);
  }

  const InitializerListAST *GetInitializerList() const {
    return Data.size() == 3 ? llvm::cast<InitializerListAST>(Data[2].AST) : 0;
  }

  bool HasInitializerList() const {
    return Data.size() != 1;
  }

  SizeTy GetInitializerSize() const;
};

// scalar_initializer
//   : initializer
class ScalarInitializerAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == ScalarInitializer;
  }

public:
  ScalarInitializerAST(InitializerAST *Init)
    : AbstractSyntaxTreeNode(ScalarInitializer, Init) { }
};

// array_initializer
//   : *LBrace* initializer_list *RBrace*
class ArrayInitializerAST : public AbstractSyntaxTreeNode {
public:
  typedef InitializerListAST::SizeTy SizeTy;

public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == ArrayInitializer;
  }

public:
  ArrayInitializerAST(LBraceAST *Open,
                      InitializerListAST *List,
                      RBraceAST *Closed)
    : AbstractSyntaxTreeNode(ArrayInitializer, Open, List, Closed) { }

public:
  SizeTy GetInitializerSize() const {
    const InitializerListAST *List;
    List = llvm::cast<InitializerListAST>(Data[1].AST);

    return List->GetInitializerSize();
  }
};

// type
//   : *Int*
class TypeAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == Type;
  }

public:
  TypeAST(IntAST *Int) : AbstractSyntaxTreeNode(Type, Int) { }
};

// array_declaration
//   : *Identifier* *LSquare* *Number* *RSquare*
//   | *Identifier* *LSquare* *Number* *RSquare* *Assign* initializer_list
//   | *Identifier* *LSquare* *RSquare* *Assign* initializer_list
class ArrayDeclarationAST : public AbstractSyntaxTreeNode {
public:
  typedef NumberAST::NumberTy ArraySizeTy;

public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == ArrayDeclaration;
  }

public:
  ArrayDeclarationAST(IdentifierAST *Id,
                      LSquareAST *LSquare,
                      NumberAST *Size,
                      RSquareAST *RSquare)
    : AbstractSyntaxTreeNode(ArrayDeclaration,
                             Id,
                             LSquare,
                             Size,
                             RSquare) { }

  ArrayDeclarationAST(IdentifierAST *Id,
                      LSquareAST *LSquare,
                      NumberAST *Size,
                      RSquareAST *RSquare,
                      AssignAST *Assign,
                      ArrayInitializerAST *Init)
    : AbstractSyntaxTreeNode(ArrayDeclaration,
                             Id,
                             LSquare,
                             Size,
                             RSquare,
                             Assign,
                             Init) { }

  ArrayDeclarationAST(IdentifierAST *Id,
                      LSquareAST *LSquare,
                      RSquareAST *RSquare,
                      AssignAST *Assign,
                      ArrayInitializerAST *Init)
    : AbstractSyntaxTreeNode(ArrayDeclaration,
                             Id,
                             LSquare,
                             RSquare,
                             Assign,
                             Init) { }

public:
  const IdentifierAST *GetIdentifier() const {
    return llvm::cast<IdentifierAST>(Data[0].AST);
  }

  ArraySizeTy GetArraySize() const {
    if(const NumberAST *Size = llvm::dyn_cast<NumberAST>(Data[2].AST))
      return Size->GetValue();

    else if(const ArrayInitializerAST *Initializer = GetInitializer())
      return Initializer->GetInitializerSize();

    else
      llvm_unreachable("Corrupted array declaration AST");
  }

  const ArrayInitializerAST *GetInitializer() const {
    const ArrayInitializerAST *Initializer;

    switch(Data.size()) {
      case 5:
        Initializer = llvm::cast<ArrayInitializerAST>(Data[4].AST);
        break;

      case 6:
        Initializer = llvm::cast<ArrayInitializerAST>(Data[5].AST);
        break;

      default:
        Initializer = 0;
    }

    return Initializer;
  }

  bool HasInitializer() const {
    return Data.size() >= 5;
  }

  llvm::StringRef GetArrayName() const {
    const IdentifierAST *Id = GetIdentifier();
    return Id->GetName();
  }
};

// scalar_declaration
//   : *Identifier*
//   | *Identifier* *Assign* scalar_initializer
class ScalarDeclarationAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == ScalarDeclaration;
  }

public:
  ScalarDeclarationAST(IdentifierAST *Id)
    : AbstractSyntaxTreeNode(ScalarDeclaration, Id) { }

  ScalarDeclarationAST(IdentifierAST *Id,
                       AssignAST *Assign,
                       ScalarInitializerAST *Init)
    : AbstractSyntaxTreeNode(ScalarDeclaration, Id, Assign, Init) { }

public:
  const IdentifierAST *GetIdentifier() const {
    return llvm::cast<IdentifierAST>(Data[0].AST);
  }

  const ScalarInitializerAST *GetInitializer() const {
    return HasInitializer() ? llvm::cast<ScalarInitializerAST>(Data[2].AST) : 0;
  }

  bool HasInitializer() const {
    return Data.size() == 3;
  }

  llvm::StringRef GetScalarName() const {
    const IdentifierAST *Id = GetIdentifier();
    return Id->GetName();
  }
};

// declaration
//   : scalar_declaration
//   | array_declaration
class DeclarationAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == Declaration;
  }

public:
  DeclarationAST(ScalarDeclarationAST *Decl)
    : AbstractSyntaxTreeNode(Declaration, Decl) { }

  DeclarationAST(ArrayDeclarationAST *Decl)
    : AbstractSyntaxTreeNode(Declaration, Decl) { }
};

// declaration_list
//   : declaration *Comma* declaration_list
//   | declaration
class DeclarationListAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == DeclarationList;
  }

public:
  DeclarationListAST(DeclarationAST *Decl,
                     CommaAST *Comma = 0,
                     DeclarationListAST *DeclList= 0)
    : AbstractSyntaxTreeNode(DeclarationList, Decl, Comma, DeclList) {
    assert((Comma && DeclList || !Comma && !DeclList) && "Missing elements");
  }
};

// var_declaration
//   : *Identifier* declaration_list *SemiColon*
class VarDeclarationAST : public AbstractSyntaxTreeNode {
public:
  static inline bool classof(const AbstractSyntaxTreeNode *AST) {
    return AST->GetId() == VarDeclaration;
  }

public:
  VarDeclarationAST(TypeAST *Type,
                    DeclarationListAST *DeclList,
                    SemiColonAST *Semi)
    : AbstractSyntaxTreeNode(VarDeclaration, Type, DeclList, Semi) { }
};

// non_empty_var_declarations
//   : var_declaration non_empty_var_declarations
//   | var_declaration
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

// var_declarations
//   : non_empty_var_declarations
//   | empty
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

// program
//   : var_declarations statements
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
//
// The same holds for CodeBlockAST -- it needs StatementsAST and StatementAST.
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

inline size_t AbstractSyntaxTreeNode::size() const {
  return llvm::isa<TokenAST>(this) ? 0 : Data.size();
}

inline bool AbstractSyntaxTreeNode::empty() const {
  return size() == 0;
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

inline CodeBlockAST::CodeBlockAST(LBraceAST *LBrace,
                                  StatementsAST *Stmts,
                                  RBraceAST *RBrace)
  : AbstractSyntaxTreeNode(CodeBlock, LBrace, Stmts, RBrace) { }

inline CodeBlockAST::CodeBlockAST(StatementAST *Stmt)
  : AbstractSyntaxTreeNode(CodeBlock, Stmt) { }

} // End namespace acse.

#endif // ACSE_IR_ABSTRACTSYNTAXTREENODE_H
