
// RUN: acse-lex %s | FileCheck %s

// Algebraic operators.
+ - * / %
// CHECK: Add Sub Mul Div Mod

// Relational operators.
< <= == >= >
// CHECK: Less LessOrEqual Equal NotEqual GreaterOrEqual Greater

// Bitwise operators.
& | ! << >>
// CHECK: BAnd BOr BNot LShift RShift

// Logical operators.
&& ||
// CHECK: LAnd LOr
