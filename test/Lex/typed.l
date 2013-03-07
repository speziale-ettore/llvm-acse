
// RUN: acse-lex %s | FileCheck %s

1 2 3 5 7
// CHECK: Number Number Number Number Number

hello world
// CHECK: Identifier Identifier
