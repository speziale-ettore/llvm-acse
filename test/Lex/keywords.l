
// RUN: acse-lex %s | FileCheck %s

int
if else
do while
read write

// CHECK:      Int
// CHECK:      If Else
// CHECK-NEXT: Do While
// CHECK-NEXT: Read Write
