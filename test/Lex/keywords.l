
// RUN: acse-lex %s | FileCheck %s

if else
do while
read write

// CHECK:      If Else
// CHECK-NEXT: Do While
// CHECK-NEXT: Read Write
