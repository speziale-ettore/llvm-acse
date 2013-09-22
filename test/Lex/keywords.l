
// RUN: acse-lex %s | FileCheck %s

int
if else
do while
return
read write

// CHECK:      Int
// CHECK:      If Else
// CHECK-NEXT: Do While
// CHECK-NEXT: Return
// CHECK-NEXT: Read Write
