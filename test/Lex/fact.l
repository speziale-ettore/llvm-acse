
// RUN: acse-lex %s | FileCheck %s

int fact = 1,
    n = 10;
// CHECK:      Identifier Identifier Assign Number Comma
// CHECK-NEXT:            Identifier Assign Number SemiColon

while(n) {
  fact = fact * n;
  n = n - 1;
}
// CHECK:      While LPar Identifier RPar LBrace
// CHECK-NEXT:   Identifier Assign Identifier Mul Identifier SemiColon
// CHECK-NEXT:   Identifier Assign Identifier Sub Number SemiColon
// CHECK-NEXT: RBrace
