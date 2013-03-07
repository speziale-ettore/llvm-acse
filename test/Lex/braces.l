
// RUN: acse-lex %s | FileCheck %s

{} [] ()

// CHECK: LBrace RBrace LSquare RSquare LPar RPar
