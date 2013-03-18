
// RUN: acse-lex -eat-comments %s | FileCheck %s

// A long time ago in a galaxy, far far away ...
//
// It is a period of civil war. Rebel spaceships, striking ...

// CHECK-NOT: LineComment
// CHECK-NOT: LineComment
// CHECK-NOT: LineComment

read text; // ... from a hidden base

// CHECK: Read Identifier SemiColon

/*
 * Eh, in questo mondo di ladri
 * C'è ancora un gruppo di amici
 * che non si arrendono mai.
 *
 * Eh, in questo mondo di santi
 * Il nostro cuore è rapito
 * Da mille profeti e da quattro cantanti.
*/

// CHECK-NOT: MultiLineComment
