
// RUN: acse-lex %s | FileCheck %s

// A long time ago in a galaxy, far far away ...
//
// It is a period of civil war. Rebel spaceships, striking ...

// CHECK: LineComment
// CHECK: LineComment
// CHECK: LineComment

read text; // ... from a hidden base

// CHECK: Read Identifier SemiColon LineComment

/*
 * Eh, in questo mondo di ladri
 * C'è ancora un gruppo di amici
 * che non si arrendono mai.
 *
 * Eh, in questo mondo di santi
 * Il nostro cuore è rapito
 * Da mille profeti e da quattro cantanti.
*/

// CHECK: MultiLineComment
