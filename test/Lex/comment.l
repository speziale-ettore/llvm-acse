
// RUN: acse-lex %s | FileCheck %s

// A long time ago in a galaxy, far far away ...
//
// It is a period of civil war. Rebel spaceships, striking ...

// CHECK: Comment
// CHECK: Comment
// CHECK: Comment

read text; // ... from a hidden base

// CHECK: Read Identifier SemiColon Comment
