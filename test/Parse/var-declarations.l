
// RUN: acse-parse %s | FileCheck %s

int hello, world;
int hello_init = 0, world_init = 1;

// CHECK: TODO
