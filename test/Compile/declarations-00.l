
// RUN: acse-cc -S %s

int a;
int b, c;

// CHECK: @a = external global i32
// CHECK: @b = external global i32
// CHECK: @c = external global i32

int e[2];
int f[3], g[5];

// CHECK: @e = external global [2 x i32]
// CHECK: @f = external global [3 x i32]
// CHECK: @g = external global [5 x i32]
