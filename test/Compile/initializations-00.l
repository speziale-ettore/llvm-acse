
// RUN: acse-cc -S %s

int two = 2, three = 3, five = 5;

// CHECK: @two = global i32 2
// CHECK: @three = global i32 3
// CHECK: @five = global i32 5

int hello[5] = {72, 101, 108, 108, 111};
int world[] = {87, 111, 114, 108, 100};
int explicit_null[4] = {0};

// CHECK: @hello = global [5 x i32] [i32 72, i32 101, i32 108, i32 108, i32 111]
// CHECK: @world = global [5 x i32] [i32 87, i32 111, i32 114, i32 108, i32 100]
// CHECK: @explicit_null = global [4 x i32] zeroinitializer
