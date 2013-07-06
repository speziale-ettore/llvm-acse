
// RUN: acse-parse %s

int nested, not, minus, scalar_read;
int array_read[10];

nested = 1 * (2 + 3);
not = !4;
minus = 5 + -(6);
scalar_read = nested;
array_read = array[7];
