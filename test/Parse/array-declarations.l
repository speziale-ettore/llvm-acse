
// RUN: acse-parse %s

int hello[5] = {72, 101, 108, 108, 111}, world[] = {87, 111, 114, 108, 100};
int implicit_null[4],
    explicit_null[4] = {0};
