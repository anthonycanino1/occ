// compile
// Copyright 2016 The occ Authors. All rights reserved.
// Test:

int f1();

int f2(void);

void f3(int x);

int f4(int *x);

int f5(int *x, int y, char **c);

void f6(int);

int f7(int *);

int f8(int *, int, char **);

// Some tougher ones
void h1(int *[]);
void h2(int (*)[]);
void h3(int *());
void h4(int (*)(void));
void h5(int (*const [])(int));

