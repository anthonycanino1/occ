// compile
// Copyright 2016 The occ Authors. All rights reserved.
// Test:

struct A1 {
  struct A1 *next;
};

struct A1 a1;
struct A1 *a2;

struct N0 {
  int x;
  struct N1 {
    int x;
    struct N2 {
      int x;
      struct N3 {
        int x;
        struct N4 {
          int x;
        } n4;
      } n3;
    } n2;
  } n1;
};
