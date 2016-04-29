// compile
// Copyright 2016 The occ Authors. All rights reserved.
// Test:

struct A0 { 
  int x;
};

struct A0 a0_0;
struct A0 a0_1;

struct A0 *a0_2;
struct A0 *a0_3;

struct A1 {
  int x;
};

struct A1 a1_0;
struct A1 a1_1;

struct A2 {
  int x;
  struct A3 {
    int x;
  } a3;
};

struct A2 a2_0;
struct A2 a2_1;

struct A2 *a2_2;
struct A2 *a2_3;
