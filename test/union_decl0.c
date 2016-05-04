// compile
// Copyright 2016 The occ Authors. All rights reserved.
// Test:

union A0 { 
  int x;
};

union A0 a0_0;
union A0 a0_1;

union A0 *a0_2;
union A0 *a0_3;

union A1 {
  int x;
};

union A1 a1_0;
union A1 a1_1;

union A2 {
  int x;
  union A3 {
    int x;
  } a3;
};

union A2 a2_0;
union A2 a2_1;

union A2 *a2_2;
union A2 *a2_3;

union A4 {
  int x;
};

