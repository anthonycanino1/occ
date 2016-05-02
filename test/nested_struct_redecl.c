// error
// Copyright 2016 The occ Authors. All rights reserved.
// Test:

struct A0 { 
  struct A1 { /* Nested struct is still declared globally */
    int x;
  } a1;
};

struct A1 {   // ERROR redefinition.*
  int y;
};

