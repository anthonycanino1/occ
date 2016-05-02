// error
// Copyright 2016 The occ Authors. All rights reserved.
// Test:

struct A0 { 
  int x;
  int x;  // ERROR duplicate member x
};

struct A0 {   // ERROR redefinition.*
  int y;
};

