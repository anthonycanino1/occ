// error
// Copyright 2016 The occ Authors. All rights reserved.
// Test:

struct A0 {
  int x;
};

union A0 { // ERROR redefinition
  int x;
};
