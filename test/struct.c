/* Empty Struct */
struct A0 { } ;

struct A1 { int a; } ; 

struct A2 {
  int x;
  float y;
  char z;
};

struct A3 {
  struct {
    int a;
  } A;
  struct {
    int b;
  } B;
};

const struct A4 { int x; } a4_1;

static struct A5 { int x; } a5_1;

struct A6 { int x; } static a6_1;

struct A7 { int x; } a7_1, a7_2;

struct A10 { int x; } a10_1, a10_2;

const volatile struct A9 { int x; } a9_1;

//extern static struct A8 { int x; } a8_1; // ERROR: Multiple storage
