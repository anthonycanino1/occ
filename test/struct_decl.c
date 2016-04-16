struct A0 { int x; };
//struct A0 { int y; }; // ERROR: Redefinition

//A0 a0;  // ERROR: Unknown type
struct A0 a1;

/*
struct A1 {
  struct A1 a1; // ERROR: Incomplete type
};
*/

/*
struct A1 {
  struct A1 *a1;
};
*/

/*
struct A1 {
  struct A1 { // ERROR: Nested redefinition
    int x;
  } a1;
};
*/

/*
struct A1 {
  struct A0 { // ERROR: Redefinition
    int x;
  } a1;
};
*/

/*
struct A1 {
  int x; // ERROR: duplicate member
  int x;
};
*/

/*
struct A1 {
  int x;
  struct A1_1 {
    int x;
  } a1;
};
*/

typedef struct A2 { int x; } A2;
/*
int A2; // Redecl
*/

struct A3 {
  int A2; // A3 hides the typedef decl
  A2 *a2;
};

int f() {
  struct A3 { int x; };
  struct A3 a3;
  a3.x;
}
