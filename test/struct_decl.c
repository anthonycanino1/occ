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

struct A1 {
  struct A0 {
    int x;
  } a1;
};
