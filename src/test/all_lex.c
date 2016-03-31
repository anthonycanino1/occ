/**************************************************************************/
/*                                                                        */
/*                                 OCC                                    */
/*                                                                        */
/**************************************************************************/

// File simply contains C code that exercises the full extent of the lexer.
// It is not meant to actualy compile, but to serve as a reference to check
// the lexer directly.

// Punctuators
// [ ] ( ) { } . -> ,
// ++ -- & * + - ~ !
// / & << >> < > <= >= == != ^ | && ||
// ? : ; ...
// = *= /= %= += -= <<= >>= &= ^= |=

// Identifiers
// _
// __
// a
// _a
// abc
// _abc
// abc123

// Integer Constants
 123 
// 123u 123U 
// 123l 123L 
// 123lu 123Lu 123ul 123uL
// 123Ul 123UL 123lU 123LU
// 123ll 123LL
// 123llu 123LLu 123ull 123uLL
// 123llU 123LLU 123Ull 123ULL

 0x10 0x1a 0xff
 017 027



// Strings
// "simple string"
// "simple string with newline\n"
// "simple escaped \" string"
// "simple escaped \\ string"

