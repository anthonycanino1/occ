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
// 123 
// 123u 123U 
// 123l 123L 
// 123lu 123Lu 123ul 123uL
// 123Ul 123UL 123lU 123LU
// 123ll 123LL
// 123llu 123LLu 123ull 123uLL
// 123llU 123LLU 123Ull 123ULL

// 0x10 0x1a 0xff
// 017 027

// Float Constants
// 1. .1 1.1
// 1e2 .1e2 1.1e2
// 1e+2 .1e+2 1.1e+2
// 1e-2 .1e-2 1.1e-2

// 1.f .1f 1.1f
// 1.l .1l 1.1l

//0x.ap0    // 0.625
//0x.ap1    // 1.25
//0x.ap2    // 2.5
//0x.a1p0   // 0.62896
//0x.ap-1   // 0.314453
//0x.ap-2   // 0.156250
//0x.afp0   // 0.683594
//0x.afp4   // 10.937500
//0xap2     // 40.0
//0xap-2    // 2.5

// 0xa.f 0x.ap2f 0xa.ap2f
// 0xa.l 0x.ap2l 0xa.ap2l

// Chars
'a'

// Runes
//'\u00a2'
//'\u20ac'
//'\U00010348'
'¢'
'€'
'𐍈'
'⌘'

// Strings
// "simple string"
// "simple string with newline\n"
// "simple escaped \" string"
// "simple escaped \\ string"
"String with unicode À"
"Two Code Point:\u00a2"
"Three Code Point:\u20ac"
"Four Code Point:\U00010348"

"alpha:α beta:β gamma:γ"
"alpha:\u03b1 beta:\u03b2 gamma:\u03b3"

