(**************************************************************************)
(*                                                                        *)
(*                                 OCC                                    *)
(*                                                                        *)
(**************************************************************************)

(* The lexer definition *)

{
open Lexing
open Parser 

open Errors

let keyword_table =
  Misc.create_hashtable 30 [
    "auto", AUTO;
    "break", BREAK;
    "case", CASE;
    "char", CHAR;
    "const", CONST;
    "default", DEFAULT;
    "do", DO;
    "double", DOUBLE;
    "else", ELSE;
    "enum", ENUM;
    "extern", EXTERN;
    "float", FLOAT;
    "for", FOR;
    "goto", GOTO;
    "if", IF;
    "int", INT;
    "long", LONG;
    "register", REGISTER;
    "return", RETURN;
    "short", SHORT;
    "signed", SIGNED;
    "sizeof", SIZEOF;
    "static", STRUCT;
    "switch", SWITCH;
    "typedef", TYPEDEF;
    "union", UNION;
    "unsigned", UNSIGNED;
    "void", VOID;
    "volatile", VOLATILE;
    "while", WHILE;
]

(* Buffering string literals *)
let init_strbuf = Bytes.create 256
let strbuf = ref init_strbuf
let strind = ref 0 

let reset_strbuf () =
  strbuf := init_strbuf ; 
  strind := 0

let store_strrune r = 
  let rbuf = Rune.bytes r in
  let rlen = Bytes.length rbuf in
  if !strind + rlen >= Bytes.length !strbuf then begin
    let newlen = (Bytes.length (!strbuf)) * 2 in
    let newbuf = Bytes.create newlen in
    Bytes.blit !strbuf 0 newbuf 0 (Bytes.length !strbuf) ;
    strbuf := newbuf
  end;
  Bytes.blit rbuf 0 !strbuf !strind rlen ;
  strind := !strind + rlen 

let store_strchar c =
  if !strind >= Bytes.length !strbuf then begin
    let newlen = (Bytes.length (!strbuf)) * 2 in
    let newbuf = Bytes.create newlen in
    Bytes.blit !strbuf 0 newbuf 0 (Bytes.length !strbuf) ;
    strbuf := newbuf
  end;
  Bytes.unsafe_set !strbuf !strind c ;
  incr strind

let get_strbuf () =
  let s = Bytes.sub_string !strbuf 0 !strind in
  strbuf := init_strbuf ;
  s

(* Buffering int literals *)
type inttag =
  | Decimal
  | Octal
  | Hexidecimal
;;

type signtag =
  | Signed
  | Unsigned
;;

type inttypetag =
  | Int
  | Long
  | Long_Long 
;;

(* Positioning *)

(* TODO: Finish implementation: Need to properly parse the suffix,
 * leaving reset_intlex for now *)
let int_sign = ref Signed
let int_type = ref Int 

let reset_intlex () =
  int_sign := Signed ; int_type := Int 

(* TODO: Finish implementation: This will depend upon the arch that
 * the compiler is generating; hence, right now we only convert to
 * an int value and give it the default 'int' type *)
let convert_int tag (sign, typ) value = 
  match tag with
  | Decimal -> Type.Intval (int_of_string value, Type.Int)
  | Hexidecimal -> Type.Intval (int_of_string ("0x" ^ value), Type.Int)
  | Octal -> Type.Intval (int_of_string ("0o" ^ value), Type.Int) 

let convert_dec_float value =
  Type.Floatval (float_of_string value, Type.Float)

let convert_hex_float signf expn =
  let rec convert_hex_float' signf len i acc =
    if i >= len then acc
    else 
      let v = float_of_int (Misc.hex_to_int signf.[i]) in 
      let v' = v /. (float_of_int (Misc.pow 16 (i+1))) in
      convert_hex_float' signf len (i+1) (acc+.v')
  in 
  let l,r = Misc.split_fractional signf in
  let v = float_of_int (int_of_string ("0x" ^ l)) in
  let v' = convert_hex_float' r (String.length r) 0 v in
  if expn == "" then
    Type.Floatval (v', Type.Float)
  else
    let v'' = v' *. (2. ** float_of_int (int_of_string (Misc.chop expn))) in
    Type.Floatval (v'', Type.Float) 

let dump_token t =
  match t with
  | (Parser.INTLIT (Type.Intval (i,t))) -> Printf.printf "INTLIT:%d\n" i
  | (Parser.FLOATLIT (Type.Floatval (i,t))) -> Printf.printf "FLOATLIT:%f\n" i
  | (Parser.STRLIT (Type.Strval s)) -> Printf.printf "STRLIT:%s\n" s
  | (Parser.IDENT s) -> Printf.printf "IDENT:%s\n" s
  | Parser.LBRACK -> Printf.printf "LBRACK\n"
  | Parser.RBRACK -> Printf.printf "RBRACK\n"
  | Parser.LPAREN -> Printf.printf "LPAREN\n"
  | Parser.RPAREN -> Printf.printf "RPAREN\n"
  | Parser.LBRACE -> Printf.printf "LBRACE\n"
  | Parser.RBRACE -> Printf.printf "RBRACE\n"
  | Parser.DOT -> Printf.printf "DOT\n"
  | Parser.DOTS -> Printf.printf "DOTS\n"
  | Parser.COMMA -> Printf.printf "COMMA\n"
  | Parser.SEMI -> Printf.printf "SEMI\n"
  | Parser.ARROW -> Printf.printf "ARROW\n"
  | Parser.INC-> Printf.printf "INC\n"
  | Parser.DEC-> Printf.printf "DEC\n"
  | Parser.AND-> Printf.printf "AND\n"
  | Parser.STAR-> Printf.printf "STAR\n"
  | Parser.PLUS-> Printf.printf "PLUS\n"
  | Parser.MINUS-> Printf.printf "MINUS\n"
  | Parser.NEG-> Printf.printf "NEG\n"
  | Parser.BANG-> Printf.printf "BANG\n"
  | Parser.DIV-> Printf.printf "DIV\n"
  | Parser.MOD -> Printf.printf "MOD\n"
  | Parser.XOR -> Printf.printf "XOR\n"
  | Parser.OR -> Printf.printf "OR\n"
  | Parser.LSHIFT-> Printf.printf "LSHIFT\n"
  | Parser.RSHIFT-> Printf.printf "RSHIFT\n"
  | Parser.LT -> Printf.printf "LT\n"
  | Parser.GT -> Printf.printf "GT\n"
  | Parser.LTEQ-> Printf.printf "LTEQ\n"
  | Parser.GTEQ-> Printf.printf "GTEQ\n"
  | Parser.EQEQ-> Printf.printf "EQEQ\n"
  | Parser.NEQ-> Printf.printf "NEQ\n"
  | Parser.ANDAND-> Printf.printf "ANDAND\n"
  | Parser.OROR-> Printf.printf "OROR\n"
  | Parser.TERNARY-> Printf.printf "TERNARY\n"
  | Parser.COLON-> Printf.printf "COLON\n"
  | Parser.EQ -> Printf.printf "EQ \n"
  | Parser.PLUSEQ-> Printf.printf "PLUSEQ\n"
  | Parser.MINUSEQ-> Printf.printf "MINUSEQ\n"
  | Parser.MULTEQ-> Printf.printf "MULTEQ\n"
  | Parser.DIVEQ-> Printf.printf "DIVEQ\n"
  | Parser.MODEQ-> Printf.printf "MODEQ\n"
  | Parser.LSHIFTEQ-> Printf.printf "LSHIFTEQ\n"
  | Parser.RSHIFTEQ-> Printf.printf "RSHIFTEQ\n"
  | Parser.ANDEQ-> Printf.printf "ANDEQ\n"
  | Parser.OREQ-> Printf.printf "OREQ\n"
  | Parser.XOREQ-> Printf.printf "XOREQ\n" 
  | Parser.EOF -> Printf.printf "EOF\n" 
  | _ -> Printf.printf "Unknown\n" 

}

let blank = [' ' '\t' '\n']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_' '0'-'9']

let hex_preffix = "0" ['x' 'X']

let octal_digit = ['0'-'7']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let dec_digit_start = ['1'-'9']
let dec_digit = ['0'-'9']

let unsigned_suffix = ['u' 'U']
let signed_suffix = ['l' 'L']
let int_suffix = (unsigned_suffix signed_suffix? | 
                  unsigned_suffix signed_suffix signed_suffix? | 
                  signed_suffix unsigned_suffix? | 
                  signed_suffix signed_suffix unsigned_suffix? )

let frac_const = (dec_digit * "." dec_digit +) | dec_digit + "."
let expon_part = ['e' 'E'] ['+' '-']? dec_digit +

let hex_frac_const = (hex_digit * "." hex_digit +) | hex_digit + "."
let bin_expon_part = ['p' 'P'] ['+' '-']? dec_digit + 

let float_suff = ['f' 'F' 'l' 'L']

let hex_quad = hex_digit hex_digit hex_digit hex_digit 

rule ltoken = parse
  | blank +
    { ltoken lexbuf }

  | "//" [^'\n'] * "\n"
    { ltoken lexbuf }

  | "/*" _ * "*/" 
    { ltoken lexbuf }

  | "#" [^'\n'] * "\n"
    { ltoken lexbuf }

  | "0" ((octal_digit *) as v) int_suffix?
    { reset_intlex() ;
      INTLIT (convert_int Octal (!int_sign, !int_type) v) }

  | hex_preffix ((hex_digit *) as v) int_suffix? 
    { reset_intlex() ;
      INTLIT (convert_int Hexidecimal (!int_sign, !int_type) v) }

  | ((dec_digit_start dec_digit * | "0") as v) int_suffix? 
    { reset_intlex() ;
      INTLIT (convert_int Decimal (!int_sign, !int_type) v) }

  | ((frac_const expon_part?) | (dec_digit + expon_part) as v) float_suff? 
    { FLOATLIT (convert_dec_float v) } 

  | hex_preffix ((hex_frac_const as s) (bin_expon_part as e) | ((hex_digit+) as s) (bin_expon_part as e) ) float_suff?
    { FLOATLIT (convert_hex_float s e ) } 

  | lowercase identchar *
    { let s = Lexing.lexeme lexbuf in
      try Hashtbl.find keyword_table s
      with Not_found -> IDENT s } 

  | "'"

  | "\""
    { reset_strbuf() ;
      lstring lexbuf ; 
      STRLIT (Type.Strval (get_strbuf())) }

  | "[" { LBRACK }
  | "]" { RBRACK }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "." { DOT }
  | "..." { DOTS }
  | "," { COMMA }
  | ";" { SEMI }
  | "->" { ARROW } 
  | "++" { INC }
  | "--" { DEC }
  | "&" { AND }
  | "*" { STAR }
  | "+" { PLUS }
  | "-" { MINUS }
  | "~" { NEG }
  | "!" { BANG }
  | "/" { DIV }
  | "%" { MOD } 
  | "^" { XOR } 
  | "|" { OR } 
  | "<<" { LSHIFT }
  | "<" { LT }
  | ">" { GT }
  | ">>" { RSHIFT }
  | "<=" { LTEQ }
  | ">=" { GTEQ }
  | "==" { EQEQ }
  | "!=" { NEQ }
  | "&&" { ANDAND }
  | "||" { OROR }
  | "?" { TERNARY }
  | ":" { COLON }
  | "=" { EQ } 
  | "+=" { PLUSEQ }
  | "-=" { MINUSEQ }
  | "*=" { MULTEQ }
  | "/=" { DIVEQ }
  | "%=" { MODEQ }
  | "<<=" { LSHIFTEQ }
  | ">>=" { RSHIFTEQ }
  | "&=" { ANDEQ }
  | "|=" { OREQ }
  | "^=" { XOREQ }
  | eof { EOF }  

and lstring = parse
  | "\"" { () }
  | "\\" ['\\' '\'' '\"' 'n']
    { store_strchar (Lexing.lexeme_char lexbuf 0) ; 
      store_strchar (Lexing.lexeme_char lexbuf 1) ;
      lstring lexbuf } 
  | "\\" "u" (hex_quad as rune)
    { 
      let rune_bytes = int_of_string ("0x" ^ rune) in
      let rune = Rune.create rune_bytes in
      store_strrune rune ;
      lstring lexbuf } 
  | "\\" "U" (hex_quad hex_quad as rune)
    { 
      let rune_bytes = int_of_string ("0x" ^ rune) in
      let rune = Rune.create rune_bytes in
      store_strrune rune ;
      lstring lexbuf } 

  | _ 
    { 
      store_strchar (Lexing.lexeme_char lexbuf 0) ;
      lstring lexbuf }
  | eof
    { raise (Error (Unterminated_string)) }
