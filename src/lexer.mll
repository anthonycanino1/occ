(**************************************************************************)
(*                                                                        *)
(*                                 OCC                                    *)
(*                                                                        *)
(**************************************************************************)

(* The lexer definition *)

{
open Lexing
open Parser 

open Ast

type error = 
  | Invalid_char
  | Invalid_rune
  | Invalid_sequence
  | Unterminated_string 

let report_error err loc = 
  match err with
  | Invalid_char -> Compile.Errors.errorf Compile.errors loc "invalid chararacter literal."  
  | Invalid_rune -> Compile.Errors.errorf Compile.errors loc "invalid rune literal."  
  | Invalid_sequence -> Compile.Errors.errorf Compile.errors loc "invalid rune sequence."  
  | Unterminated_string -> Compile.Errors.errorf Compile.errors loc "unterminated string."  

let keyword_table =
  Misc.create_hashtable 30 [
    "auto", AUTO;
    "break", BREAK;
    "case", CASE;
    "const", CONST;
    "default", DEFAULT;
    "do", DO;
    "else", ELSE;
    "enum", ENUM;
    "extern", EXTERN;
    "for", FOR;
    "goto", GOTO;
    "if", IF;
    "inline", INLINE;
    "register", REGISTER;
    "restrict", RESTRICT;
    "return", RETURN;
    "signed", SIGNED;
    "sizeof", SIZEOF;
    "static", STATIC;
    "struct", STRUCT;
    "switch", SWITCH;
    "typedef", TYPEDEF;
    "union", UNION;
    "unsigned", UNSIGNED;
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
  let rbuf = Rune.to_bytes r in
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
  | Decimal -> Intval (int_of_string value, {typ=Int32_typ; quals=[Const_qual]})
  | Hexidecimal -> Intval (int_of_string ("0x" ^ value), {typ=Int32_typ; quals=[Const_qual]})
  | Octal -> Intval (int_of_string ("0o" ^ value), {typ=Int32_typ; quals=[Const_qual]}) 

let convert_dec_float value =
  Floatval (float_of_string value, {typ=Float32_typ; quals=[Const_qual]})

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
    Floatval (v', {typ=Float32_typ; quals=[Const_qual]})
  else
    let v'' = v' *. (2. ** float_of_int (int_of_string (Misc.chop expn))) in
    Floatval (v'', {typ=Float32_typ; quals=[Const_qual];}) 

let is_rune c =
  let byte = int_of_char c in
  if (byte land 0x80) != 0 then true else false

let simple_esc_code c = 
  let raw_esc_code c =
    match c with
    | '\''  -> 0x27
    | '"'   -> 0x22
    | '?'   -> 0x3f
    | '\\'  -> 0x5c
    | 'a'   -> 0x07
    | 'b'   -> 0x08
    | 'f'   -> 0x0c
    | 'n'   -> 0x0a
    | 'r'   -> 0x0d
    | 't'   -> 0x09
    | 'v'   -> 0x0b
    | _     -> raise (Misc.Internal_error "invalid escape code")
  in Char.chr (raw_esc_code c)

let simple_esc_char c = String.make 1 (simple_esc_code c)
  
let dump_token t =
  match t with
  | (Parser.INTLIT (Ast.Intval (i,t))) -> Printf.printf "INTLIT:%d\n" i
  | (Parser.FLOATLIT (Ast.Floatval (i,t))) -> Printf.printf "FLOATLIT:%f\n" i
  | (Parser.STRLIT (Ast.Strval s)) -> Printf.printf "STRLIT:%s\n" s
  | (Parser.RUNELIT (Ast.Runeval s)) -> Printf.printf "RUNELIT:%s\n" (Rune.to_string s)
  | (Parser.CHARLIT (Ast.Charval s)) -> Printf.printf "CHARLIT:%s\n" s
  | (Parser.IDENT s) -> Printf.printf "IDENT:%s\n" s
  | (Parser.STRUCT) -> Printf.printf "STRUCT\n"
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
let ident_start = ['A'-'Z' 'a'-'z' '_']
let ident_char = ['A'-'Z' 'a'-'z' '_' '0'-'9']

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

let simple_escape = ['\'' '"' '?' '\\' 'a' 'b' 'f' 'n' 't' 'v']

rule ltoken = parse
  | blank +
    { ltoken lexbuf }

  | "//" [^'\n'] * "\n"
    { ltoken lexbuf }

  | "/*" _ * "*/" 
    { ltoken lexbuf }

  | "#" [^'\n'] * "\n"
    { ltoken lexbuf }

  | ident_start ident_char *
    { let s = Lexing.lexeme lexbuf in
      try 
        Hashtbl.find keyword_table s
      with Not_found ->
        let sym_opt = Decl.lookup_decl_sym s in
        match sym_opt with
        | Some sym -> begin
          match sym.sdesc with
          | Type_sym  -> TYPE s
          | _         -> IDENT s
          end
        | None -> IDENT s }

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
  
  | "'\\u" (hex_quad as rune) "'"
    { RUNELIT (Ast.Runeval (Rune.from_rune (int_of_string ("0x" ^ rune)))) } 

  | "'\\U" (hex_quad hex_quad as rune) "'"
    { RUNELIT (Ast.Runeval (Rune.from_rune (int_of_string ("0x" ^ rune)))) } 

  | "'\\" (simple_escape as esc) "'"
    { CHARLIT (Ast.Charval (simple_esc_char esc)) }

  | "'" ([^ '\'' '\\'] * as lit)  "'"
    { match () with
      | () when is_rune lit.[0] -> begin
        try
          let bytes = Bytes.of_string lit in
          RUNELIT (Ast.Runeval (Rune.from_sequence bytes)) 
        with (Rune.Invalid_sequence _) -> 
          report_error Invalid_rune (Location.curr lexbuf) ;
          UNKNOWN
        end
      | () when String.length lit == 1 -> CHARLIT (Ast.Charval lit)
      | _ -> report_error Invalid_char (Location.curr lexbuf) ; UNKNOWN }

  | "\""
    { reset_strbuf() ;
      lstring lexbuf ; 
      STRLIT (Ast.Strval (get_strbuf())) }

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
  | _ { UNKNOWN }


and lstring = parse
  | "\"" { () }
  | "\\" (simple_escape as esc)
    { store_strchar (simple_esc_code esc) ;
      lstring lexbuf } 
  | "\\" "u" (hex_quad as quad)
    { 
      let rune = int_of_string ("0x" ^ quad) in
      store_strrune (Rune.from_rune rune) ;
      lstring lexbuf 
    } 
  | "\\" "U" (hex_quad hex_quad as quad)
    { let rune = int_of_string ("0x" ^ quad) in
      if Rune.is_valid_rune rune then begin
        store_strrune (Rune.from_rune rune) ;
        lstring lexbuf
        end
      else 
        report_error Invalid_rune (Location.curr lexbuf) } 
  | _ 
    { store_strchar (Lexing.lexeme_char lexbuf 0) ;
      lstring lexbuf }
  | eof
    { report_error Unterminated_string (Location.curr lexbuf) }
