(**************************************************************************)
(*                                                                        *)
(*                                 OCC                                    *)
(*                                                                        *)
(**************************************************************************)

(* The lexer definition *)

{
open Lexing
open Parser

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
}

let blank = [' ' '\t' '\n']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_' '0'-'9']

rule token = parse
  | blank 
    { token lexbuf }
  | lowercase identchar *
    { let s = Lexing.lexeme lexbuf in
      try Hashtbl.find keyword_table s
      with Not_found -> IDENT (Decl.lookup_symbol s) }
  | eof 
    { EOF }  
  | "#" { token lexbuf }

and pp_comment = parse
  | "#" { token lexbuf }
  

