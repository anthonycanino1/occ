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
;;
}

rule token = parse
  | eof { EOF }  

