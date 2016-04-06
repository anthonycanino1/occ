(**************************************************************************)
(*                                                                        *)
(*                                 OCC                                    *)
(*                                                                        *)
(**************************************************************************)
open Lexing

type t = string * int * int * bool

let curr_file = ref ""

let curr lexbuf = 
  (!curr_file, lexbuf.lex_start_p.pos_lnum, lexbuf.lex_start_p.pos_cnum, false)

let to_error_string (file,line,col,_) = 
  Printf.sprintf "%s:%d:%d" file line col
