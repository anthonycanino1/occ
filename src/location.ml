(* Copyright 2016 The OCC Authors. All rights reserved. 
   Use of this source is governed by a BSD-style license
   located in the LICENSE file. *)

open Lexing

type t = string * int * int * bool

let curr_file = ref ""

let curr lexbuf = 
  (!curr_file, lexbuf.lex_start_p.pos_lnum, lexbuf.lex_start_p.pos_cnum, false)

let curr_yacc pos =
  (!curr_file, pos.pos_lnum, pos.pos_cnum, false)

(* For use while developing *)
let dummy = (!curr_file, 0, 0, true)

let to_error_string (file,line,col,_) = 
  Printf.sprintf "%s:%d:%d" file line col
