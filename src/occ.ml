(**************************************************************************)
(*                                                                        *)
(*                                 OCC                                    *)
(*                                                                        *)
(**************************************************************************)


let odbg = ref false

let spec = [
  ("-d", Arg.Bool (fun f -> odbg := f), ": debug mode");
]

let usage = "usage: occ <options> <files>\nOptions are:"

let compile file = 
  let ic = Preproc.preproc_file file in
  let lexbuf = Lexing.from_channel ic in
  let result = Parser.implementation Lexer.token lexbuf in
  result
  (*
  let lexbuf = Lexing.from_channel inchan in
  let result = Parser.implementation Lexer.token lexbuf in
  result
  *)
;; 

let _ = 
  Arg.parse spec compile usage
;;
