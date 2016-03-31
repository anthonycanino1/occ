(**************************************************************************)
(*                                                                        *)
(*                                 OCC                                    *)
(*                                                                        *)
(**************************************************************************)

exception Found_eof

let odbg = ref false

let spec = [
  ("-d", Arg.Bool (fun f -> odbg := f), ": debug mode");
]

let usage = "usage: occ <options> <files>\nOptions are:"

let compile file = 
  let ic = Preproc.preproc_file file in
  let lexbuf = Lexing.from_channel ic in
  try
    while true do
      let t = Lexer.ltoken lexbuf in
      Lexer.dump_token t ; 
      if t = Parser.EOF then raise Found_eof
    done
  with Found_eof -> ()

let _ = 
  Arg.parse spec compile usage
