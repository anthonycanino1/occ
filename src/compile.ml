(* Copyright 2016 The OCC Authors. All rights reserved. 
   Use of this source is governed by a BSD-style license
   located in the LICENSE file. *)

module Errors = struct
  type t = (string list) ref 

  let empty : t = ref []

  let length errs = List.length !errs

  (* Use the following for debugging *)
  let errorf errs loc fmt =
    errs := "dummy" :: !errs ;
    Printf.printf ("%s: error: " ^^ fmt ^^ "\n") (Location.to_error_string loc)

  let dump errs = ()

  (* Use the following for actual compilation *)
  (*
  let errorf errs loc fmt =
    Printf.ksprintf (fun s -> errs := s :: !errs) ("%s: error: " ^^ fmt) (Location.to_error_string loc)

  let dump errs = List.iter (fun e -> Printf.printf "%s\n" e) (List.rev !errs)
  *)
end

let errors = Errors.empty

let logf fmt =
  Printf.printf ("[DEBUG] " ^^ fmt ^^ "\n") 


