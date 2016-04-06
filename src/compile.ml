(**************************************************************************)
(*                                                                        *)
(*                                 OCC                                    *)
(*                                                                        *)
(**************************************************************************)
module Errors = struct
  type t = (string list) ref 

  let empty : t = ref []

  let length errs = List.length !errs

  let errorf errs loc fmt =
    Printf.ksprintf (fun s -> errs := s :: !errs) ("%s: error: " ^^ fmt) (Location.to_error_string loc)

  let dump errs = List.iter (fun e -> Printf.printf "%s\n" e) (List.rev !errs)
end

let errors = Errors.empty



