(* Copyright 2016 The OCC Authors. All rights reserved. 
   Use of this source is governed by a BSD-style license
   located in the LICENSE file. *)

let preproc_file file =
  let s = Printf.sprintf "cpp %s" file in
  Unix.open_process_in s

  
