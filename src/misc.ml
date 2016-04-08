(* Copyright 2016 The OCC Authors. All rights reserved. 
   Use of this source is governed by a BSD-style license
   located in the LICENSE file. *)

exception Internal_error of string

let create_hashtable size init = 
  let tbl = Hashtbl.create size in
  List.iter (fun (k, v) -> Hashtbl.add tbl k v) init;
  tbl

let pow b e =
  let rec pow' e acc =
    match e with
    | 0 -> acc
    | _ -> pow' (e-1) (acc*b)
  in pow' e 1

let hex_to_int h =
  match h with
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'a' -> 10
  | 'b' -> 11
  | 'c' -> 12
  | 'd' -> 13
  | 'e' -> 14
  | 'f' -> 15
  | 'A' -> 10
  | 'B' -> 11
  | 'C' -> 12
  | 'D' -> 13
  | 'E' -> 14
  | 'F' -> 15
  | _   -> raise (Internal_error "invalid hex to int conversion")

let split_fractional s =
  try 
    let i = String.index s '.' in
    let len = String.length s in
    if i <> 0 then
      (String.sub s 0 i, String.sub s (i+1) (len-(i+1)))
    else
      ("0", String.sub s (i+1) (len-(i+1)))
  with Not_found -> (s,"")

let chop s =
  if s <> "" then
    String.sub s 1 ((String.length s)-1)
  else
    ""

