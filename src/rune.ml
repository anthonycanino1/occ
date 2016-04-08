(* Copyright 2016 The OCC Authors. All rights reserved. 
   Use of this source is governed by a BSD-style license
   located in the LICENSE file. *)

exception Invalid_sequence of string

type pointtag =
  | Two
  | Three
  | Four

type t = int * pointtag

let max_rune = 0x0010ffff

let is_valid_rune byte = byte <= max_rune && (byte < 0xd800 || byte >= 0xe000)

let create byte tag : t = (byte, tag)

let from_rune r =
  match () with
  | () when r >= 0x0080 && r <= 0x07ff ->
    create r Two
  | () when r >= 0x0800 && r <= 0xffff ->
    create r Three
  | () when r >= 0x10000 && r <= 0x1fffff ->
    create r Four
  | _ -> raise (Misc.Internal_error "invalid rune")

let from_sequence bytes = 
  let len = Bytes.length bytes in
  match () with
  | () when len == 2 ->
    let b1 = (Char.code bytes.[0]) in
    let b2 = (Char.code bytes.[1]) in
    let rune = ((b1 land 0x1f) lsl 6) lor (b2 land 0x3f) in
    if is_valid_rune rune then from_rune rune
    else raise (Invalid_sequence "")
  | () when len == 3 ->
    let b1 = (Char.code bytes.[0]) in
    let b2 = (Char.code bytes.[1]) in
    let b3 = (Char.code bytes.[2]) in
    let rune = ((b1 land 0x0f) lsl 12) lor ((b2 land 0x3f) lsl 6) lor (b3 land 0x3f) in
    if is_valid_rune rune then from_rune rune
    else raise (Invalid_sequence "")
  | () when len == 4 ->
    let b1 = (Char.code bytes.[0]) in
    let b2 = (Char.code bytes.[1]) in
    let b3 = (Char.code bytes.[2]) in
    let b4 = (Char.code bytes.[3]) in
    let rune = ((b1 land 0x07) lsl 18) lor ((b2 land 0x3f) lsl 12) lor ((b3 land 0x3f) lsl 6) lor (b4 land 0x3f) in
    if is_valid_rune rune then from_rune rune
    else raise (Invalid_sequence "")
  | _ ->
    raise (Invalid_sequence "")

let to_bytes (r,t) =
  match t with
  | Two ->
    let bytes = Bytes.create 2 in
    let r1 = 0xc0 lor (r lsr 6) in
    let r2 = 0x80 lor (r land 0x3f) in
    Bytes.unsafe_set bytes 0 (Char.chr r1) ;
    Bytes.unsafe_set bytes 1 (Char.chr r2) ;
    bytes
  | Three ->
    let bytes = Bytes.create 3 in
    let r1 = 0xe0 lor (r lsr 12) in
    let r2 = 0x80 lor ((r lsr 6) land 0x3f) in
    let r3 = 0x80 lor (r land 0x3f) in
    Bytes.unsafe_set bytes 0 (Char.chr r1) ;
    Bytes.unsafe_set bytes 1 (Char.chr r2) ;
    Bytes.unsafe_set bytes 2 (Char.chr r3) ;
    bytes 
  | Four ->
    let bytes = Bytes.create 4 in
    let r1 = 0xf0 lor (r lsr 18) in
    let r2 = 0x80 lor ((r lsr 12) land 0x3f) in
    let r3 = 0x80 lor ((r lsr 6) land 0x3f) in
    let r4 = 0x80 lor (r land 0x3f) in
    Bytes.unsafe_set bytes 0 (Char.chr r1) ;
    Bytes.unsafe_set bytes 1 (Char.chr r2) ;
    Bytes.unsafe_set bytes 2 (Char.chr r3) ;
    Bytes.unsafe_set bytes 3 (Char.chr r4) ;
    bytes

let to_string t =
  let bytes = to_bytes t in
  Bytes.to_string bytes
