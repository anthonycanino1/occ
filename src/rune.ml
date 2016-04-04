open Errors

type t = int

let create byte : t = byte

let bytes rune =
  match () with
  | () when rune >= 0x0080 && rune <= 0x07ff ->
    let buf = Bytes.create 2 in
    let r1 = 0xc0 lor (rune lsr 6) in
    let r2 = 0x80 lor (rune land 0x3f) in
    Bytes.unsafe_set buf 0 (Char.chr r1) ;
    Bytes.unsafe_set buf 1 (Char.chr r2) ;
    buf 
  | () when rune >= 0x0800 && rune <= 0xffff ->
    let buf = Bytes.create 3 in
    let r1 = 0xe0 lor (rune lsr 12) in
    let r2 = 0x80 lor ((rune lsr 6) land 0x3f) in
    let r3 = 0x80 lor (rune land 0x3f) in
    Bytes.unsafe_set buf 0 (Char.chr r1) ;
    Bytes.unsafe_set buf 1 (Char.chr r2) ;
    Bytes.unsafe_set buf 2 (Char.chr r3) ;
    buf 
  | () when rune >= 0x10000 && rune <= 0x1fffff ->
    let buf = Bytes.create 4 in
    let r1 = 0xf0 lor (rune lsr 18) in
    let r2 = 0x80 lor ((rune lsr 12) land 0x3f) in
    let r3 = 0x80 lor ((rune lsr 6) land 0x3f) in
    let r4 = 0x80 lor (rune land 0x3f) in
    Bytes.unsafe_set buf 0 (Char.chr r1) ;
    Bytes.unsafe_set buf 1 (Char.chr r2) ;
    Bytes.unsafe_set buf 2 (Char.chr r3) ;
    Bytes.unsafe_set buf 3 (Char.chr r4) ;
    buf 
  | _ ->
    raise (Error (Internal))


