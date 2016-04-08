(* Copyright 2016 The OCC Authors. All rights reserved. 
   Use of this source is governed by a BSD-style license
   located in the LICENSE file. *)

type ctype = 
  | Int8
  | UInt8
  | Int16
  | UInt16
  | Int32
  | UInt32
  | Int64
  | UInt64
  | Float32
  | Float64

type value =
  | Strval of string
  | Intval of int * ctype
  | Floatval of float * ctype
  | Charval of string
  | Runeval of Rune.t 
