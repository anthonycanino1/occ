(* Copyright 2016 The OCC Authors. All rights reserved. 
   Use of this source is governed by a BSD-style license
   located in the LICENSE file. *)

type store_spec =
  | Auto
  | Extern
  | Register
  | Static 

type unary_op =
  | Pre_inc
  | Post_inc
  | Pre_dec
  | Post_dec
  | Int_promote
  | Arith_neg
  | Bit_neg
  | Bool_not

type binary_op =
  | Arith_add
  | Airth_min
  | Airth_mult
  | Airth_div
  | Airth_mod
  | Bit_lshift
  | Bit_rshift
  | Bit_and
  | Bit_or
  | Bit_xor
  | Bool_and
  | Bool_or
  | Bool_eq
  | Bool_neq
  | Bool_lt
  | Bool_lteq
  | Bool_gt
  | Bool_gteq

type type_qual =
  | Const
  | Restrict
  | Volatile 

type symbol = 
  {
    name: string;
    decln: node;
    defn: node;
    stype: ctype; 
    storage: store_spec;
  }

and node = 
  | Nil
  (* Expressions *)
  | Name of symbol
  | Value of value
  | Unary_op of unary_op * node * ctype
  | Binary_op of binary_op * node * node * ctype
  | Indir of node * ctype
  | Address of node * ctype
  | Call of node * node list * ctype
  | Cast of node * ctype
  | Proj of node * node * ctype
  | Index of node * node * ctype
  | Assign of node * node * ctype
  | Comma of node * node * ctype
  (* Statements *)
  | Expr_stmt of node
  | If of node * node list * node list
  | Switch of node * node list
  | While of node * node list * [`Normal | `Do]
  | For of node list * node list
  | Goto of node
  | Continue 
  | Break
  | Return of node
  | Label of node list
  | Case of node * node list
  | Default of node * node list
  (* Declarations *)
  | Typedef of node * node
  | Struct of node * node list
  | Union of node * node list
  | Enum of node * node list
  | Function of node * node * node list * node list

and ctype_desc = 
  (* Compiler Tags *)
  | TNil
  (* Builtin C Types *)
  | TInt8
  | TUInt8
  | TInt16
  | TUInt16
  | TInt32
  | TUInt32
  | TInt64
  | TUInt64
  | TFloat32
  | TFloat64
  (* Extended C Types *)
  | TRune
  (* Complex Types *)
  | TArray of ctype 
  | TPointer of ctype
  | TStruct
  | TUnion
  | TFunction

and ctype =
  {
    typ: ctype_desc;
    quals: type_qual list;
  }

and value =
  | Strval of string
  | Intval of int * ctype
  | Floatval of float * ctype
  | Charval of string
  | Runeval of Rune.t 

let ctype_nil = {typ=TNil; quals=[];}

let scopes : ( [`Mark | `Symbol of symbol] list) ref = ref []
let symtab : (string, symbol) Hashtbl.t = Hashtbl.create 8 

let lookup_symbol name = 
  try Hashtbl.find symtab name 
  with Not_found -> 
    let sym = {name= name; decln= Nil; defn=Nil; storage=Auto; stype=ctype_nil;} in
    Hashtbl.add symtab name sym ; sym 
