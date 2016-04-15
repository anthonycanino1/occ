(* Copyright 2016 The OCC Authors. All rights reserved. 
   Use of this source is governed by a BSD-style license
   located in the LICENSE file. *)

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

type store_spec =
  | Auto_sto
  | Extern_sto
  | Register_sto
  | Static_sto 
  | Typedef_sto
  | Nil_sto

type type_qual =
  | Const_qual
  | Restrict_qual
  | Volatile_qual

type func_spec =
  | Inline_spec
  | Noreturn_spec

type symbol_desc =
  | Nil_sym
  | Decl_sym
  | Type_sym

type symbol = 
  {
    name: string;
    mutable sdesc: symbol_desc;
    mutable decln: node;
    mutable defn: node;
    mutable stype: ctype; 
    mutable storage: store_spec;
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
  | Struct of node option * node list
  | Union of node * node list
  | Enum of node * node list
  | Function of node * node * node list * node list

and ctype_desc = 
  (* Builtin C Types *)
  | Unit_typ
  | Int8_typ
  | UInt8_typ
  | Int16_typ
  | UInt16_typ
  | Int32_typ
  | UInt32_typ
  | Int64_typ
  | UInt64_typ
  | Float32_typ
  | Float64_typ
  (* Extended C Types *)
  | Rune_typ
  (* Complex Types *)
  | Array_typ of ctype 
  | Pointer_typ of ctype
  | Struct_typ
  | Union_typ
  | Function_typ
  | Incomplete_typ

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
