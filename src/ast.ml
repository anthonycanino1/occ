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
  | Decl_sym
  | Type_sym

type symbol = 
  {
    name: string;
    sdesc: symbol_desc;
    mutable stype: ctype; 
    storage: store_spec;
    block: int;
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
  | Variable of symbol * node
  | Typedef of string * node
  | Struct of string * node list
  | Union of string option * node list
  | Enum of string option * node list
  | Function of string * node * node list * node list

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

(* Type exists soley to aid in the construction of types *)
and type_cons =
  | Name_hp of string
  | Pointer_hp of type_cons
  | Func_hp of type_cons * ctype list
  | Array_hp of type_cons 

let rec string_of_ctype {typ;quals}  = 
  match typ with
  | Unit_typ    -> "unit"
  | Int8_typ    -> "int8"
  | UInt8_typ   -> "uint8"
  | Int16_typ   -> "int16"
  | UInt16_typ  -> "uint16"
  | Int32_typ   -> "int32"
  | UInt32_typ  -> "uint32"
  | Int64_typ   -> "int64"
  | UInt64_typ  -> "uint64"
  | Float32_typ -> "float32"
  | Float64_typ -> "float64"
  (* Extended C Types *)
  | Rune_typ        -> "rune"
  | Array_typ t     -> string_of_ctype t ^ "[]"
  | Pointer_typ t   -> string_of_ctype t ^ "*"
  | Struct_typ      -> "struct"
  | Union_typ       -> "union"
  | Function_typ    -> "function"
  | Incomplete_typ  -> "incomplete"


let string_of_node nd =
  match nd with
  | Nil -> "Nil"
  (* Expressions *)
  | Name _ -> "Name"
  | Value _ -> "Value"
  | Unary_op (_,_,_) -> "Unary_op"
  | Binary_op (_,_,_,_) -> "Binary_op"
  | Indir (_,_) -> "Indir"
  | Address (_,_) -> "Address"
  | Call (_,_,_) -> "Call"
  | Cast (_,_) -> "Cast"
  | Proj (_,_,_) -> "Proj"
  | Index (_,_,_) -> "Index"
  | Assign (_,_,_) -> "Assign"
  | Comma (_,_,_) -> "Comma"
  (* Statements *)
  | Expr_stmt _ -> "Expr_stmt"
  | If (_,_,_) -> "If"
  | Switch (_,_) -> "Switch"
  | While (_,_,_) -> "While"
  | For (_,_) -> "For"
  | Goto _ -> "Goto"
  | Continue -> "Continue"
  | Break -> "Break"
  | Return _ -> "Return"
  | Label _ -> "Label"
  | Case (_,_) -> "Case"
  | Default (_,_) -> "Default"
  (* Declarations *)
  | Variable (_,_) -> "Variable"
  | Typedef (_,_) -> "Typedef"
  | Struct (_,_) -> "Struct"
  | Union (_,_) -> "Union"
  | Enum (_,_) -> "Enum"
  | Function (_,_,_,_) -> "Function" 
