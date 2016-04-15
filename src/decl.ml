(* Copyright 2016 The OCC Authors. All rights reserved. 
   Use of this source is governed by a BSD-style license
   located in the LICENSE file. *) 

let ctype_nil = let open Ast in {typ=Incomplete_typ; quals=[];}

let scopes : ( [`Mark | `Symbol of Ast.symbol] list) ref = ref []

let decl_symtab : (string, Ast.symbol) Hashtbl.t = Hashtbl.create 8 
let struct_symtab : (string, Ast.symbol) Hashtbl.t = Hashtbl.create 8 

let new_name name table =
  let open Ast in
  let sym : symbol = {
    name=name; 
    sdesc=Nil_sym; 
    decln=Nil; 
    defn=Nil; 
    storage=Auto_sto; 
    stype=ctype_nil;
  } in
  Hashtbl.add table name sym ; sym 

let new_decl_name name = new_name name decl_symtab
let new_struct_name name = new_name name struct_symtab

let lookup_sym name table = 
  try Some (Hashtbl.find table name)
  with Not_found -> None 

let lookup_struct_sym name = lookup_sym name decl_symtab
let lookup_decl_sym name = lookup_sym name struct_symtab

(* Setup symbol table for builtin types *)
let builtin_types = [
  ("void", Ast.Unit_typ);
  ("char", Ast.Int8_typ);
  ("short", Ast.Int16_typ);
  ("int", Ast.Int32_typ);
  ("long", Ast.Int32_typ);
  ("float", Ast.Float32_typ);
  ("double", Ast.Float64_typ);
  ("rune", Ast.Int32_typ);
]

let () =
  List.iter (fun (s,t) -> 
    let sym = new_decl_name s in 
    let open Ast in 
    sym.stype <- {typ=t; quals=[];}; sym.sdesc <- Type_sym) builtin_types


