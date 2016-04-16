(* Copyright 2016 The OCC Authors. All rights reserved. 
   Use of this source is governed by a BSD-style license
   located in the LICENSE file. *) 

open Ast

let ctype_nil = let open Ast in {typ=Incomplete_typ; quals=[];} 

let symtab : (string, Ast.symbol) Hashtbl.t = Hashtbl.create 8 

let scopes : ( [`Mark | `Symbol of ('a * 'b * 'c * 'd * 'e * 'f * 'g)] list) ref = ref []
let curr_block = ref 0 

let lookup_sym name =
  try 
    (Hashtbl.find symtab name)
  with Not_found -> 
    let sym : symbol = {
      name=name; 
      sdesc=Nil_sym; 
      decln=Nil; 
      defn=Nil; 
      storage=Auto_sto; 
      stype=ctype_nil;
      block= !curr_block;
    } in
    Hashtbl.add symtab name sym ; 
    sym 

(* TODO: Super simple name node creation right now,
 * but will get complex. declaring functions *)
let new_name sym = Name sym
let old_name sym = sym.decln

let push_decl sym =
  let dcl = (sym.name, sym.sdesc, sym.decln, sym.defn, sym.stype, sym.storage, sym.block) in
  scopes := (`Symbol dcl :: !scopes)

let mark_scope =
  scopes := (`Mark :: !scopes)

let pop_decls () =
  let rec pop_decls' scps =
    match scps with
    | (`Symbol (name,sdesc,decln,defn,stype,storage,block)) :: scps ->
      let sym = lookup_sym name in
      sym.sdesc <- sdesc ;
      sym.decln <- decln ;
      sym.defn  <- defn ;
      sym.stype <- stype ;
      sym.storage <- storage ;
      sym.block <- block ;
      pop_decls' scps 
    | (`Mark :: scps) ->
      scopes := scps
    | [] -> ()
  in pop_decls' !scopes

let declare nd =
  let declare' sym =
    match sym.decln with 
    | Nil ->
      sym.decln <- nd ; 
      sym.block <- !curr_block ;
    | _ ->
      if !curr_block == sym.block then
        Compile.Errors.errorf Compile.errors Location.dummy "redeclaration of symbol %s" sym.name
      else
        push_decl sym ;
        sym.decln <- nd ;
        sym.block <- !curr_block 
  in
  match nd with
  | Variable (sym, _) -> declare' sym
  | Typedef (sym, _) -> declare' sym
  | Struct ((Some sym), _) -> declare' sym
  | Union ((Some sym), _) -> declare' sym
  | Enum ((Some sym), _) -> declare' sym
  | Function (sym, _, _, _) -> declare' sym
  | _ -> raise (Misc.Internal_error "declaring a non decl node")
  
(* Setup symbol table for builtin types *)
let builtin_types = [
  ("void", Unit_typ);
  ("char", Int8_typ);
  ("short", Int16_typ);
  ("int", Int32_typ);
  ("long", Int32_typ);
  ("float", Float32_typ);
  ("double", Float64_typ);
  ("rune", Int32_typ);
]

let () =
  List.iter (fun (s,t) -> 
    let sym = lookup_sym s in 
    sym.stype <- {typ=t; quals=[];}; sym.sdesc <- Type_sym) builtin_types


