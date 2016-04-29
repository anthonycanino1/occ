(* Copyright 2016 The OCC Authors. All rights reserved. 
   Use of this source is governed by a BSD-style license
   located in the LICENSE file. *) 

open Ast
open Compile

let ctype_nil = let open Ast in {typ=Incomplete_typ; qual=Noq;} 

let symtab : (string, Ast.symbol) Hashtbl.t = Hashtbl.create 8 

let scopes : ( [`Mark | `Symbol of Ast.symbol] list) ref = ref []
let curr_block = ref 0 

let curr_anon = ref 0

let gen_anon_struct () = 
  let id = Printf.sprintf "struct::%d" !curr_anon in
  curr_anon := !curr_anon + 1 ;
  id

let lookup_sym name =
  try Some (Hashtbl.find symtab name)
  with Not_found -> None

let push_decl sym =
  scopes := ((`Symbol sym) :: !scopes) 

let push_sym sym =
  Hashtbl.add symtab sym.name sym ;
  push_decl sym 

let mark_scope =
  curr_block := !curr_block + 1 ;
  scopes := (`Mark :: !scopes)

let pop_decls () =
  if !curr_block == 0 then 
    raise (Misc.Internal_error "trying to pop global scope")
  else
  let rec pop_decls' scps =
    match scps with
    | (`Symbol sym) :: scps ->
      Hashtbl.remove symtab sym.name ;
      pop_decls' scps
    | (`Mark :: scps) ->
      scopes := scps
    | [] -> ()
  in pop_decls' !scopes ;
  curr_block := !curr_block - 1 

let tag name =
  let osym = lookup_sym name in
  match osym with
  | None ->
    let sym = {
      name=name;
      stype=ctype_nil; 
      stoclass=Nil_sto;
      block= !curr_block;
    } in
    push_sym sym ; 
    sym
  | Some sym ->
    if sym.stype.typ = Incomplete_typ then
      sym
    else
      begin
      Errors.errorf errors Location.dummy "incompatable redeclaration of %s" sym.name ;
      sym
      end

let define_incomplete sym nd =
  if sym.stype.typ <> Incomplete_typ then
    Errors.errorf errors Location.dummy "redefinition of symbol %s" sym.name
  else
    Printf.printf "defined %s\n" sym.name ; 
    sym.stype <- {typ=Incomplete_typ; qual=Noq}

let rec declare dcl typ cls =
  match dcl with
  | Pointer_hp d  -> declare d {typ=Pointer_typ typ; qual=Noq} cls
  | Array_hp d    -> declare d {typ=Array_typ typ; qual=Noq} cls
  | Func_hp (d,_) -> declare d {typ=Function_typ; qual=Noq} cls
  | Name_hp name  ->
    let osym = lookup_sym name in
    match osym with
    | None ->
      let sym = {
        name=name;
        stype=typ; 
        stoclass=cls;
        block= !curr_block;
      } in
      push_sym sym ; 
      Printf.printf "declared %s with type %s\n" sym.name (string_of_ctype sym.stype); 
      sym
    | Some sym ->
      if sym.stype.typ = Incomplete_typ then
        sym
      else
        begin
        Errors.errorf errors Location.dummy "incompatable redeclaration of %s" sym.name ;
        sym
        end

    


    
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
    let sym = {
      name=s;
      stype={typ=t; qual=Noq;};
      stoclass=Typedef_sto;
      block=0;
    } in
    Hashtbl.add symtab s sym) builtin_types


