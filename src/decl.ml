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

let push_decl sym =
  scopes := ((`Symbol sym) :: !scopes) 

let mark_scope () =
  curr_block := !curr_block + 1 ;
  scopes := (`Mark :: !scopes)

let push_sym sym =
  Hashtbl.add symtab sym.name sym ;
  push_decl sym 

let lookup_sym name =
  try Some (Hashtbl.find symtab name)
  with Not_found -> None

let newsym name typ cls =
  let sym = {
    name=name;
    stype=typ; 
    stoclass=cls;
    block= !curr_block;
  } in
  push_sym sym ; 
  logf "declared %s with type %s" sym.name (string_of_ctype sym.stype); 
  sym 

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
  | None -> newsym name ctype_nil Nil_sto
  | Some sym -> begin
    match sym.stype.typ with 
    | Incomplete_typ    -> sym
    | Struct_typ (_,_)  -> sym
    | _ ->
      Errors.errorf errors Location.dummy "incompatible redeclaration of %s" sym.name ;
      sym
  end

let define_incomplete sym nd =
  if sym.stype.typ <> Incomplete_typ then
    Errors.errorf errors Location.dummy "redefinition of %s" sym.name
  else
    logf "defined %s" sym.name ; 
    sym.stype <- {typ=Incomplete_typ; qual=Noq}

let rec print_struct typ =
  match typ with
  | {typ=Struct_typ (s,t); qual=_} -> 
    (Printf.sprintf "%s { " s.name) ^
    (List.fold_right (fun (s,t) str -> Printf.sprintf "%s : " s.name ^ print_struct t ^ str) t "")
    ^ " } "
  | _ -> Printf.sprintf "%s " (string_of_ctype typ)

let define_struct sym typs =
  if sym.stype.typ <> Incomplete_typ then
    Errors.errorf errors Location.dummy "redefinition of %s" sym.name
  else
    let structyp = Struct_typ (sym, typs) in
    sym.stype <- {typ=structyp; qual=Noq; } ;
    logf "Built %s"  (print_struct sym.stype) 

let xdeclare name typ cls = 
  let osym = lookup_sym name in
    match osym with
    | None -> newsym name typ cls
    | Some sym ->
      if sym.block == !curr_block then 
        if sym.stype.typ = Incomplete_typ then
          sym
        else begin
          Errors.errorf errors Location.dummy "incompatible redeclaration of %s" sym.name ;
          sym
        end
      else
        newsym name typ cls 

let sdeclare name typ cls = 
  let osym = lookup_sym name in
    match osym with
    | None -> newsym name typ cls
    | Some sym ->
      if sym.block == !curr_block then begin
        Errors.errorf errors Location.dummy "duplicate member %s" sym.name ;
        sym
        end
      else
        newsym name typ cls 

let rec declare f dcl typ cls =
  match dcl with
  | Pointer_hp d  -> declare f d {typ=Pointer_typ typ; qual=Noq} cls
  | Array_hp d    -> declare f d {typ=Array_typ typ; qual=Noq} cls
  | Func_hp (d,_) -> declare f d {typ=Function_typ; qual=Noq} cls
  | Name_hp name  -> f name typ cls 
    
    
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


