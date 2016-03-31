(**************************************************************************)
(*                                                                        *)
(*                                 OCC                                    *)
(*                                                                        *)
(**************************************************************************)
type symbol = 
  {
    name: string;
    decl: node;
  }

and node = 
  | Nil
  | Name of symbol

type symbol_tag =
  | Mark
  | Symbol of symbol 

let scopes : (symbol_tag list) ref = ref []
let symtab : (string, symbol) Hashtbl.t = Hashtbl.create 8 

let lookup_symbol name = 
  try Hashtbl.find symtab name 
  with Not_found -> 
    let sym = {name= name; decl= Nil;} in
    Hashtbl.add symtab name sym ; sym






