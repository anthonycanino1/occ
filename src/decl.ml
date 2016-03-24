(**************************************************************************)
(*                                                                        *)
(*                                 OCC                                    *)
(*                                                                        *)
(**************************************************************************)
type symbol = 
  {
    name: string;
    decl: Node.node;
  }

type symbol_tag =
  | Mark
  | Symbol of symbol 

let scopes : (symbol_tag list) ref = ref []
let symtab : (string, symbol) Hashtbl.t = Hashtbl.create 8 

let lookup_symbol name = 
  try Hashtbl.find symtab name 
  with Not_found -> 
    let sym = {name= name; decl= Node.Nil;} in
    Hashtbl.add symtab name sym ; sym






