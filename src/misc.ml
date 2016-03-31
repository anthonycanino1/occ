(**************************************************************************)
(*                                                                        *)
(*                                 OCC                                    *)
(*                                                                        *)
(**************************************************************************)

let create_hashtable size init = 
  let tbl = Hashtbl.create size in
  List.iter (fun (k, v) -> Hashtbl.add tbl k v) init;
  tbl
