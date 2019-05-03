(************************************************************************************)
(*                                                                                  *)
(*                                     CAPH                                         *)
(*                            http://caph.univ-bpclermont.fr                        *)
(*                                                                                  *)
(*                                  Jocelyn SEROT                                   *)
(*                         Jocelyn.Serot@univ-bpclermont.fr                         *)
(*                                                                                  *)
(*         Copyright 2011-2019 Jocelyn SEROT.  All rights reserved.                 *)
(*  This file is distributed under the terms of the Q Public License version 1.0.   *)
(*                                                                                  *)
(************************************************************************************)

(* The macro language *)

exception Undefined

type macro_val =
    M_Int of int * Const.signness option
  | M_String of string

let table = ref ([] : (string * macro_val option) list)

let get_value v = 
  try M_Int (int_of_string v, None)
  with Failure "int_of_string" ->
    let l = String.length v in
    begin match String.sub v 0 (l-1), String.sub v (l-1) 1 with
      p, "S" -> begin try M_Int (int_of_string p, Some Const.Signed) with Failure "int_of_string" -> M_String v end
    | p, "U" -> begin try M_Int (int_of_string p, Some Const.Unsigned) with Failure "int_of_string" -> M_String v end
    | _, _ -> M_String v
    end

let add_defn d = (* [d] is either "name" or "name=value" *)
  let k, v = 
    match Misc.split_string '=' d with
    [name] -> name, None
  | [name; value] -> name, Some (get_value value)  
  | _ -> failwith "Macro.add_defn" (* should not happen *) in
  table := (k, v) :: !table

let is_defined id = List.mem_assoc id !table

let lookup id = 
  try List.assoc id !table 
  with Not_found -> raise Undefined

