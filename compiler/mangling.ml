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

(* Name mangling - loosely inspired from http://www.ofb.net/gnu/gcc/gxxint_15.html *)

open Types

let rec string_of_type t  = match real_type t with
  | Tconstr({tc_name="bool"}, _, _) -> "b"
  | Tconstr({tc_name="float"}, _, _) -> "f"
  | Tconstr({tc_name="int"}, [sg], [sz]) -> 
      begin match real_type sg, size_repr sz with
      | Tconstr({tc_name="_unsigned"},_,_), SzConst s -> "u" ^ string_of_int s
      | Tconstr({tc_name="_unsigned"},_,_), _ -> "u"
      | Tconstr({tc_name="_signed"},_,_), SzConst s -> "s" ^ string_of_int s
      | Tconstr({tc_name="_signed"},_,_), _ -> "s"
      | _, _ -> "i"
      end
  | Tproduct ts -> Misc.string_of_list string_of_type "" ts
  | Tarrow (t1,t2) -> string_of_type t1 ^ "M" ^ string_of_type t2
  | Tconstr({tc_name="unit"}, [], []) -> ""
  | Tconstr({tc_name=c}, _, _) when Misc.string_is_prefix "_enum" c -> ""
  | Tconstr({tc_name="array"}, [ty], [sz]) when is_simple_type ty || is_array_of_simple_type ty ->
      "A" ^ string_of_size sz ^ string_of_type ty
  | Tconstr({tc_name=name}, ts, ss) ->
      "C" ^ name ^ Misc.string_of_list string_of_type "" ts ^ Misc.string_of_list string_of_size "x" ss
  | ty -> Misc.fatal_error ("Mangling.string_of_type: cannot mangle type " ^ Pr_type.string_of_type ty)

and string_of_size sz =
  match size_repr sz with
    SzConst n -> string_of_int n
  | _ -> failwith "Mangling.string_of_type: size variable"

let string_of_name name ts ss fs = 
  let ts' = List.filter is_ground_type ts in
  let ss' = List.filter is_ground_size ss in
  match ts', ss', fs with
    [], [], [] -> name
  | ts, ss, fs -> name ^ "_" ^ Misc.string_of_three_lists string_of_type string_of_size (function s -> s) "_" ts ss fs


