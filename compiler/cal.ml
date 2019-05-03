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

(* Generation of RVC-CAL signatures for actors (needed for XDF output) *)

open Printf
open Interm

let rec string_of_type t  = 
  let open Types in 
  match real_type t with
  | Tconstr({tc_name="bool"}, _, _) -> "bool"
  | Tconstr({tc_name="float"}, _, _) -> "float"
  | Tconstr({tc_name="int"}, [sg], [sz]) ->
      begin match real_type sg, size_repr sz with
      | Tconstr({tc_name="_unsigned"},_,_), SzConst s -> "uint(size=" ^ string_of_int s ^ ")"
      | Tconstr({tc_name="_unsigned"},_,_), _ -> "uint"
      | Tconstr({tc_name="_signed"},_,_), SzConst s -> "int(size=" ^ string_of_int s ^ ")"
      | Tconstr({tc_name="_signed"},_,_), _ -> "int"
      | _, _ -> "int"
      end
  | ty -> Misc.not_implemented ("XDF translation of type " ^ Pr_type.string_of_type t)

let string_of_actor_param (id,ty) = string_of_type ty ^ " " ^ id 
let string_of_actor_io (id,ty) =  string_of_type ty ^ " " ^ id 

let dump_actor_sig name a =
  let fname = Misc.prefix_dir Genmake.target.Genmake.dir name ^ ".cal" in
  let oc = open_out fname  in
  fprintf oc "actor %s_act(%s)\n  %s\n   ==>\n  %s\n: end\n"
    name
    (Misc.string_of_list string_of_actor_param "," a.ia_params)
    (Misc.string_of_list string_of_actor_io "," a.ia_ins)
    (Misc.string_of_list string_of_actor_io "," a.ia_outs);
  Logfile.write fname;
  close_out oc

let dump_actor (id,a) =
  match a.ia_tvars, a.ia_svars with
  | [], [] -> dump_actor_sig a.ia_name a
  | _, _  -> Misc.not_implemented "CAL backend: type polymorphic actor"
