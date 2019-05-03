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

(* Dynamic loading of external fns for the interpreter *)

let cmo_files = ref []

type func = int list -> int

let the_functions = ref ([] : (string * func) list)

let register name fn = the_functions := (name,fn) :: !the_functions

let load_file f = 
  try 
    Dynlink.loadfile f;
    Printf.printf "** Dynamically loaded file %s\n" f
  with 
    Dynlink.Error e -> Error.dynload_error (Dynlink.error_message e)

let init path = List.iter load_file !cmo_files

let call fname args =
  let f = 
    try List.assoc fname !the_functions 
    with Not_found -> Error.unknown_extern_function fname !cmo_files in
  f args
