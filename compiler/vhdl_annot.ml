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

(* Reading annotation file *)
(* Minimum version - Mar 15, 2011 - JS *)

let parse fname = 
  let ic =
    try open_in fname
    with Sys_error _ -> Error.cannot_open_file fname in
  let res = ref [] in
  let lineno = ref 0 in
  try 
    while true do      (* Quick and dirty way *)
      let line = input_line ic in
      begin try
        ignore (Scanf.sscanf line "%s %s %s %d" (fun k p _ v -> res := (k,(p,v)) :: !res));
      with 
        _ -> Error.invalid_annot_file fname !lineno
      end;
      incr lineno
    done;
    !res
  with End_of_file ->
    !res
