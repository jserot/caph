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

type signness = Signed | Unsigned 

let show_signness = ref false

type t = 
    CInt of int * signness option * int option (* value, signness, size (in bits) *)
  | CBool of bool
  | CFloat of float

let rec string_of_const = function
  | CInt (v,sg,_) -> string_of_int v ^ string_of_signness sg
  | CBool v -> string_of_bool v
  | CFloat v -> string_of_float v

and string_of_signness s = match !show_signness, s with
  false, _ -> ""
| true, None -> ""
| true, Some Signed -> "S"
| true, Some Unsigned -> "U"
