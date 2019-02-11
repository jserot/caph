(************************************************************************************)
(*                                                                                  *)
(*                                     CAPH                                         *)
(*                            http://caph.univ-bpclermont.fr                        *)
(*                                                                                  *)
(*                                  Jocelyn SEROT                                   *)
(*                         Jocelyn.Serot@univ-bpclermont.fr                         *)
(*                                                                                  *)
(*         Copyright 2011-2018 Jocelyn SEROT.  All rights reserved.                 *)
(*  This file is distributed under the terms of the Q Public License version 1.0.   *)
(*                                                                                  *)
(************************************************************************************)

(* Minimal support for range patterns in filenames. Ex: "foo_[1-3].txt". *)

type t = 
    Litteral of string
  | Range of string * int * int * string (* prefix, range, suffix *)

let check s =
  let r = Str.regexp "\\[\\([0-9]+\\)-\\([0-9]+\\)\\]" in
  try
    let _ = Str.search_backward r s (String.length s - 1) in
    let p1 = Str.match_beginning () in
    let p2 = Str.match_end () in
    Range(
      String.sub s 0 p1,
      int_of_string (Str.matched_group 1 s),
      int_of_string (Str.matched_group 2 s),
      String.sub s p2 (String.length s - p2))
  with
  | Invalid_argument _
  | Not_found ->
    Litteral s

let expand s = match check s with
    Litteral s -> [s]
  | Range (pfx,lo,hi,sfx) -> List.map (function i -> pfx ^ string_of_int i ^ sfx) (Misc.range lo hi)
