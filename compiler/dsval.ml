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

(* Dynamic semantic values *)

open Syntax
open Expr

type pid = int
type cid = int

type ds_val = Expr.e_val
  (* Strictly speaking, the two semantics domains should be distinct; for ex, the value [Val_ignored] should normally
     be part of [ds_val] but not of [e_val]. But merging them simplifies coding *)

and channel = {
    mutable ch_traced: bool;
    ch_stat: chan_stat;
    ch_cap: int;     (* capacity (fifo size) *)
    mutable ch_val: ds_val list }

and chan_stat = {
    mutable max_occ: int (* Max number of accumulated tokens *)
    (* TODO : more indicators .... *)
  }

(* Printing *)

let string_of_chan_occ c = Printf.sprintf "occ=%d/%d" (List.length c.ch_val) c.ch_cap

let string_of_chan_stat cs = Printf.sprintf "max=%d" cs.max_occ
