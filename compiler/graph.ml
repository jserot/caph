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

(* A minimalistic Graph module for use with the [Sdf] module *)

type node_id = int
type edge_id = int 

type ('a, 'b) t = (node_id * 'a node_desc list)

and 'a node_desc = {
  n_desc: 'a;
  n_inps: (node_id * 'b edge_desc) list;
  n_outps: (node_id * 'b edge_desc) list;
  mutable n_icnt: int;  (* used for topological sorting *)
  }

and 'b edge_desc = {
  e_desc: 'b;
  e_src: node_id;
  e_dst: node_id
}

