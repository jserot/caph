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

(* DOT formatted output *)

open Misc
open Printf
open Ssval
open Static
open Interm
open Arrays

type dot_config = {
    mutable labeled_edges: bool;
    mutable show_indexes: bool;
    mutable stream_io_box_shape: string;
    mutable port_io_box_shape: string;
    mutable param_box_shape: string;
    mutable slotted_boxes: bool;
    mutable show_wire_annots: bool
  }

let cfg = {
  labeled_edges = true;
  show_indexes = false;
  stream_io_box_shape = "invtriangle";
  port_io_box_shape = "hexagon";
  param_box_shape = "hexagon";
  slotted_boxes = true;
  show_wire_annots = false
}

let max_array_disp_size = ref 16

let rec string_of_params b = match b.ib_params with
    [] -> ""
  | ps -> "(" ^ Misc.string_of_list (function (name,(v,ty)) -> string_of_val v) "," ps ^  ")"

and string_of_val v =
  let clip s = if String.length s > !max_array_disp_size then "..." else s in
  match v with  (* Special version for DOT since node labels do not support any chars :( *)
    Expr.Val_int (i,_) -> string_of_int i
  | Expr.Val_bool b -> string_of_bool b
  | Expr.Val_float b -> string_of_float b
  | Expr.Val_array1 (n,vs) -> clip (Array1.to_string string_of_val vs)
  | Expr.Val_array2 (n,vs) -> clip (Array2.to_string string_of_val vs)
  | Expr.Val_array3 (n,vs) -> clip (Array3.to_string string_of_val vs)
  | Expr.Val_prim _
  | Expr.Val_fun (_,_,_)
  | Expr.Val_extern _ -> "(fun)"
  | _ -> "?"

let string_of_fparams b = match b.ib_fpbs with
    [] -> ""
  | ps -> "(" ^ Misc.string_of_list (function (_,fid) -> fid) "," ps ^  ")"

let string_of_io_kind = function
    Syntax.StreamIO -> cfg.stream_io_box_shape
  | Syntax.PortIO -> cfg.port_io_box_shape
  | Syntax.ParamIn -> cfg.param_box_shape

let output_box ch (i,b) =
  let ioslots c n =
    let rec h = function
        0 -> ""
      | 1 -> "<" ^ c ^ string_of_int (n-1) ^ ">"
      | i -> "<" ^ c ^ string_of_int (n-i) ^ ">|" ^ h (i-1) in
    h n in
  let bid =
    if cfg.show_indexes
    then string_of_int i ^ ":" ^ b.ib_name
    else b.ib_name in
  match b.ib_tag with
    InpB kind ->
      fprintf ch "n%d [shape=%s,label=\"%s\"];\n" i (string_of_io_kind kind) bid
  | OutB kind ->
      fprintf ch "n%d [shape=%s,label=\"%s\"];\n" i (string_of_io_kind kind) bid
  | RegularB ->
      if cfg.slotted_boxes then
        fprintf ch "n%d [shape=record,style=rounded,label=\"<id>%s%s%s|{{%s}|{%s}}\"];\n"
          i
          bid
          (string_of_fparams b)
          (string_of_params b)
          (ioslots "e" (List.length b.ib_ins))
          (ioslots "s" (List.length b.ib_outs))
      else
        fprintf ch "n%d [shape=box,style=rounded,label=\"%s%s\"];\n" i  bid (string_of_params b) 
  | DummyB ->
      fprintf ch "n%d [shape=box,style=dotted,label=\"%s\"];\n" i "dummy"

let string_of_wtype ty = ":" ^ Pr_type.string_of_type ty

let output_wire ch wire_annots (wid,(((s,ss),(d,ds)),ty))=
  let wire_annot wid =
    try Interm.string_of_wire_annot (List.assoc wid wire_annots)
    with Not_found -> "" in
  match cfg.labeled_edges, cfg.show_indexes, cfg.show_wire_annots with
  | true, _, true -> fprintf ch "n%d:s%d -> n%d:e%d [label=\" w%d:%s\"];\n" s ss d ds wid (wire_annot wid)
  | true, true, false -> fprintf ch "n%d:s%d -> n%d:e%d [label=\" %s\"];\n" s ss d ds ("w" ^ string_of_int wid ^ string_of_wtype ty)
  | true, false, false -> fprintf ch "n%d:s%d -> n%d:e%d [label=\" %s\"];\n" s ss d ds (string_of_wtype ty)
  | false, _, _ -> fprintf ch "n%d:s%d -> n%d:e%d;\n" s ss d ds

let output ch fifo_sizes boxes wires = 
  fprintf ch "digraph g {\n";
(*   fprintf ch "node [shape=record]\n"; *)
  List.iter (output_box ch) boxes;
  List.iter (output_wire ch fifo_sizes) wires;
  fprintf ch "}\n"

let output_ir name ir =
    let fname = Misc.prefix_dir Genmake.target.Genmake.dir name ^ ".dot" in
    let oc = open_out fname  in
    output oc ir.ir_wire_annots ir.ir_boxes ir.ir_wires;
    Logfile.write fname;
    close_out oc
