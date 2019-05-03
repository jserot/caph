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

(* DIF formatted output *)

open Misc
open Printf
open Ssval
open Static
open Interm
open Arrays

type dif_config = {
  node_prefix: string;
  edge_prefix: string;
  src_ifile: string;
  snk_ofile: string;
  }

let cfg = {
  node_prefix = "n";
  edge_prefix = "e";
  src_ifile = "ifile";
  snk_ofile = "ofile";
}

let node_id i = cfg.node_prefix ^ string_of_int i
let edge_id i = cfg.edge_prefix ^ string_of_int i

let string_of_val v =
  match v with 
    Expr.Val_int (i,_) -> string_of_int i
  | Expr.Val_bool b -> string_of_bool b
  | Expr.Val_float b -> string_of_float b
  | _ -> "?"

let string_of_node (i,b) = match b.ib_tag with
    Static.InpB _
  | Static.OutB _ 
  | Static.RegularB -> node_id i 
  | Static.DummyB -> "_" ^ string_of_int i (* should not happen *)

let string_of_edge (wid,(((s,ss),(d,ds)),ty)) =
  edge_id wid ^ "(" ^ node_id s ^ "," ^ node_id d ^ ")"
(* Q : how are I/O slot indexes handled in DIF ?? *)
(*   sprintf "e%d(n%d:s%d -> n%d:e%d)" s ss d ds *)

let output_parameter oc (name, gc) = 
  fprintf oc "    %s = %s;\n" name (string_of_val gc.gc_val)

let output_actor_type oc (name, a) =
  fprintf oc "  actortype %s {\n" a.ia_name;
  fprintf oc "    input %s;\n" (Misc.string_of_list (function (i,ty) -> i) ", " a.ia_ins);
  fprintf oc "    output %s;\n" (Misc.string_of_list (function (i,ty) -> i) ", " a.ia_outs);
  if a.ia_params <> [] then
    fprintf oc "    param %s;\n" (Misc.string_of_list (function (i,ty) -> i) ", " a.ia_params);
  fprintf oc "    }\n\n"

let output_io_type oc (i,b) = 
  match b.ib_tag with 
    Static.InpB _ ->
      let outp = "o" in
      fprintf oc "  actor type %s {\n" b.ib_name;
      fprintf oc "    output %s;\n" outp;
      fprintf oc "    param %s;\n" cfg.src_ifile;
(*       fprintf oc "    production %s:%d;\n" outp 1; *)
      fprintf oc "    }\n\n"
  | Static.OutB _  ->
      let inp = "i" in
      fprintf oc "  actor type %s {\n" b.ib_name;
      fprintf oc "    input %s;\n" inp;
      fprintf oc "    param %s;\n" cfg.snk_ofile;
(*       fprintf oc "    consumption %s:%d;\n" inp 1; *)
      fprintf oc "    }\n\n"
  |  _ ->
      ()
    
let output_actor oc (i,b) = 
  let string_of_param (n,(v,ty)) = n ^ "=" ^ string_of_val v in
  let string_of_inp (id, (wid,ty)) = edge_id wid ^ "->" ^ id  in
  let string_of_outp (id, (wids,ty)) = 
    let string_of_woutp wid = id ^ "->" ^ edge_id wid in
    Misc.string_of_list string_of_woutp ", " wids in
  fprintf oc "  actor %s {\n" (node_id i);
  fprintf oc "    type: %s;\n" b.ib_name;
  begin match b.ib_tag, b.ib_params with
    Static.InpB kind, _ -> fprintf oc "    param %s = \"%s\";\n" cfg.src_ifile b.ib_device;
  | Static.OutB kind, _ -> fprintf oc "    param %s = \"%s\";\n" cfg.snk_ofile b.ib_device;
  | Static.RegularB, params when params <> [] ->
      fprintf oc "    param %s;\n" (Misc.string_of_list string_of_param ", " b.ib_params);
  | _ -> ()
  end;
  fprintf oc "    interface %s;\n" (Misc.string_of_two_lists string_of_inp string_of_outp ", " b.ib_ins b.ib_outs);
  fprintf oc "    }\n\n"
 
let output ch name ir =
  fprintf ch "dif %s {\n\n" name;
  fprintf ch "  topology {\n";
  fprintf ch "    nodes = %s;\n" (Misc.string_of_list string_of_node ", " ir.ir_boxes);
  fprintf ch "    edges = %s;\n" (Misc.string_of_list string_of_edge ", " ir.ir_wires);
  fprintf ch "    }\n\n";
  if ir.ir_consts <> [] then begin
    fprintf ch "  parameter {\n";
    List.iter (output_parameter ch) ir.ir_consts;
    fprintf ch "    }\n\n"
    end;
  List.iter (output_io_type ch) ir.ir_boxes;
  List.iter (output_actor_type ch) ir.ir_actors;
  List.iter (output_actor ch) ir.ir_boxes;
  fprintf ch "}\n"

let output_ir name ir =
    let fname = Misc.prefix_dir Genmake.target.Genmake.dir name ^ ".dif" in
    let oc = open_out fname  in
    output oc name ir;
    Logfile.write fname;
    close_out oc
