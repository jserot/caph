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

(* Preesm backend (.pi format) *)

open Misc
open Printf
open Ssval
open Static
open Interm
open Arrays

type preesm_config = {
    mutable xml_version: string;
    mutable xml_encoding: string;
    mutable code_prefix: string;
    mutable actors_num: bool;
    mutable use_floats: bool;
  }

let cfg = {
  xml_version = "1.0";
  xml_encoding = "UTF-8";
  code_prefix = "";
  actors_num = true;
  use_floats = false;
}

type preesm_type = 
  | Integer of bool * int option (* signed, width *)
  | Char of bool (* signed *)
  | Boolean
  | Float
  | Double
(* to be extended *)

let rec type_of t  = 
  let open Types in 
  match real_type t with
  | Tconstr({tc_name="bool"}, _, _) -> Boolean
  | Tconstr({tc_name="float"}, _, _) -> if cfg.use_floats then Float else Double
  | Tconstr({tc_name="int"}, [sg], [sz]) ->
      begin match real_type sg, size_repr sz with
      | Tconstr({tc_name="_unsigned"},_,_), SzConst s -> Integer (false, Some s)
      | Tconstr({tc_name="_signed"},_,_), SzConst s -> Integer (true, Some s)
      | Tconstr({tc_name="_signed"},_,_), _ -> Integer (true, None)
      | Tconstr({tc_name="_unsigned"},_,_), _ -> Integer (false, None)
      | _, _ -> Integer (false, None)
      end
  | ty -> Misc.not_implemented ("PREESM translation of type " ^ Pr_type.string_of_type t)

let string_of_type t  = 
  match type_of t with
  | Integer (false, Some 8) -> "unsigned char" (* special case *)
  | Integer (_, _) -> "int"
  | Char true -> "char"
  | Char false -> "unsigned char"
  | Boolean -> "bool"
  | Float -> "float"
  | Double -> "double"

let string_of_val v = match v with  
    Expr.Val_int (i,_) -> string_of_int i
  | Expr.Val_bool b -> string_of_bool b
  | Expr.Val_float b -> string_of_float b
  | _ -> Misc.not_implemented ("PREESM translation of value " ^ Expr.string_of_val v) 

let box_id i b = b.ib_name ^ (if cfg.actors_num then "_" ^ string_of_int i else "")

let lookup_box boxes bid = 
      try List.assoc bid boxes
      with Not_found -> Misc.fatal_error "Preesm.lookup_box"

let lookup_wire wires wid = 
      try List.assoc wid wires
      with Not_found -> Misc.fatal_error "Preesm.lookup_wire"

let is_param_box b = b.ib_tag = InpB Syntax.ParamIn

let output_actor_io oc dir is_config id ty =                            
  fprintf oc "          <param direction=\"%s\" isConfig=\"%s\" name=\"%s\" type=\"%s\"/>\n"
    dir (string_of_bool is_config) id (string_of_type ty)

let output_actor_inp oc is_param (id, (wid,ty)) =
  output_actor_io oc "IN" is_param id ty

let output_actor_outp oc (id, (wids,ty)) =
  output_actor_io oc "OUT" false (* TO FIX *) id ty

let output_actor_iinit oc (id, (wid,ty)) =
  fprintf oc "      <param direction=\"IN\" isConfig=\"true\" name=\"%s\" type=\"%s\"/>\n" id (string_of_type ty)

let output_actor_port oc dir is_param (id, (wid,ty)) =
  if is_param then
    fprintf oc "        <port kind=\"cfg_%s\" name=\"%s\"/>\n" dir id
  else
    fprintf oc "        <port kind=\"%s\" name=\"%s\" expr=\"\" annotation=\"NONE\"/>\n" dir id (* TO FIX *)

let output_actor oc ir (i,b) =
  match b.ib_tag with
  | RegularB ->
     let open Syntax in
     let id = box_id i b in 
     let incl_file, loop_fn, init_fn, period = 
       begin match List.assoc_opt "preesm" b.ib_impl with
       | Some [s1; s2; s3; s4] -> s1, s2, s3, s4
       | _ -> Misc.fatal_error "Preesm.output_actor: cannot retrieve implementation details"
       end in
     fprintf oc "    <node id=\"%s\" kind=\"actor\" period=\"0\">\n" id;
     fprintf oc "      <data key=\"graph_desc\">%s%s.h</data>\n" cfg.code_prefix incl_file;
     fprintf oc "      <loop name=\"%s\">\n" loop_fn;
     let is_param (id, (wid,ty)) = 
       let w = lookup_wire ir.ir_wires wid in
       is_param_box (Static.src_box_of_wire ir.ir_boxes w) in
     let param_ins, fifo_ins = List.partition is_param b.ib_ins in 
     List.iter (output_actor_inp oc true) param_ins;
     List.iter (output_actor_inp oc false) fifo_ins;
     List.iter (output_actor_outp oc) b.ib_outs;
     fprintf oc "      </loop>\n";
     if init_fn <> "" then  begin
         fprintf oc "      <init name=\"%s\">\n" init_fn;
         List.iter (output_actor_iinit oc) param_ins;
         fprintf oc "      </init>\n";
       end;
     List.iter (output_actor_port oc "input" true) param_ins;
     List.iter (output_actor_port oc "input" false) fifo_ins;
     List.iter (output_actor_port oc "output" false) b.ib_outs;
     fprintf oc "    </node>\n"
  | _ ->
      () 

let output_parameter oc (i,b) =
  match b.ib_tag, b.ib_ival with
  | InpB Syntax.ParamIn, Some v ->
     fprintf oc "    <node expr=\"%s\" id=\"%s\" kind=\"param\"/>\n" (string_of_val v) b.ib_name 
  | InpB Syntax.ParamIn, _ ->
     Misc.fatal_error "Preesm.output_parameter: no initial value"
  | _ -> ()

let output_connexion oc ir (wid,(((s,ss),(d,ds)),ty))=
  let mk_field name v = if v = "" then "" else Printf.sprintf "%s=\"%s\"" name v in
  let src_id, src_slot, kind, ty' =
    let b = lookup_box ir.ir_boxes s in
     match b.ib_tag with
      RegularB -> box_id s b, fst (List.nth b.ib_outs ss), "fifo", string_of_type ty
    | InpB Syntax.ParamIn -> box_id s b, "", "dependency", ""
    | _ -> Misc.fatal_error "Preesm.output_connexion: invalid source" in
  let dst_id, dst_slot =
    let b = lookup_box ir.ir_boxes d in
     match b.ib_tag with
      RegularB -> box_id d b, fst (List.nth b.ib_ins ds)
    | _ -> Misc.fatal_error "Preesm.output_connexion: invalid target" in
  fprintf oc "    <edge kind=\"%s\" source=\"%s\" %s target=\"%s\" targetport=\"%s\" %s/>\n"
    kind src_id (mk_field "sourceport" src_slot) dst_id dst_slot (mk_field "type" ty')

let output oc name ir = 
  fprintf oc "<?xml version=\"%s\" encoding=\"%s\"?>\n" cfg.xml_version cfg.xml_encoding;
  fprintf oc "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\">\n";
  fprintf oc "  <key attr.name=\"parameters\" for=\"graph\" id=\"parameters\"/>\n";
  fprintf oc "  <key attr.name=\"variables\" for=\"graph\" id=\"variables\"/>\n";
  fprintf oc "  <key attr.name=\"arguments\" for=\"node\" id=\"arguments\"/>\n";
  fprintf oc "  <key attr.name=\"name\" attr.type=\"string\" for=\"graph\"/>\n";
  fprintf oc "  <key attr.name=\"graph_desc\" attr.type=\"string\" for=\"node\"/>\n";
  fprintf oc "  <graph edgedefault=\"directed\">\n";
  fprintf oc "    <data key=\"name\">%s</data>\n" name;
  List.iter (output_parameter oc) ir.ir_boxes;
  List.iter (output_actor oc ir) ir.ir_boxes;
  List.iter (output_connexion oc ir) ir.ir_wires;
  fprintf oc "  </graph>\n";
  fprintf oc "</graphml>\n"

let output_ir name ir =
  let fname = Misc.prefix_dir Genmake.target.Genmake.dir name ^ ".pi" in
  let oc = open_out fname  in
  output oc name ir;
  Logfile.write fname;
  close_out oc
