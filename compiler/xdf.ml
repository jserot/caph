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

(* XDF formatted output *)

open Misc
open Printf
open Ssval
open Static
open Interm
open Arrays

type xdf_config = {
    mutable xml_version: string;
    mutable xml_encoding: string;
    mutable target_package: string;
  }

let cfg = {
  xml_version = "1.0";
  xml_encoding = "UTF-8";
  target_package = "";
}

type xdf_type = 
    Integer of int
  | Boolean
  | Real
(* to be extended *)

let rec type_of t  = 
  let open Types in 
  match real_type t with
  | Tconstr({tc_name="bool"}, _, _) -> Boolean
  | Tconstr({tc_name="float"}, _, _) -> Real
  | Tconstr({tc_name="int"}, [sg], [sz]) as ty ->
      begin match real_type sg, size_repr sz with
      | Tconstr({tc_name="_unsigned"},_,_), SzConst s -> Integer s
      | Tconstr({tc_name="_signed"},_,_), SzConst s -> Integer s
      | _, _ -> Misc.not_implemented ("XDF translation of type " ^ Pr_type.string_of_type ty)
      end
  | ty -> Misc.not_implemented ("XDF translation of type " ^ Pr_type.string_of_type t)

let output_io_port oc dir name ty = 
  let open Types in
  fprintf oc "<Port kind=\"%s\" name=\"%s\">\n" dir name;
  begin match type_of ty with
  | Integer s ->
      fprintf oc "    <Type name=\"int\">\n";
      fprintf oc "        <Entry kind=\"Expr\" name=\"size\">\n";
      fprintf oc "            <Expr kind=\"Literal\" literal-kind=\"Integer\" value=\"%d\"/>\n" s;
      fprintf oc "        </Entry>\n";
      fprintf oc "    </Type>\n";
  | Boolean ->
      fprintf oc "    <Type name=\"bool\">\n";
      fprintf oc "    </Type>\n"
  | Real ->
      fprintf oc "    <Type name=\"real\">\n";
      fprintf oc "    </Type>\n"
  end;
  fprintf oc "</Port>\n"

let output_port oc (i,b) =
  match b.ib_tag with
    InpB Syntax.StreamIO -> output_io_port oc "Input" b.ib_name b.ib_typ
  | OutB Syntax.StreamIO ->output_io_port oc "Output" b.ib_name b.ib_typ
  | _ -> ()

let string_of_type t  = 
  match type_of t with
    Integer s -> "Integer"
  | Boolean -> "Boolean"
  | Real -> "Real"

let string_of_val v = match v with  
    Expr.Val_int (i,_) -> string_of_int i
  | Expr.Val_bool b -> string_of_bool b
  | Expr.Val_float b -> string_of_float b
  | _ -> Misc.not_implemented ("XDF translation of value " ^ Expr.string_of_val v)

let output_inst_param oc (name,(v,ty)) =
  fprintf oc "  <Parameter name=\"%s\">\n" name; 
  fprintf oc "    <Expr kind=\"Literal\" literal-kind=\"%s\" value=\"%s\"/>\n" (string_of_type ty) (string_of_val v);
  fprintf oc "  </Parameter>\n"

let box_id i b = b.ib_name ^ "_act_" ^ string_of_int i

let full_actor_name n = match cfg.target_package with
    "" -> n
  | p -> p ^ "." ^ n ^ "_act"
       
let output_instance oc (i,b) =
  match b.ib_tag with
  | RegularB ->
      fprintf oc "<Instance id=\"%s\">\n" (box_id i b);
      fprintf oc "  <Class name=\"%s\"/>\n" (full_actor_name b.ib_name);
      List.iter (output_inst_param oc) b.ib_params;
      fprintf oc "</Instance>\n"
  | _ ->
      () 

let read_annot_file fname =
  let ic = open_in fname  in
  close_in ic
  
let output_connexion oc anns boxes (wid,(((s,ss),(d,ds)),ty))=
  let lookup bid = 
      try List.assoc bid boxes
      with Not_found -> Misc.fatal_error "Xdf.output_connexion" in
  let src_id, src_slot =
    let b = lookup s in
     match b.ib_tag with
      RegularB -> box_id s b, fst (List.nth b.ib_outs ss)
    | InpB Syntax.StreamIO -> "", b.ib_name
    | _ -> Misc.fatal_error "Xdf.output_connexion" in
  let dst_id, dst_slot =
    let b = lookup d in
     match b.ib_tag with
      RegularB -> box_id d b, fst (List.nth b.ib_ins ds)
    | OutB Syntax.StreamIO -> "", b.ib_name 
    | _ -> Misc.fatal_error "Xdf.output_connexion" in
  fprintf oc "<Connection dst=\"%s\" dst-port=\"%s\" src=\"%s\" src-port=\"%s\">\n" dst_id dst_slot src_id src_slot;
  begin try
      match List.assoc ("w" ^ string_of_int wid) anns with
      | "fifo_size", sz -> 
          fprintf oc "  <Attribute kind=\"Value\" name=\"bufferSize\">\n";
          fprintf oc "    <Expr kind=\"Literal\" literal-kind=\"Integer\" value=\"%d\"/>\n" sz;
          fprintf oc "  </Attribute>\n"
      | _, _ -> ()
  with
    Not_found -> ()
  end;
  fprintf oc "</Connection>\n"

let output oc name ir = 
  let anns =
    let fname = Systemc.cfg.Systemc.sc_fifo_stats_file in
    if Sys.file_exists fname then begin
      Printf.printf "Reading annotation file %s\n" fname;
      Vhdl_annot.parse fname
      end
    else
      [] in
  fprintf oc "<?xml version=\"%s\" encoding=\"%s\"?>\n" cfg.xml_version cfg.xml_encoding;
  fprintf oc "<XDF name=\"%s\">\n" name; 
  List.iter (output_port oc) ir.ir_boxes;
  List.iter (output_instance oc) ir.ir_boxes;
  List.iter (output_connexion oc anns ir.ir_boxes) ir.ir_wires;
  fprintf oc "</XDF>\n"

let output_ir name ir =
  let fname = Misc.prefix_dir Genmake.target.Genmake.dir name ^ ".xdf" in
  let oc = open_out fname  in
  output oc name ir;
  Logfile.write fname;
  close_out oc
