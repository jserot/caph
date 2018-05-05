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

open Printf
open Genmake
open Static

let dump_qip_file fname files =
  let oc = open_out fname in
  fprintf oc "set module_files {\n";
  List.iter 
    (function f -> Printf.fprintf oc "  %s\n" f)
    files;
  fprintf oc "}\n";
  close_out oc;
  Printf.printf "Wrote file %s\n" fname

let rec dump_xml_file name fname sp src_files =
  let oc = open_out fname in
  fprintf oc "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n";
  fprintf oc "<process name=\"%s\" description=\"\">\n" name;
  fprintf oc "  <files>\n";
  List.iter
    (function f ->
      fprintf oc "    <file name=\"%s\" path=\"%s\" type=\"%s\"/>\n"
        (Filename.basename f)
        f
        (Misc.get_suffix f))
    src_files;
  let streams, ports =
    List.fold_left 
      (fun (ss,ps) (_,b) -> match b.b_tag with
      | InpB Syntax.StreamIO -> ((b.b_name,"in",Vhdl.num_size_of_type b.b_typ)::ss), ps
      | OutB Syntax.StreamIO -> ((b.b_name,"out",Vhdl.num_size_of_type b.b_typ)::ss), ps
      | InpB Syntax.PortIO -> (ss,(b.b_name,"in",Vhdl.num_size_of_type b.b_typ)::ps)
      | OutB Syntax.PortIO -> (ss,(b.b_name,"out",Vhdl.num_size_of_type b.b_typ)::ps)
      | _ -> (ss,ps))
      ([],[])
      sp.boxes in
  fprintf oc "  </files>\n";
  fprintf oc "  <stream>\n";
  List.iter 
    (function (id,dir,sz) ->
      fprintf oc "    <flow name=\"%s\" type=\"%s\" size=\"%d\"/>\n" id dir sz)
    streams;
  fprintf oc "  </stream>\n";
  fprintf oc "  <ports>\n";
  List.iter 
    (function (id,dir,sz) ->
      fprintf oc "    <flow name=\"%s\" type=\"%s\" size=\"%d\"/>\n" id dir sz)
    ports;
  fprintf oc "  </ports>\n";
  fprintf oc "</process>\n";
  close_out oc;
  Printf.printf "Wrote file %s\n" fname

let dump_files name tp =
  let _,files = List.partition
      (function f -> filename_contains "tb" f)
      (!(target.vhdl_files) @ !(target.vhdl_extra_files)) in
  let qip_name = name ^ ".qip" in
  let qip_file = Misc.prefix_dir Genmake.target.Genmake.dir qip_name in
  dump_qip_file qip_file files;
  let xml_name = name ^ ".xml" in
  let xml_file = Misc.prefix_dir Genmake.target.Genmake.dir xml_name in
  dump_xml_file name xml_file tp (qip_name :: files)
