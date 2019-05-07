(***************************************************************************************/
/*                                                                                     */
/*                This file is part of the CAPH Compiler distribution                  */
/*                            http://caph.univ-bpclermont.fr                           */
/*                                                                                     */
/*                                  Jocelyn SEROT                                      */
/*                         Jocelyn.Serot@univ-bpclermont.fr                            */
/*                                                                                     */
/*         Copyright 2011-2018 Jocelyn SEROT.  All rights reserved.                    */
/*  This file is distributed under the terms of the GNU Library General Public License */
/*      with the special exception on linking described in file ../LICENSE.            */
/*                                                                                     */
/***************************************************************************************)

(* Top-level Makefile generator - since 2.8.1 *)

open Printf

type t_args = {
  mutable prefix: string;
  mutable main_file: string;
  mutable proj_file: string;
  mutable caph_dir: string;
  mutable output_file: string;
  }

let args = {
  prefix="main";
  main_file="main.cph";
  proj_file="main.proj";
  caph_dir="";
  output_file="Makefile";
  }

let options_spec = [
"-main", Arg.String (function s -> args.main_file <- s), "set name of the top source file (default: main.cph)";
"-caph_dir", Arg.String (function s -> args.caph_dir <- s), "set path to the CAPH install directory (default: got from the CAPH environment variable)";
"-o", Arg.String (function s -> args.output_file <- s), "set name of the output file (default: Makefile)"
]

let usage = "usage: caphmake [-main fname] [-caph_dir path] [-o fname] [proj_file]"

let anonymous f = args.proj_file <- f

let dump_banner oc = 
  Printf.fprintf oc "## ###############################################################################\n";
  Printf.fprintf oc "## This file has been automatically generated with command :\n";
  Printf.fprintf oc "## %s\n" (Misc.string_of_list (function i -> i) " " (Array.to_list Sys.argv));
  Printf.fprintf oc "## ###############################################################################\n\n"

let dump_self_target oc =
  let cmd = Misc.string_of_list (fun i -> i) " " (Array.to_list Sys.argv) in
  fprintf oc "caphmake:\n";
  fprintf oc "\t%s\n" cmd
  
let dump_makef_target oc args target =
  let mfile = sprintf "./%s/Makefile" target in
  let abbrev = function "systemc" -> "SC" | s -> String.uppercase_ascii s in
  fprintf oc "%s.makefile: %s\n" target mfile;
  fprintf oc "%s: %s $(GEN_CAPH_SRCS)\n" mfile args.main_file;
  fprintf oc "\t$(CAPHC) -target_dir ./%s -I %s/lib/caph -make -%s $(%s_OPTS) %s\n" target args.caph_dir target (abbrev target) args.main_file;
  fprintf oc "\t@echo \"Wrote file %s\"\n" mfile;
  fprintf oc "\n"

let dump_dot_target oc args =
  let target = sprintf "./dot/%s.dot" args.prefix in
  fprintf oc "dot: %s\n\n" target;
  fprintf oc "%s: %s $(GEN_CAPH_SRCS)\n" target args.main_file;
  fprintf oc "\t$(CAPHC) -target_dir ./dot -prefix %s -I %s/lib/caph -dot $(DOT_OPTS) %s\n\n" args.prefix args.caph_dir args.main_file;
  fprintf oc "dot.show: %s\n" target;
  fprintf oc "\t$(GRAPHVIZ) %s\n\n" target

let dump_xdf_target oc args =
  let target = sprintf "./xdf/%s.xdf" args.prefix in
  fprintf oc "xdf: %s\n\n" target;
  fprintf oc "%s: %s $(GEN_CAPH_SRCS)\n" target args.main_file;
  fprintf oc "\t$(CAPHC) -target_dir ./xdf -prefix %s -I %s/lib/caph -xdf $(XDF_OPTS) %s\n\n" args.prefix args.caph_dir args.main_file

let dump_dif_target oc args =
  let target = sprintf "./dif/%s.dif" args.prefix in
  fprintf oc "dif: %s\n\n" target;
  fprintf oc "%s: %s $(GEN_CAPH_SRCS)\n" target args.main_file;
  fprintf oc "\t$(CAPHC) -target_dir ./dif -prefix %s -I %s/lib/caph -dif $(DIF_OPTS) %s\n\n" args.prefix args.caph_dir args.main_file

let dump_relay_target oc args target sub =
  fprintf oc "%s.%s:\n" target sub;
  fprintf oc "\t(cd %s; make %s CAPH=%s)\n" target sub args.caph_dir

let error msg = eprintf "** error: %s" msg; exit 1

let main () =
  Arg.parse options_spec anonymous usage;
  if args.caph_dir = "" then
      begin try args.caph_dir <- Sys.getenv "CAPH"
      with Not_found -> error "the CAPH environment variable is not set. Please set it or use the [-caph_dir] option\n"
      end;
  if args.proj_file = "" then error usage;
  if not (Sys.file_exists args.proj_file) then error ("cannot find file " ^ args.proj_file);
  if not (Sys.file_exists args.main_file) then error ("cannot find file " ^ args.main_file);
  try
    let oc = open_out args.output_file in
    dump_banner oc;
    args.prefix <- Filename.remove_extension (Filename.basename args.main_file);
    fprintf oc "include %s/lib/etc/Makefile.core\n\n" args.caph_dir;
    fprintf oc "-include %s\n\n" args.proj_file;
    dump_self_target oc;
    fprintf oc "\n";
    dump_dot_target oc args;
    fprintf oc ".PHONY: ./sim/Makefile ./systemc/Makefile ./vhdl/Makefile\n"; 
    fprintf oc "makefiles: %s\n\n" (Misc.string_of_list (function t -> t ^ ".makefile") " " ["sim"; "systemc"; "vhdl"]);
    List.iter (dump_makef_target oc args) ["sim"; "systemc"; "vhdl"];
    List.iter (dump_relay_target oc args "sim") ["run"; "show"; "save"; "clean"; "clobber"];
    fprintf oc "\n";
    List.iter (dump_relay_target oc args "systemc") ["code"; "exe"; "run"; "show"; "save"; "check"; "clean"; "clobber"];
    fprintf oc "\n";
    List.iter (dump_relay_target oc args "vhdl") ["bin"; "code"; "exe"; "run"; "show"; "save"; "check"; "viewtrace"; "viewvcdtrace"; "clean"; "clobber"];
    fprintf oc "\n";
    dump_xdf_target oc args;
    fprintf oc "\n";
    dump_dif_target oc args;
    fprintf oc "\n";
    fprintf oc "clean:\n";
    List.iter
      (function t -> fprintf oc "\t@if [ -e %s/Makefile ]; then (cd %s; make clean CAPH=%s); fi\n" t t args.caph_dir)
      ["sim"; "systemc"; "vhdl"];
    fprintf oc "\t@$(RM) caph.output\n";
    fprintf oc "\t@$(RM) *_mocs.dat\n";
    fprintf oc "\t@$(RM) dot/*\n";
    fprintf oc "\t@$(RM) xdf/*\n";
    fprintf oc "\t@$(RM) dif/*\n";
    fprintf oc "\t@$(RM) $(GEN_CAPH_SRCS)\n";
    fprintf oc "\n";
    fprintf oc "clobber: clean\n";
    List.iter (function t -> fprintf oc "\t@if [ -e %s/Makefile ]; then (cd %s; make clobber CAPH=%s); fi\n" t t args.caph_dir) ["sim";"systemc";"vhdl"];
    List.iter (function t -> fprintf oc "\t@$(RM) Makefile.%s\n" t) ["sim"; "systemc"; "vhdl"];
    fprintf oc "\t@$(RM) $(GEN_CAPH_SRCS)\n";
    fprintf oc "\t@$(RM) fifo_stats.dat io_monitor.dat\n";
    fprintf oc "\t@$(RMDIR) xdf dif\n";
    fprintf oc "\t@$(RM) *~\n";
    close_out oc;
    printf "Wrote file %s\n" args.output_file
  with
  | Sys.Break -> flush stderr; exit 5
  | Sys_error msg ->
     eprintf "Input/output error: %s.\n" msg;
     flush stderr;
     exit 6
  | e ->
     eprintf "Internal error: %s.\n" (Printexc.to_string e);
     flush stderr; exit 7

let _ = Printexc.print main ()
