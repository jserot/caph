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
  mutable main: string;
  mutable proj: string;
  mutable caph_dir: string;
  mutable output_file: string;
  }

let args = {
  main="main";
  proj="";
  caph_dir="";
  output_file="Makefile";
  }

let options_spec = [
"-main", Arg.String (function s -> args.main <- s), "set name of the top source file (default: main)";
"-caph_dir", Arg.String (function s -> args.caph_dir <- s), "set path to the CAPH install directory (default: got from the CAPH environment variable)";
"-o", Arg.String (function s -> args.output_file <- s), "set name of the output file (default: Makefile)"
]

let usage = "usage: caphmake [-main name] [-caph_dir path] [-o fname]"

let anonymous = function _ -> ()

let dump_banner oc = 
  Printf.fprintf oc "## ###############################################################################\n";
  Printf.fprintf oc "## This file has been automatically generated with command :\n";
  Printf.fprintf oc "## %s\n" (Misc.string_of_list (function i -> i) " " (Array.to_list Sys.argv));
  Printf.fprintf oc "## ###############################################################################\n\n"

let dump_makef_target oc args target =
  let abbrev = function "systemc" -> "SC" | s -> String.uppercase_ascii s in
  fprintf oc "%s.makefile: Makefile.%s\n" target target;
  fprintf oc "Makefile.%s: %s.cph $(GEN_CAPH_SRCS)\n" target args.main;
  fprintf oc "\t$(CAPHC) -I %s/lib/caph -make -%s $(%s_OPTS) %s.cph\n" args.caph_dir target (abbrev target) args.main;
  fprintf oc "\t@echo \"Wrote file Makefile.%s\"\n" target;
  fprintf oc "\n"

let dump_dot_target oc args =
  fprintf oc "dot: %s.dot\n\n" args.proj;
  fprintf oc "%s.dot: %s.cph $(GEN_CAPH_SRCS)\n" args.proj args.main;
  fprintf oc "\t$(CAPHC) -I %s/lib/caph -dot $(DOT_OPTS) %s.cph\n\n" args.caph_dir args.main;
  fprintf oc "dot.show: %s.dot\n" args.proj;
  fprintf oc "\t$(GRAPHVIZ) %s.dot\n\n" args.proj

let dump_xdf_target oc args =
  fprintf oc "xdf: %s.xdf\n\n" args.main;
  fprintf oc "%s.xdf: %s.cph $(GEN_CAPH_SRCS)\n" args.main args.main;
  fprintf oc "\t$(CAPHC) -I %s/lib/caph -xdf $(XDF_OPTS) %s.cph\n\n" args.caph_dir args.main

let dump_dif_target oc args =
  fprintf oc "dif: %s.dif\n\n" args.main;
  fprintf oc "%s.dif: %s.cph $(GEN_CAPH_SRCS)\n" args.main args.main;
  fprintf oc "\t$(CAPHC) -I %s/lib/caph -dif $(DIF_OPTS) %s.cph\n\n" args.caph_dir args.main

let dump_relay_target oc args target sub =
  fprintf oc "%s.%s:\n" target sub;
  fprintf oc "\tmake -f Makefile.%s %s CAPH=%s\n" target sub args.caph_dir

let main () =
  Arg.parse options_spec anonymous usage;
  if args.caph_dir = "" then
      begin try args.caph_dir <- Sys.getenv "CAPH"
      with Not_found ->
        eprintf "** error: the CAPH environment variable is not set. Please set it or use the [-caph_dir] option\n";
        exit 1
      end;
  args.proj <- Filename.basename (Sys.getcwd ());
  try
  let oc = open_out args.output_file in
  dump_banner oc;
  fprintf oc "include %s/lib/etc/Makefile.core\n\n" args.caph_dir;
  fprintf oc "-include %s.proj\n\n" (Filename.basename (Sys.getcwd ()));
  dump_dot_target oc args;
  fprintf oc "makefiles: %s\n\n" (Misc.string_of_list (function t -> t ^ ".makefile") " " ["sim"; "systemc"; "vhdl"]);
  List.iter (dump_makef_target oc args) ["sim"; "systemc"; "vhdl"];
  List.iter (dump_relay_target oc args "sim") ["run"; "show"; "save"; "clean"; "clobber"];
  fprintf oc "\n";
  List.iter (dump_relay_target oc args "systemc") ["code"; "exe"; "run"; "show"; "save"; "check"; "clean"; "clobber"];
  fprintf oc "\n";
  List.iter (dump_relay_target oc args "vhdl") ["code"; "exe"; "run"; "show"; "save"; "check"; "viewtrace"; "viewvcdtrace"; "clean"; "clobber"];
  fprintf oc "\n";
  dump_xdf_target oc args;
  fprintf oc "\n";
  dump_dif_target oc args;
  fprintf oc "\n";
  fprintf oc "clean:\n";
  List.iter
    (function t -> fprintf oc "\tif [ -e Makefile.%s ]; then make -f Makefile.%s clean CAPH=%s; fi\n" t t args.caph_dir)
    ["sim"; "systemc"; "vhdl"];
  fprintf oc "\t$(RM) caph.output\n";
  fprintf oc "\t$(RM) %s.dot %s_expanded.dot *_act_*.dot *_mocs.dat\n" args.proj args.proj;
  fprintf oc "\t$(RM) *.xdf *.cal\n";
  fprintf oc "\t$(RM) $(GEN_CAPH_SRCS)\n";
  fprintf oc "\n";
  fprintf oc "clobber: clean\n";
  List.iter (function t -> fprintf oc "\tif [ -e Makefile.%s ]; then make -f Makefile.%s clobber CAPH=%s; fi\n" t t args.caph_dir) ["sim";"systemc";"vhdl"];
  List.iter (function t -> fprintf oc "\t$(RM) Makefile.%s\n" t) ["sim"; "systemc"; "vhdl"];
  fprintf oc "\t$(RM) $(GEN_CAPH_SRCS)\n";
  fprintf oc "\t$(RM) *_sdf_fifo_sizes.dat\n";
  fprintf oc "\t$(RMDIR) dot sim systemc vhdl xdf\n";
  fprintf oc "\t$(RM) *~\n";
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
