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

open Printf

let safe_mode = ref false

type safe_config = {
    mutable allowed_ifiles: string list;
    mutable allowed_ofiles: string list
  }

let safe_cfg = {
  allowed_ifiles = [];
  allowed_ofiles = []
}

let suffix = ref "makefile"

type target_desc = {
  mutable dir: string;
  mutable proj_file: string;
  mutable main_file: string;
  mutable prefix: string;
  h_files: string list ref;
  cpp_files: string list ref;
  vhdl_files: string list ref;
  sim_extra_files: string list ref;
  h_extra_files: string list ref;
  cpp_extra_files: string list ref;
  vhdl_extra_files: string list ref;
  input_files: file_desc list ref;
  output_files: file_desc list ref;
  input_type_converters: (string * Types.typ) list ref;
  output_type_converters: (string * Types.typ) list ref;
  other_files: string list ref; (* to be removed ? *)
}

and file_desc  = {
  mutable orig_name: string;     (* ex: f1.txt, f2.pgm, ... *)
  mutable actual_name: string;   (* ex: f1.txt, f2.txt, f3.bin, ... *)
  mutable typ: Types.typ;
  mutable converter: file_converter
  }

and file_converter = (file_fmt * file_fmt) option

and file_fmt = FF_Txt | FF_Pgm | FF_Bin

type genmake_config = {
  mutable tb_inline_io: bool (* This duplicates the corresponding field [Vhdl.cfg] to avoid circular build dependency *)
  }

let cfg = {
  tb_inline_io = false
  }
let add_to_target files file = if not (List.mem file !files) then files := file :: !files 

let target = {
  dir = ".";
  main_file = "";
  proj_file = "";
  prefix = "";
  h_files = ref [];
  cpp_files = ref [];
  vhdl_files = ref [];
  sim_extra_files = ref [];
  h_extra_files = ref [];
  cpp_extra_files = ref [];
  vhdl_extra_files = ref [];
  input_files = ref [];
  output_files = ref [];
  input_type_converters = ref [];
  output_type_converters = ref [];
  other_files = ref []
}

let caph_dir = try Sys.getenv "CAPH" with Not_found -> ""

let mk_file_spec (fname,ty) = { orig_name=fname; actual_name=fname; typ=ty; converter=None }

let add_io_file files (fname,ty) =
  List.iter (function f -> add_to_target files (mk_file_spec (f,ty))) (Filepat.expand fname);
  Misc.change_extension "txt" fname

let add_input_file = add_io_file target.input_files
let add_output_file = add_io_file target.output_files

let add_target ?(extra=false) filename =
  let fname = Filename.basename filename in
  if Filename.check_suffix fname ".vhd" then begin
    if extra then add_to_target target.vhdl_extra_files fname
    else add_to_target target.vhdl_files fname
    end
  else if Filename.check_suffix fname ".h" then begin
    if extra then add_to_target target.h_extra_files fname
    else add_to_target target.h_files fname
    end
  else if Filename.check_suffix fname ".cpp" then begin
    if extra then add_to_target target.cpp_extra_files fname
    else add_to_target target.cpp_files fname
    end
  else
    add_to_target target.other_files fname

(* let string_of_targets (f,ty) = Misc.string_of_list (function f' -> f') " " (Fileglob.expand f) *)

(* let to_txt_file (f,ty) = Misc.change_extension "txt" f, ty *)

let full_path f = Misc.prefix_dir target.dir f

let convert_input_file f = 
  match Misc.get_extension f.orig_name, Types.real_type f.typ with
    "txt", _ -> ()
  | "pgm", ty -> 
      f.actual_name <- Misc.change_extension "txt" f.orig_name;
      f.converter <- Some (FF_Pgm,FF_Txt)
  | ext, ty ->
      Misc.not_implemented ("Genmake.convert_input_file: cannot handle file " ^ f.orig_name)

let convert_output_file f = 
  match Misc.get_extension f.orig_name, Types.real_type f.typ with
    "txt", _ -> ()
  | "pgm", ty -> 
      f.actual_name <- Misc.change_extension "txt" f.orig_name;
      f.converter <- Some (FF_Txt,FF_Pgm)
  | ext, ty ->
      Misc.not_implemented ("Genmake.convert_output_file: cannot handle file " ^ f.orig_name)

let dump_inp_file_target oc f =
  match f.converter with
    Some (FF_Pgm,FF_Txt) -> 
      let f' = if target.dir <> "." then "../" ^ f.orig_name else f.orig_name in
      fprintf oc "%s: %s\n" f.actual_name f';
      fprintf oc "\t$(PGM2TXT) -abbrev $< $@\n"
  | None ->
     if target.dir <> "." then begin
      let f = f.orig_name in 
      let f' = "../" ^ f in  (* TO FIX *) 
      fprintf oc "%s: %s\n" f f';
      fprintf oc "\t$(LN) %s %s\n" f' f
      end
  | _ ->
      Error.invalid_input_data_file f.orig_name

let rec depth_of_type ty = 
  let default_pgm_depth = 255 in
  let open Types in
  match real_type ty with
  | Tconstr({tc_name="bool"}, _, _) -> 1
  | Tconstr({tc_name="float"}, _, _) -> default_pgm_depth
  | Tconstr({tc_name="int"}, [sg], [sz]) ->
      begin match real_type sg, size_repr sz with
      | Tconstr({tc_name="_unsigned"},_,_), SzConst s -> Misc.pow2 s - 1
      | Tconstr({tc_name="_signed"},_,_), SzConst s -> Misc.pow2 s - 1 (* ?? *)
      | _, _ -> default_pgm_depth
      end
  | Tconstr({tc_name="dc"}, [ty'], _) -> depth_of_type ty' 
  | _ -> default_pgm_depth

let rec bitwidth_of_type ty = 
  let open Types in
  match real_type ty with
  | Tconstr({tc_name="bool"}, _, _) -> Some 1
  | Tconstr({tc_name="float"}, _, _) -> Some 32
  | Tconstr({tc_name="int"}, [sg], [sz]) -> begin match size_repr sz with SzConst s -> Some s | _ -> None end
  | Tconstr({tc_name="dc"}, [ty'], _) -> begin match bitwidth_of_type ty' with Some s -> Some (s+2) | None -> None end
  | _ -> None

let dump_outp_file_target oc f =
  match f.converter with
    Some (FF_Txt,FF_Pgm) -> 
      fprintf oc "%s: %s\n" f.orig_name f.actual_name;
      fprintf oc "\t$(TXT2PGM) -abbrev %d $< $@\n" (depth_of_type f.typ)
  | None ->
      ()
  | _ ->
      Error.invalid_input_data_file f.orig_name

let dump_show_cmd oc f =
  match Misc.get_extension f.orig_name with
    "txt" -> fprintf oc "\t@/bin/echo -n \"%s: \"\n\t@cat %s\n\t@/bin/echo \"\"\n" f.orig_name f.orig_name
  | "pgm" -> fprintf oc "\t$(DISPIMG) %s\n" f.orig_name
  |     _ -> Misc.not_implemented ("Genmake.dump_show_cmd: don't know how to show file " ^ f.orig_name)

let dump_check_cmd oc t0 f =
  let f' = "../" ^ t0 ^ "/" ^ f.orig_name in
  fprintf oc "\tif [ -e %s ]; then $(CHECK_DIFF) %s %s; fi\n" f' f.orig_name f'

let dump_sim_makefile () = 
  let fname = if target.dir = "." then "Makefile.sim" else target.dir ^ "/" ^ "Makefile" in
  let oc = open_out fname in
  Misc.dump_banner "#" oc;
  Misc.check_file target.proj_file;
  let src_file, proj_file =
    if target.dir = "." then target.main_file, target.proj_file
    else "../" ^ target.main_file, "../" ^ target.proj_file in  (* TO FIX *)
  if caph_dir <> "" then fprintf oc "CAPH=%s\n\n" caph_dir;
  fprintf oc "-include %s\n" proj_file;
  fprintf oc "include $(CAPH)/lib/etc/Makefile.core\n";
  fprintf oc "\n";
  fprintf oc ".PHONY: run view clean clobber\n";
  fprintf oc "\n";
  List.iter convert_input_file !(target.input_files);
  List.iter convert_output_file !(target.output_files);
  fprintf oc "run: %s %s %s\n"
    src_file
    (Misc.string_of_list (Misc.change_extension "cmo") " " !(target.sim_extra_files))
    (Misc.string_of_list (function f -> f.actual_name) " " !(target.input_files));
  fprintf oc "\t$(CAPHC) -sim -I $(CAPH)/lib/caph $(SIM_OPTS) %s\n" src_file;
  fprintf oc "\n";
  List.iter (dump_inp_file_target oc) !(target.input_files);
  fprintf oc "\n";
  List.iter (dump_outp_file_target oc) !(target.output_files);
  fprintf oc "\n";
  fprintf oc "show: %s\n" (Misc.string_of_list (function f -> f.orig_name) " " (!(target.output_files)));
  List.iter (dump_show_cmd oc) !(target.output_files);
  fprintf oc "\n";
  List.iter 
    (fun f ->
      fprintf oc "%s: %s\n" (Misc.change_extension "cmo" f) f;
	  fprintf oc "\t$(CAMLC) -c -I $(CAPH)/lib/ml $<\n")
    !(target.sim_extra_files);
  fprintf oc "\n";
  fprintf oc "clean:\n";
  List.iter (function f -> if f.actual_name <> f.orig_name then fprintf oc "\t@$(RM) %s\n" f.actual_name) !(target.input_files);
  List.iter (function f -> fprintf oc "\t@$(RM) %s\n\t@$(RM) %s\n" f.orig_name f.actual_name) !(target.output_files);
  List.iter (fun f -> fprintf oc "\t@$(RM) %s %s\n" (Misc.change_extension "cmi" f) (Misc.change_extension "cmo" f)) !(target.sim_extra_files);
  fprintf oc "\t@$(RM) caph.output\n";
  fprintf oc "\t@$(RM) *~\n";
  fprintf oc "\n";
  fprintf oc "clobber: clean\n";
  List.iter (function f -> fprintf oc "\t@$(RM) %s.sim\n" f.orig_name) !(target.output_files);
  fprintf oc "\t@$(RM) Makefile\n";
  fprintf oc "\n";
  fprintf oc "-include %s\n" (Misc.replace_file_suffix "proj" "rules" proj_file)

let filename_contains part fname = List.mem part (Misc.split_string '_' (Filename.chop_extension fname))

(* SystemC *)

let dump_systemc_makefile () =
  let fname = if target.dir = "." then "Makefile.systemc" else target.dir ^ "/" ^ "Makefile" in
  let oc = open_out fname in
  let mk_obj_file f = Misc.change_extension "o" f in
(*   let mk_obj_file' f = full_path (mk_obj_file f) in *)
  List.iter convert_input_file !(target.input_files);
  List.iter convert_output_file !(target.output_files);
  Misc.dump_banner "#" oc;
  Misc.check_file target.proj_file;
  let src_file, proj_file =
    if target.dir = "." then target.main_file, target.proj_file
    else "../" ^ target.main_file, "../" ^ target.proj_file in  (* TO FIX *)
  if caph_dir <> "" then fprintf oc "CAPH=%s\n\n" caph_dir;
  fprintf oc "-include %s\n" proj_file;
  fprintf oc "include $(CAPH)/lib/etc/Makefile.core\n";
  fprintf oc "\n";
  fprintf oc "%%.o: %%.cpp\n";
  fprintf oc "\t$(SC_CXX) $(CXXFLAGS) -c $<\n";
  fprintf oc "\n";
  fprintf oc ".PHONY: run code view clean clobber\n";
  fprintf oc "\n";
  fprintf oc "code: %s_net.cpp\n" target.prefix;
  fprintf oc "\n";
  fprintf oc "%s_net.cpp: %s\n" target.prefix src_file;
  fprintf oc "\t$(CAPHC) -I $(CAPH)/lib/caph -systemc $(SC_OPTS) %s\n" src_file;
  fprintf oc "\n";
  List.iter
    (function f -> fprintf oc "%s: %s_net.cpp\n" f target.prefix)
    (List.filter (function f -> not (filename_contains "net" f)) !(target.cpp_files));
  fprintf oc "\n";
  fprintf oc "exe: ./%s_sc\n" target.prefix;
  fprintf oc "\n";
  let objs = Misc.string_of_list mk_obj_file " " !(target.cpp_files) in
  let objs' = Misc.string_of_list (function f -> Filename.basename (mk_obj_file f)) " " !(target.cpp_files) in
(*   let objs'' = Misc.string_of_list mk_obj_file " " !(target.cpp_extra_files) in *)
  let objs''' = Misc.string_of_list (function f -> Filename.basename (mk_obj_file f)) " " !(target.cpp_extra_files) in
  fprintf oc "./%s_sc: %s %s\n" target.prefix objs objs''';
  fprintf oc "\t$(SC_LD) $(LDFLAGS) %s %s -o %s_sc -lsystemc  2>&1 | c++filt\n" objs' objs''' target.prefix;
  (* if target.dir <> "." then begin
   *   fprintf oc "\tif [ -e ./%s_sc ]; then $(RM) ./%s_sc; fi\n" target.prefix target.prefix;
   *   fprintf oc "\t$(LN) %s_sc ./%s_sc\n" target.prefix target.prefix
   *   end; *)
  fprintf oc "\n";
  fprintf oc "run: ./%s_sc %s\n"
    target.prefix
    (Misc.string_of_list (function f -> f.actual_name) " " !(target.input_files));
  fprintf oc "\t./%s_sc\n" target.prefix;
  fprintf oc "\n";
  List.iter (dump_inp_file_target oc) !(target.input_files);
  fprintf oc "\n";
  List.iter (dump_outp_file_target oc) !(target.output_files);
  fprintf oc "\n";
  List.iter
    (fun f -> fprintf oc "%s: %s\n"  (mk_obj_file f) f)
    (!(target.cpp_files) @ !(target.cpp_extra_files));
  fprintf oc "\n";
  fprintf oc "show: %s\n" (Misc.string_of_list (function f -> f.orig_name) " " (!(target.output_files)));
  List.iter (dump_show_cmd oc) !(target.output_files);
  fprintf oc "\n";
  fprintf oc "check: %s\n"
    (Misc.string_of_list (fun f -> f.orig_name) " " (!(target.output_files)));
  List.iter (dump_check_cmd oc "sim")  !(target.output_files);
  fprintf oc "\n";
  fprintf oc "clean:\n";
  List.iter
    (fun m -> fprintf oc "\t@$(RM) %s\n" m)
    !(target.cpp_files);
  List.iter
    (fun m -> fprintf oc "\t@$(RM) %s\n" m)
    !(target.h_files);
  fprintf oc "\t@$(RM) %s_sc\n" target.prefix;
  fprintf oc "\t@$(RM) *.o\n";
  List.iter (function f -> if f.actual_name <> f.orig_name then fprintf oc "\t@$(RM) %s\n" f.actual_name) !(target.input_files);
  List.iter (function f -> fprintf oc "\t@$(RM) %s\n\t@$(RM) %s\n" f.orig_name f.actual_name) !(target.output_files);
  fprintf oc "\t@$(RM) ./%s_sc\n" target.prefix;
  fprintf oc "\t@$(RM) caph.output *_expanded.dot io_monitor.dat\n";
  fprintf oc "\n";
  fprintf oc "clobber: clean\n";
  fprintf oc "\t@$(RM) %s_fifo_stats.dat\n" target.prefix;
  fprintf oc "\t@$(RM) Makefile\n";
  fprintf oc "\n";
  fprintf oc "-include %s\n" (Misc.replace_file_suffix "proj" "rules" proj_file)

(* VHDL *)

let convert_vhdl_input_file f = 
  match Misc.get_extension f.orig_name, Types.real_type f.typ with
    "bin", _ -> ()
  | "txt", ty -> 
      f.actual_name <- Misc.change_extension "bin" f.orig_name;
      f.converter <- Some (FF_Txt,FF_Bin)
  | "pgm", ty -> 
      f.actual_name <- Misc.change_extension "bin" f.orig_name;
      f.converter <- Some (FF_Pgm,FF_Bin)
  | ext, ty ->
      Misc.not_implemented ("Genmake.convert_vhdl_input_file: cannot handle file " ^ f.orig_name)

let convert_vhdl_output_file f = 
  match Misc.get_extension f.orig_name, Types.real_type f.typ with
    "bin", _ -> ()
  | "txt", ty -> 
      f.actual_name <- Misc.change_extension "bin" f.orig_name;
      f.converter <- Some (FF_Bin,FF_Txt)
  | "pgm", ty -> 
      f.actual_name <- Misc.change_extension "bin" f.orig_name;
      f.converter <- Some (FF_Bin,FF_Pgm)
  | ext, ty ->
      Misc.not_implemented ("Genmake.convert_vhdl_output_file: cannot handle file " ^ f.orig_name)

exception FileConverter of Types.typ

let rec txt2bin_opts ty = 
  let open Types in match real_type ty with
  | Tconstr({tc_name="bool"}, _, _) -> None, "bool 1"
  | Tconstr({tc_name="float"}, _, _) -> None, "float 32"
  | (Tconstr({tc_name="int"}, [sg], [sz]) as ty) ->
      begin match real_type sg, size_repr sz with
      | Tconstr({tc_name="_unsigned"},_,_), SzConst s -> None, "uint " ^ string_of_int s
      | Tconstr({tc_name="_signed"},_,_), SzConst s -> None, "sint " ^ string_of_int s
      | _, _ -> raise (FileConverter ty)
      end
  | Tconstr({tc_name="dc"}, [ty'], _) -> 
      begin match txt2bin_opts ty' with
      | None, opts' -> None, "-abbrev -dc " ^ opts'
      | _, _ -> raise (FileConverter ty)
      end
  | Tconstr({tc_name=name}, ts, ss) when Typing.is_variant_type name ->
      Some (Mangling.string_of_name name ts ss []), ""
  | ty -> raise (FileConverter ty)

let rec pgm2bin_opts ty = 
  let open Types in 
  match real_type ty with
  | Tconstr({tc_name="dc"}, [ty'], _) -> 
        begin match bitwidth_of_type ty' with
        | Some s -> string_of_int s
        | None -> raise (FileConverter ty)
        end 
  | ty -> raise (FileConverter ty)

let dump_vhdl_inp_file_target oc acc f =
  match f.converter with
    Some (FF_Txt,FF_Bin) -> 
      begin match txt2bin_opts f.typ with 
      | None, opts ->
          fprintf oc "%s: ../%s\n" f.actual_name f.orig_name; (* TO FIX *)
          fprintf oc "\t$(TXT2BIN) %s $< > $@\n" opts;
      | Some c, opts ->
          let encoder = "encode_" ^ c in
          fprintf oc "%s: ../%s %s\n" f.actual_name f.orig_name encoder; (* TO FIX *)
          fprintf oc "\t./encode_%s %s $< > $@\n" c opts;
          add_to_target target.input_type_converters (encoder, f.typ)
      end;
      f.actual_name :: acc
  | Some (FF_Pgm,FF_Bin) -> 
      fprintf oc "%s: ../%s\n" f.actual_name f.orig_name; (* TO FIX *)
      let opts = pgm2bin_opts f.typ in 
      fprintf oc "\t$(PGM2BIN) %s $< $@\n" opts;
      f.actual_name :: acc
  | None ->
      acc
  | _ ->
      Error.invalid_input_data_file f.orig_name

let dump_vhdl_outp_file_target oc f =
  match f.converter with
    Some (FF_Bin,FF_Txt) -> 
      begin match txt2bin_opts f.typ with 
      | None, opts ->
          fprintf oc "%s: %s\n" f.orig_name f.actual_name;
          fprintf oc "\t$(BIN2TXT) %s $< > $@\n" opts
      | Some c, opts ->
          let decoder = "decode_" ^ c in
          fprintf oc "%s: %s %s\n" f.orig_name f.actual_name decoder;
          fprintf oc "\t./decode_%s %s $< > $@\n" c opts;
          add_to_target target.output_type_converters (decoder, f.typ);
      end
  | Some (FF_Bin,FF_Pgm) -> 
      fprintf oc "%s: %s\n" f.orig_name f.actual_name;
      let opts = pgm2bin_opts f.typ in 
      fprintf oc "\t$(BIN2PGM) %s $< $@\n" opts
  | None ->
      ()
  | _ ->
      Error.invalid_input_data_file f.orig_name

let dump_type_converter oc (pgm,ty) =
  fprintf oc "%s: %s.c\n" pgm pgm;
  fprintf oc "\t$(CC) -g -I $(CAPH)/lib/c -L$(CAPH)/lib/c -o %s %s.c -ltyconv\n" pgm pgm

let dump_vhdl_makefile () =
  let fname = if target.dir = "." then "Makefile.vhdl" else target.dir ^ "/" ^ "Makefile" in
  let oc = open_out fname in
  let srcs1,srcs2 = List.partition
        (function f -> filename_contains "globals" f || filename_contains "types" f || filename_contains "splitters" f)
        !(target.vhdl_files) in
  target.vhdl_files := srcs1 @ srcs2; (* Rudimentary sorting.. *)
  let mk_obj_file f = Misc.change_extension "o" f in
  List.iter convert_vhdl_input_file !(target.input_files);
  List.iter convert_vhdl_output_file !(target.output_files);
  Misc.check_file target.proj_file;
  let src_file, proj_file =
    if target.dir = "." then target.main_file, target.proj_file
    else "../" ^ target.main_file, "../" ^ target.proj_file in  (* TO FIX *)
  Misc.dump_banner "#" oc;
  if caph_dir <> "" then fprintf oc "CAPH=%s\n\n" caph_dir;
  fprintf oc "-include %s\n" proj_file;
  fprintf oc "include $(CAPH)/lib/etc/Makefile.core\n";
  fprintf oc "\n";
  fprintf oc "%%.o: %%.vhd\n";
  fprintf oc "\t$(GHDL) -a $(GHDL_ELAB_OPTS) $<\n";
  fprintf oc "\n";
  fprintf oc ".PHONY: bin run code viewtrace viewvcdtrace clean clobber\n";
  fprintf oc "\n";
  fprintf oc "code: %s %s_tb.vhd\n" (if cfg.tb_inline_io then "bin" else "") target.prefix;
  fprintf oc "\n";
  fprintf oc "%s_tb.vhd: %s\n" target.prefix src_file;
  fprintf oc "\t$(CAPHC) -I $(CAPH)/lib/caph -vhdl $(VHDL_OPTS) %s\n" src_file;
  fprintf oc "\n";
  List.iter
    (function f -> fprintf oc "%s: %s_tb.vhd\n" f target.prefix)
    (List.filter (function f -> not (filename_contains "tb" f)) !(target.vhdl_files));
  fprintf oc "\n";
  fprintf oc "exe: ./%s_tb\n" target.prefix;
  fprintf oc "\n";
  fprintf oc "run: ./%s_tb %s\n" 
    target.prefix
    (Misc.string_of_list (function f -> f.actual_name) " " !(target.input_files));
  fprintf oc "\t$(GHDL) -r $(GHDL_ELAB_OPTS) %s_tb $(GHDL_RUN_OPTS)\n" target.prefix;
  fprintf oc "\n";
  fprintf oc "./%s_tb: %s\n" target.prefix (Misc.string_of_list mk_obj_file  " " (!(target.vhdl_extra_files) @ !(target.vhdl_files)));
  fprintf oc "\t$(GHDL) -e $(GHDL_ELAB_OPTS) %s_tb\n" target.prefix;
  (* if target.dir <> "." then begin
   *   fprintf oc "\tif [ -e ./%s_tb ]; then $(RM) ./%s_tb; fi\n" target.prefix target.prefix;
   *   fprintf oc "\t$(LN) %s/%s_tb ./%s_tb\n" target.dir target.prefix target.prefix
   *   end; *)
  fprintf oc "\n";
  List.iter
    (fun f -> fprintf oc "%s: %s\n"  (mk_obj_file f) f)
    (!(target.vhdl_files) @ !(target.vhdl_extra_files)) ;
  fprintf oc "\n";
  let bin_targets = List.fold_left (dump_vhdl_inp_file_target oc) [] !(target.input_files) in
  fprintf oc "\n";
  fprintf oc "bin: %s\n" (Misc.string_of_list Misc.id " " bin_targets);
  fprintf oc "\n";
  List.iter (dump_vhdl_outp_file_target oc) !(target.output_files);
  fprintf oc "\n";
  List.iter (dump_type_converter oc) !(target.input_type_converters);
  List.iter (dump_type_converter oc) !(target.output_type_converters);
  fprintf oc "\n";
  fprintf oc "viewvcdtrace: %s_tb.vcd\n" target.prefix;
  fprintf oc "\t$(GTKWAVE) -f %s_tb.vcd -a %s_tb.sav\n" target.prefix target.prefix;
  fprintf oc "viewtrace: %s_tb.ghw\n" target.prefix;
  fprintf oc "\t$(GTKWAVE) -f %s_tb.ghw -a %s_tb.sav\n" target.prefix target.prefix;
  fprintf oc "\n";
  fprintf oc "show: %s\n" (Misc.string_of_list (function f -> f.orig_name) " " (!(target.output_files)));
  List.iter (dump_show_cmd oc) !(target.output_files);
  fprintf oc "\n";
  fprintf oc "check: %s\n"
    (Misc.string_of_list (fun f -> f.orig_name) " " (!(target.output_files)));
  List.iter (dump_check_cmd oc "sim")  !(target.output_files);
  fprintf oc "\n";
  fprintf oc "clean:\n";
  List.iter
    (fun m -> fprintf oc "\t@$(RM) %s\n" m)
    !(target.vhdl_files);
  fprintf oc "\t@$(RM) %s_tb\n" target.prefix;
  fprintf oc "\t@$(RM) *.o\n";
  fprintf oc "\t@$(RM) *.cf\n";
  fprintf oc "\t@$(RM) *.vcd *.ghw *.qip\n";
  fprintf oc "\t@$(RM) *.bin\n";
  List.iter (function f -> if f.actual_name <> f.orig_name then fprintf oc "\t@$(RM) %s\n" f.actual_name) !(target.input_files);
  List.iter (function f -> fprintf oc "\t@$(RM) %s\n\t@$(RM) %s\n" f.orig_name f.actual_name) !(target.output_files);
  List.iter (fun (f,ty) -> fprintf oc "\t@$(RM) -r %s*\n" f) !(target.input_type_converters);
  List.iter (fun (f,ty) -> fprintf oc "\t@$(RM) -r %s*\n" f) !(target.output_type_converters);
  fprintf oc "\t@$(RM) ./%s_tb\n" target.prefix;
  fprintf oc "\t@$(RM) caph.output *_expanded.dot\n";
  fprintf oc "\n";
  fprintf oc "clobber: clean\n";
  List.iter (function f -> fprintf oc "\t@$(RM) %s.vhdl\n" f.orig_name) !(target.output_files);
  fprintf oc "\t@@$(RM) Makefile\n";
  fprintf oc "\n";
  fprintf oc "-include %s\n" (Misc.replace_file_suffix "proj" "rules" proj_file)
