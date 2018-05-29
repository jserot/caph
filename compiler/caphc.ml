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

open Parser
open Misc
open Printf
open Location
open Syntax
open Typing
open Builtins
open Static
open Dynamic
open Options

exception Toplevel

let usage = "usage: caphc [options...] file"

let prelude_name = "prelude.cph"

let anonymous fname = source_file := fname

let reopen_input_file () = 
  close_in !Location.input_chan;
  let ic = open_in_bin !Location.input_name in
  Location.input_chan := ic
  
let print_banner () = 
  Printf.printf "-------------------------------------------------------------------------------------------------\n";
  Printf.printf "This is the Caph compiler, version %s\n" Version.version;
  Printf.printf "(C) 2011-2018 J. Serot (Jocelyn.Serot@uca.fr)\n";
  Printf.printf "For more information, see : http://caph.univ-bpclermont.fr\n"; 
  Printf.printf "-------------------------------------------------------------------------------------------------\n";
  flush stdout

let parse fname = 
  let ic = open_in_bin fname in
  (* The source file must be opened in binary mode, so that the absolute seeks in print_location work. *)
  Location.input_name := fname;
  Location.input_chan := ic;
  let lexbuf = Lexing.from_channel !Location.input_chan in
  Location.input_lexbuf := lexbuf;
  Parser.program Lexer.main Location.input_lexbuf

let main () =
try
  Sys.catch_break true;
  Arg.parse Options_spec.options_spec anonymous usage;
  print_banner ();
  let p1 = parse !source_file in
  let p = Syntax.pre_process p1 in
  let prefix = match !output_prefix with
    "" -> Filename.basename (Sys.getcwd ())
  | s -> s in
  Genmake.target.Genmake.proj_name <- prefix;
  Genmake.target.Genmake.main_file <- !source_file;
  Logfile.start ();
  if !dump_tenv then print_typing_environment builtin_typing_env;
  let tp = type_program builtin_typing_env p in
  if !dump_typed then print_typed_program tp;
  let sp = build_static tp builtin_static_env p in
  if !dump_sdf_fifo_sizes then compute_moc := true;
  if !compute_moc then Moc.compute_mocs prefix sp;
  if !dump_senv then print_static_environment tp sp;
  if !dump_boxes then print_static_boxes sp;
  begin match !abstract_interpret with
    Some bid -> Absint.run sp bid
  | None -> () 
  end;
  if !dump_fsms then Fsm.dump_fsms sp;
  if !run then begin
    if !Misc.generate_makefiles then
      Genmake.dump_sim_makefile "Makefile.sim"
    else begin
      let denv = build_dynamic tp sp builtin_static_env in
      if !dump_denv then print_dyn_env denv;
      if extern_fns_required denv then Foreign.init ();
      let final_env = Process.eval tp denv in
      if !Dynamic.dump_channel_stats  then List.iter Trace.dump_channel_stats final_env.d_channels;
      finalize_env final_env;
      close_in !Location.input_chan
      end
  end;
  reopen_input_file ();  (* Note 2012-01-11, JS : seek in Location seems broken w/o this.. *)
  if !output_fmt <> NoOutput then begin
    let ir = Interm.mk_interm tp sp in
    if !Interm.dump_ir then Interm.dump_interm_repr ir;
    if !dump_sdf_fifo_sizes then begin
      (* let ir' = Interm.insert_splitters ir in *)
      Sdf.dump_fifo_sizes prefix ir
      end;
    begin match !output_fmt with
    | Dot ->
        check_dir Genmake.target.Genmake.dir;
        Dot.output_ir prefix ir
    | Xdf ->
        check_dir Genmake.target.Genmake.dir;
        Xdf.output_ir prefix ir;
        List.iter Cal.dump_actor ir.Interm.ir_actors
    | Dif ->
        check_dir Genmake.target.Genmake.dir;
        Dif.output_ir prefix ir
    | Systemc ->
        let ir' = Interm.insert_splitters ir in
        Dot.output_ir (prefix ^ "_expanded") ir';
        check_dir Genmake.target.Genmake.dir;
        let profil = Systemc.dump_top_module prefix (prefix ^ "_net.cpp") ir' in
        begin match profil.Systemc.has_globals with
          Some _ -> Systemc.dump_globals (prefix ^ "_globals") ir'
        | None -> () end;
        List.iter (Systemc.dump_actor profil prefix ir') ir.Interm.ir_actors;
        if profil.Systemc.splitters <> [] then
          Systemc.dump_split_actors (prefix ^ Systemc.cfg.Systemc.sc_splitters_suffix ^ ".h") profil.Systemc.splitters; 
        if !Misc.generate_makefiles then Genmake.dump_systemc_makefile "Makefile.systemc"
    | Vhdl ->
        let ir' = Interm.insert_fifos_and_buffers (Interm.insert_splitters ir) in
        Dot.output_ir (prefix ^ "_expanded") ir';
        if !Interm.dump_ir then Interm.dump_interm_repr ir';
        check_dir Genmake.target.Genmake.dir;
        let profil, actors = Vhdl.dump_network prefix tp ir' in
        if profil.Vhdl.has_globals then Vhdl.dump_globals prefix ir' profil;
        if profil.Vhdl.variant_types <> [] then begin
          Vhdl.dump_types (prefix ^ "_types") ir' profil;
          if Vhdl.cfg.Vhdl.vhdl_write_type_converters then Vhdl.dump_type_io_converters profil
        end;
        List.iter (Vhdl.dump_actor prefix profil ir') ir'.Interm.ir_actors;
        Vhdl.dump_testbench prefix ir';
(*         if profil.Vhdl.has_externs then Genmake.add_target "extfns.vhd";  (\* to be adjusted/generalized.. *\) *)
        if profil.Vhdl.splitters <> [] then
          Vhdl.dump_split_actors (prefix ^ Vhdl.cfg.Vhdl.vhdl_splitters_suffix ^ ".vhd") profil.Vhdl.splitters; 
        if !Misc.generate_makefiles then Genmake.dump_vhdl_makefile "Makefile.vhdl";
    | _ ->
        ()
    end
  end;
  Logfile.stop ()
with
  Parsing_m.Parse_error ->
    let pos1 = Lexing.lexeme_start !Location.input_lexbuf in
    let pos2 = Lexing.lexeme_end !Location.input_lexbuf in
    eprintf "%aSyntax error\n" output_location (Loc(!input_name,pos1, pos2));
    flush stderr; exit 1
| Lexer.Lexical_error(errcode, pos1, pos2) ->
    let l = Loc(!input_name,pos1, pos2) in
    begin match errcode with
      Lexer.Illegal_character ->
        eprintf "%aIllegal character.\n" output_location l
    | Lexer.Unterminated_string ->
        eprintf "%aUnterminated string.\n" output_location l
    | Lexer.Unterminated_cond ->
        eprintf "%aUnterminated #if/#else.\n" output_location l
    | Lexer.Bad_char_constant ->
        eprintf "%aBad character constant.\n" output_location l
    | Lexer.Undefined_macro ->
        eprintf "%aUndefined macro.\n" output_location l
    | Lexer.Unspecified_macro ->
        eprintf "%aNo value attached to macro.\n" output_location l
    | Lexer.Nested_ifdef ->
        eprintf "%aNested #ifdef.\n" output_location l
    | Lexer.Dangling_cond s ->
        eprintf "%aOut of context %s directive.\n" output_location l s
    end;
    flush stderr; exit 2
| End_of_file -> exit 0
| Error -> flush stderr; exit 3
| Internal s ->
    eprintf "Internal error: %s.\n" s;
    flush stderr; exit 4
| Sys.Break -> flush stderr; exit 5
| Sys_error msg ->
    eprintf "Input/output error: %s.\n" msg;
    flush stderr;
    exit 6
| e ->
    eprintf "Internal error: %s.\n" (Printexc.to_string e);
    flush stderr; exit 7
    
let _ = Printexc.print main ()
