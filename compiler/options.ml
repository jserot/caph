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

let print_version () = Printf.printf "This is the Caph compiler, version %s\n" Version.version; exit 0

type output_format = NoOutput | Dot | Xdf | Dif | Systemc | Vhdl
let output_fmt = ref NoOutput
let output_prefix = ref ""
let prefix = ref ""
let dump_tenv = ref false
let dump_typed = ref false
let dump_senv = ref false
let dump_boxes = ref false
let dump_denv = ref false
let dump_fsms = ref false
let dump_static_fifo_sizes = ref false
let abstract_interpret = ref (None : int option)
let run = ref false 
let dump_sdf_fifo_sizes = ref false

let ignore_pragmas () = Syntax.allow_pragmas := false
let add_allowed_input_file f =
  Genmake.safe_mode := true;
  Genmake.safe_cfg.Genmake.allowed_ifiles <- Genmake.safe_cfg.Genmake.allowed_ifiles @ [f]
let add_allowed_output_file f =
  Genmake.safe_mode := true;
  Genmake.safe_cfg.Genmake.allowed_ofiles <- Genmake.safe_cfg.Genmake.allowed_ofiles @ [f]
let set_output_prefix name = output_prefix := name
let add_include_path path = Lexer.include_path := !Lexer.include_path @ [path]
let set_prefix p = prefix := p
let set_target_dir p = Genmake.target.Genmake.dir <- p
let do_show_signness () = Const.show_signness := true
let do_flat_variants () = Expr.flat_variants := true
let do_dump_tenv () = dump_tenv := true
let do_dump_typed () = dump_typed := true
let do_dump_senv () = dump_senv := true
let do_dump_boxes () = dump_boxes := true
let do_compute_moc () = Static.compute_moc := true
let do_dump_ir () = Interm.dump_ir := true
let do_dump_denv () = dump_denv := true
let do_dump_fsms () = dump_fsms := true
let do_run () = run := true
let do_dot () = output_fmt := Dot
let do_xdf () = output_fmt := Xdf
let do_dif () = output_fmt := Dif
let do_systemc () = output_fmt := Systemc
let do_vhdl () = output_fmt := Vhdl
let do_dot_unlabeled_edges () = Dot.cfg.Dot.labeled_edges <- false
let do_dot_unboxed_ios () =
  Dot.cfg.Dot.stream_io_box_shape <- "plaintext";
  Dot.cfg.Dot.port_io_box_shape <- "plaintext"
let do_dot_show_indexes () = Dot.cfg.Dot.show_indexes <- true
let do_dot_wire_annots () = Dot.cfg.Dot.show_wire_annots <- true
let do_dot_simple_boxes () = Dot.cfg.Dot.slotted_boxes <- false
let do_trace () = Trace.tracing := true
let do_trace_ports () = Ports.trace_port_value_change := true
let set_abbrev_dc_ctors () = Expr.abbrev_dc_ctors := true
let set_default_channel_cap c = Dynamic.default_channel_capacity := c
let do_dump_channel_stats () = Dynamic.dump_channel_stats := true
let do_dump_sdf_fifo_sizes () = dump_sdf_fifo_sizes := true
let set_max_run_cycles n = Process.max_run_cycles := n
let set_min_run_cycles n = Process.min_run_cycles := n
let do_warn_channels () = Process.warn_on_full_channels := true
let suppress_cast_warnings () = Error.report_cast_warnings := false
let do_phantom_types () = Pr_type.print_type_repr := true
let set_stdin f = Io.stdin_file := f
let set_stdout f = Io.stdout_file := f
let add_cmo_file f = match Misc.get_extension f with
  "ml" -> 
    Genmake.add_to_target Genmake.target.Genmake.sim_extra_files f;
    Foreign.cmo_files := (Misc.change_extension "cmo" f) :: !Foreign.cmo_files;
| _ -> Error.illegal_extra_file "-sim_extra" ".ml" f
let add_macro_defn d = Macro.add_defn d
let do_gen_make () = Misc.generate_makefiles := true
let set_split_output_frames () = Streams.split_output_frames := true
let do_abstract_interpret id = abstract_interpret := Some id
let set_ai_max_cycles n = Absint.cfg.Absint.ai_max_cycles <- n
(* SystemC related options *)
let set_sc_stop_time n = Systemc.cfg.Systemc.sc_stop_time <- n
let set_sc_stop_idle_time n = Systemc.cfg.Systemc.sc_stop_idle_time <- n
let set_sc_io_monitor () = Systemc.cfg.Systemc.sc_io_monitor <- true
let set_sc_io_monitor_file f = Systemc.cfg.Systemc.sc_io_monitor_file <- f
let set_sc_clock_period n = Systemc.cfg.Systemc.sc_clock_period_ns <- n
let set_sc_fifo_capacity n = Systemc.cfg.Systemc.sc_fifo_capacity <- n
let set_sc_trace () = Systemc.cfg.Systemc.sc_trace <- true
let set_sc_trace_fifos () = Systemc.cfg.Systemc.sc_trace_fifos <- true
let set_sc_dump_fifos () = Systemc.cfg.Systemc.sc_dump_fifos <- true
let set_sc_dump_fifo_stats () = Systemc.cfg.Systemc.sc_dump_fifo_stats <- true
let set_sc_fifo_stats_file f = Systemc.cfg.Systemc.sc_fifo_stats_file <- f
let set_sc_use_int () = Systemc.cfg.Systemc.sc_use_int <- true
let set_sc_use_templates () = Printf.printf "** Warning: option -sc_use_templates is deprecated (ignored here)\n"
let add_sc_extra_file f = match Misc.get_extension f with
  "h" -> Genmake.add_to_target Genmake.target.Genmake.h_extra_files f
| "cpp" -> Genmake.add_to_target Genmake.target.Genmake.cpp_extra_files f
| _ -> Error.illegal_extra_file "-sc_extra" ".h, .cpp" f
let set_sc_abbrev_dc_ctors () = Systemc.cfg.Systemc.sc_abbrev_dc_ctors <- true
let set_sc_istream_period p = Systemc.cfg.Systemc.sc_stream_in_period <- p
let set_sc_istream_hblank p = Systemc.cfg.Systemc.sc_stream_in_hblank <- p
let set_sc_istream_vblank p = Systemc.cfg.Systemc.sc_stream_in_vblank <- p
(* VHDL related options *)
let set_vhdl_num_lib s = Vhdl.cfg.Vhdl.vhdl_num_lib <- s
let set_vhdl_default_int_size s = Vhdl.cfg.Vhdl.vhdl_default_int_size <- s
let set_vhdl_annot_file f = Vhdl.cfg.Vhdl.vhdl_annot_file <- f
let set_vhdl_fifo_offset n = Vhdl.cfg.Vhdl.vhdl_fifo_offset <- n
let set_vhdl_default_fifo_capacity s = Vhdl.cfg.Vhdl.vhdl_default_fifo_capacity <- s
let set_vhdl_big_fifo_model s = Vhdl.cfg.Vhdl.vhdl_big_fifo_model <- s
let set_vhdl_small_fifo_model s = Vhdl.cfg.Vhdl.vhdl_small_fifo_model <- s
let set_vhdl_fifo_model_threshold n = Vhdl.cfg.Vhdl.vhdl_fifo_model_threshold <- n
let set_vhdl_reset_duration s = Vhdl.cfg.Vhdl.vhdl_reset_duration_ns <- s
let set_vhdl_clock_period s = Vhdl.cfg.Vhdl.vhdl_clock_period_ns <- s
let set_vhdl_seq_delay s = Vhdl.cfg.Vhdl.vhdl_seq_delay_ns <- s
let set_vhdl_istream_period p =
  if p < 1 then Printf.eprintf "Warning : pixel period for I/O streams should be > 0 !\n";
  Vhdl.cfg.Vhdl.vhdl_stream_in_period <- p
let set_vhdl_istream_blanking () = Vhdl.cfg.Vhdl.vhdl_stream_in_blanking <- true
let set_vhdl_istream_skew t = Vhdl.cfg.Vhdl.vhdl_stream_in_skew <- t
let set_vhdl_trace () = Vhdl.cfg.Vhdl.vhdl_trace <- true
let set_vhdl_init_array_at_decl () = Vhdl.cfg.Vhdl.vhdl_init_array_at_decl <- true
let set_vhdl_warn_on_unsized_consts () = Vhdl.cfg.Vhdl.vhdl_warn_on_unsized_consts <- true
let set_vhdl_use_native_mult () = Vhdl.cfg.Vhdl.vhdl_use_native_mult <- true
let set_vhdl_float_support () = Vhdl.cfg.Vhdl.vhdl_float_support <- true
let set_vhdl_write_type_converters () = Vhdl.cfg.Vhdl.vhdl_write_type_converters <- true
let set_vhdl_rename_io_wires () = Vhdl.cfg.Vhdl.vhdl_rename_io_wires <- true
let add_vhdl_extra_file f = match Misc.get_extension f with
| "vhd" -> Genmake.add_to_target Genmake.target.Genmake.vhdl_extra_files f
| _ -> Error.illegal_extra_file "-vhdl_extra" ".vhd" f
let set_vhdl_quartus () = Vhdl.cfg.Vhdl.vhdl_generate_qip <- true
(* XDF related options *)
let set_xdf_package p = Xdf.cfg.Xdf.target_package <- p
