(************************************************************************************)
(*                                                                                 *)
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

open Dsval
open Misc
open Error
open Expr
open Types
open Io

let split_output_frames = ref false

type stream_device =
    RegularFile of string
  | SocketConnection of string * int * bool (* host name, port number, blocking *)

type 'a input_context = {
    mutable ic_current_file: string;
    mutable ic_current_chan: in_channel;
    mutable ic_current_stream: 'a Stream.t;
    mutable ic_next_files: string list
  }

type output_context = {
    mutable oc_current_file: string;
    mutable oc_current_chan: out_channel;
    mutable oc_next_files: string list;
    mutable oc_prev_val: Expr.e_val
  }

let device_of src =
  if String.contains src ':' then  (* host:port means socket spec *)
    begin try
      let l = String.length src
      and i = String.index src ':' in
      let blocking, l' = if src.[l-1] = '&' then true, l-1 else false, l in
      let host = String.sub src 0 i
      and port = String.sub src (i+1) (l'-i-1) in
      SocketConnection (host, int_of_string port, blocking)
    with
    | Failure "int_of_string" ->
        err_invalid_port_number src end
  else
    let file = begin match src with 
        "stdin" -> !stdin_file
      | "stdout" -> !stdout_file
      | _ -> src end in
    RegularFile file

let rec mk_input_stream tp ty src =
  match device_of src with
    RegularFile fname ->
      begin match Filepat.expand fname with
        [fname] -> mk_input_stream_from_raw_file tp ty fname
      | fnames -> mk_input_stream_from_raw_files tp ty fnames
      end
  | SocketConnection (host,port,blocking) -> not_implemented "socket implementation of input streams"

and mk_input_stream_from_raw_file tenv ty fname =
  let ic = if fname = "stdin" then (warn_stdin (); stdin) else open_in fname in
  let parse = mk_parser tenv ty in 
  let lexer = Genlex.make_lexer ["true"; "false"; "("; ")"; ","] in
  let s =
    try input_line ic  (* TO FIX : allow input data to span along several lines of the input stream *)
    with
       Sys_blocked_io
     | End_of_file
     | Unix.Unix_error(Unix.EAGAIN,_,_)        (* raised is no data available *)
     | Unix.Unix_error (Unix.EWOULDBLOCK,_,_) -> cannot_read_from_file fname
  in
  let ss = lexer (Stream.of_string s) in
  (function t -> match Stream.peek ss with Some _ -> true | None -> false), (* Rdy *)
  (function t -> try parse ss with Stream.Failure -> None),                 (* Get *)
  (function t -> close_in ic)                                               (* Close *)


and mk_input_stream_from_raw_files tenv ty fnames =
  let c = mk_input_context fnames in
  let parse = mk_parser tenv ty in 
  (function t ->                                                      (* Rdy *)
    match Stream.peek c.ic_current_stream, c.ic_next_files with
      Some _, _ -> true
    | None, [] -> false
    | None, f::fs -> 
        if !file_trace then Printf.printf "Streams.mk_input_stream_from_files: closing file %s\n" c.ic_current_file;
        close_in c.ic_current_chan;
        c.ic_current_file <- f;
        if !file_trace then Printf.printf "Streams.mk_input_stream_from_files: opening file %s\n" c.ic_current_file;
        c.ic_current_chan <- open_in f;
        c.ic_current_stream <- mk_lexer f;
        c.ic_next_files <- fs;
        stream_rdy c.ic_current_stream),
  (function t -> try parse c.ic_current_stream with Stream.Failure | Stream.Error _ -> None),       (* Get *)
  (function t -> close_in c.ic_current_chan)

and mk_input_context fnames = 
  match fnames with
    f::fs ->
      { ic_current_file = f;
        ic_current_chan =
          (if !file_trace then Printf.printf "Streams.mk_input_context: opening file %s\n" f;
          open_in f);
        ic_current_stream = mk_lexer f;
        ic_next_files = fs }
  | [] -> fatal_error "Streams.mk_input_context: empty file list"

and mk_lexer f = 
  let ic = if f = "stdin" then (warn_stdin (); stdin) else open_in f in
  let lexer = Genlex.make_lexer ["true"; "false";"(";")";"," ] in
  let s =
    try input_line ic  (* TO FIX : allow input data to span along several lines of the input stream *)
    with
       Sys_blocked_io
     | End_of_file
     | Unix.Unix_error(Unix.EAGAIN,_,_)        (* raised is no data available *)
     | Unix.Unix_error (Unix.EWOULDBLOCK,_,_) -> cannot_read_from_file f in
  lexer (Stream.of_string s)

and stream_rdy ss = match Stream.peek ss with Some _ -> true | None -> false

let rec mk_output_stream ty dst =
  match device_of dst with
    RegularFile fname ->
      begin match Filepat.expand fname with
        [fname] -> mk_output_stream_from_file ty fname
      | fnames ->
          if !split_output_frames then mk_output_streams_from_files ty fnames
          else not_implemented "multi-files output without the -split_output_frames option"
      end
  | SocketConnection (host,port,blocking) -> not_implemented "socket implementation for output streams"

and mk_output_stream_from_file ty fname =
  let fname' = Misc.prefix_dir Genmake.target.Genmake.dir fname in
  Logfile.write fname';
  mk_output_stream_from_raw_file ty fname'

and mk_output_stream_from_raw_file ty fname =
  match fname with 
  | "stdout" ->
      (fun t v -> stream_put t stdout v),
      (function t -> ())
  | f ->
      let oc = open_out f in
      (fun t v -> stream_put t oc v),
      (function t -> close_out oc)

and mk_output_streams_from_files ty fnames =
  let fnames' = List.map (Misc.prefix_dir Genmake.target.Genmake.dir) fnames in
  let c = mk_output_context ty fnames' in
  (fun t v ->                                                      (* Put *)
    stream_put t c.oc_current_chan v;
    begin match v, c.oc_prev_val with
      Val_con("EoS",[]), Val_con("EoS",[]) ->      (* End of frame *)
        if !file_trace then Printf.printf "Streams.mk_output_streams_from_files: closing file %s\n" c.oc_current_file;
        close_out c.oc_current_chan;
        begin match c.oc_next_files with
          f::fs ->
            begin 
              c.oc_current_file <- f;
              if !file_trace then Printf.printf "Streams.mk_output_streams_from_files: opening file %s\n" c.oc_current_file;
              c.oc_current_chan <- open_out f;
              Logfile.write f;
              c.oc_next_files <- fs
            end 
        | [] ->
              if !file_trace then Printf.printf "Streams.mk_output_streams_from_files: at end of file list\n"
        end
    | _, _ -> ()
    end;
    c.oc_prev_val <- v),
  (function t -> close_out c.oc_current_chan)                    (* Close *)

and decode_ty ty = ()

and mk_output_context ty fnames =
  match fnames with
    f::fs ->
      { oc_current_file = f;
        oc_current_chan = 
          (if !file_trace then Printf.printf "Streams.mk_output_context: opening file %s\n" f;
           open_out f);
        oc_next_files = fs;
        oc_prev_val = Val_unknown }
  | [] -> fatal_error "Streams.mk_output_context: empty file list"

and stream_put t oc v =
  let s = string_of_value v in
  output_string oc s;
  flush oc

(* and mk_output_stream_from_socket ty host port = *)
(*   let fd = find_or_create_socket (host,port) Sock_out in *)
(*   let oc = Unix.out_channel_of_descr fd in *)
(*   stream_put oc ty, stream_close_fd fd *)
