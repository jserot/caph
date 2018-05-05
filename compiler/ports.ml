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

open Dsval
open Misc
open Error
open Expr
open Types
open Io

let trace_port_value_change = ref false

type 'a io_event = {
    e_val: 'a;
    e_date: int;  (* in sim cycles *)
  }

type 'a input_context = {
    mutable cur_val: 'a;
    mutable next_events: 'a io_event list;
    }

(* Reading IO Port event files *)

let p_event p_val = parser [< 'Genlex.Int t1; 'Genlex.Int t2; 'Genlex.Int t3; v=p_val >] -> (t1,t2,t3,v)

let read_event_file tp ty fname = 
  let ic = open_in fname in
  let lexer = Genlex.make_lexer ["true"; "false"; "("; ")"; ","] in
  let p_val = mk_parser tp ty in
  let r = ref [] in
  begin try
    while true do
      let s = input_line ic in
      if s.[0] <> '#' then begin
        let ss = lexer (Stream.of_string s) in
        begin match p_event p_val ss with
          (t, _, _, Some v) -> r := { e_date=t; e_val=v } :: !r
        | _ -> file_syntax_error fname end
      end
    done
  with
    End_of_file -> ()
  | Stream.Failure 
  | Stream.Error _ -> file_syntax_error fname end;
  List.rev !r

let rec mk_input_port tp ty src = mk_input_port_from_raw_file tp ty src

and mk_input_port_from_raw_file pname tp ty fname ival =
  let ctx = {
    cur_val =
      begin match ival with
        Some v -> v
      | None -> fatal_error "Ports.mk_input_port_from_raw_file: no init value" (* should not happen *) end;
    next_events =
     if fname = ""
     then []
     else begin
       let r = read_event_file tp ty fname in
       if !file_trace then Printf.printf "Ports.mk_input_port_from_file: port %s: read %d event(s) in file %s\n"
           pname (List.length r) fname;
       r
     end } in
  (function t -> true),                        (* Rdy *)
  (function t ->                               (* Get *)
    match ctx.next_events with    
     e::rest ->
       if e.e_date = t then
         begin
           if !trace_port_value_change then begin
             Printf.printf "** %s: changing current port value from %s to %s at t=%d\n"
               pname (string_of_val ctx.cur_val) (string_of_val e.e_val) t;
             flush stdout 
             end;
           ctx.cur_val <- e.e_val;
           ctx.next_events <- rest
         end;
       Some ctx.cur_val
  | [] -> Some ctx.cur_val),
  (function t -> ())                           (* Close *)

let rec mk_output_port ty dst = mk_output_port_from_file ty dst

and mk_output_port_from_file ty fname =
  Logfile.write fname;
  mk_output_port_from_raw_file ty fname

and mk_output_port_from_raw_file ty fname =
  match fname with 
  | "stdout" ->
      (function t -> port_put stdout t),    (* Put *)
      (function t -> ())                    (* Close *)
  | f ->
      let oc = open_out f in
      (function t -> port_put oc t),        (* Put *)
      (function t -> close_out oc)          (* Close *)

and port_put oc t =
  function v ->
    let s = string_of_value v in
    output_string oc s;
    output_string oc "@";
    output_string oc (string_of_int t);
    output_string oc " ";
    if !trace_port_value_change then begin
      Printf.printf "** writting port value %s at t=%d\n" (string_of_val v) t;
      flush stdout
    end;
    flush oc
