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

open Syntax
open Expr
open Dsval
open Error
open Misc
open Static
open Ssval
open Dsval
open Types
open Arrays

let default_channel_capacity = ref 256
let dump_channel_stats = ref false

type id = string

(* The dynamic environment *)

type dyn_env =
  { d_vals: (id * Expr.e_val) list;     (* global values *)
    d_procs: (pid * process) list;      (* processes (dynamic boxes) *)
    d_channels: (cid * channel) list }  (* channels  (dynamic wires) *)

and process = {
    p_name: string;
    p_kind: proc_kind;
    p_ins: (string * cid) list;
    p_outs: (string * cid list) list;
    p_params: (string * ds_val) list;
    p_vars: (string * ds_val ref) list;
    p_rules: (bp_rule * int) list;       (* rule desc, rule no *)
    mutable p_rule: (bp_rule * int) option;      (* Fireable rule in the current cycle *)
    p_fns: proc_iofns option }           (* I/O routines - for InpP and OutP processes only *)

and proc_kind = | RegularP | InpP of io_kind| OutP of io_kind

and bp_rule = Syntax.qualified_rule_pattern list * Syntax.expr list * Syntax.qualified_expr list

and proc_iofns = {
    pf_rdy: time -> bool;
    pf_get: time -> Expr.e_val option;
    pf_put: time -> Expr.e_val -> unit;
    pf_close: time -> unit }

and time = int (* exec cycle # *)

let string_of_binding (id,v) = id ^ "=" ^ Expr.string_of_val v

let current_cycle = ref 0

(* Accessors *)

let io_fns p = match p.p_fns with Some fns -> fns | None -> fatal_error "Dynamic.io_fns"

(* Rule |- Box => Process *)

let rec mk_process tp sp (bid,box) =
  match box.b_tag with 
    RegularB -> 
      bid,
      { p_name = box.b_name;
        p_kind = RegularP;
        p_ins = List.map (function (id,(wid,ty)) -> id, wid) box.b_ins;     (* Channel id = Wire id *)
        p_outs = List.map (function (id,(wids,ty)) -> id, wids) box.b_outs;
        p_params = List.map eval_param box.b_params;
        p_vars = List.map eval_var box.b_vars;
        p_rules = Misc.list_map_index (fun i (r,ty,_) -> r, i+1) box.b_rules;
        p_rule = None;
        p_fns = None }
  | InpB kind ->
      bid,
      { p_name = box.b_name;
        p_kind = InpP kind;
        p_ins = List.map (function (id,(wid,ty)) -> id, wid) box.b_ins;     (* Channel id = Wire id *)
        p_outs = List.map (function (id,(wids,ty)) -> id, wids) box.b_outs;
        p_params = List.map eval_param box.b_params;
        p_vars = [];
        p_rules = [];
        p_rule = None;
        p_fns = 
          let ty = get_out_type box.b_outs  in
          let rdy_fn, get_fn, close_fn =
            begin match kind with
              StreamIO -> Streams.mk_input_stream tp ty box.b_device
            | PortIO -> Ports.mk_input_port box.b_name tp ty box.b_device box.b_ival
            end in
          Some { pf_rdy=rdy_fn; pf_get=get_fn; pf_put=(fun t v -> ()); pf_close=close_fn } }
  | OutB kind ->
      bid,
      { p_name = box.b_name;
        p_kind = OutP kind;
        p_ins = List.map (function (id,(wid,ty)) -> id, wid) box.b_ins;     (* Channel id = Wire id *)
        p_outs = List.map (function (id,(wids,ty)) -> id, wids) box.b_outs;
        p_params = List.map eval_param box.b_params;
        p_vars = [];
        p_rules = [];
        p_rule = None;
        p_fns = 
          let ty = get_inp_type box.b_ins in
          let put_fn, close_fn = 
           begin match kind with
             StreamIO -> Streams.mk_output_stream ty box.b_device
           | PortIO -> Ports.mk_output_port ty box.b_device
           end in
          Some { pf_rdy=(function t -> true); pf_get=(function t -> None); pf_put=put_fn; pf_close=close_fn } }
  | DummyB -> 
      Misc.fatal_error "Dynamic.mk_process: dummy box"
    
and eval_var (id, (iv,ty)) = match iv, array_dim ty with
  None, (_,[]) ->                      (* scalar *)
    id, ref Val_unknown
| None, (_,[SzConst s1]) ->                       
    id, ref (Val_array1 (s1, Array1.make s1 Val_unknown))
| None, (_,[SzConst s1; SzConst s2]) ->            
    id, ref (Val_array2 ((s1,s2), Array2.make (s1,s2) Val_unknown))
| None, (_,[SzConst s1; SzConst s2; SzConst s3]) -> 
    id, ref (Val_array3 ((s1,s2,s3), Array3.make (s1,s2,s3) Val_unknown))
| None, (_, _) ->
    invalid_array_dimension id 
| Some v, (_,[]) ->                    (* scalar *)
    id, ref v
| Some (Val_array1 (n,a)), (_,[SzConst s1]) ->
    id, ref (Val_array1 (s1, Array1.copy a))
| Some (Val_array2 ((n1,n2),a)), (_,[SzConst s1; SzConst s2]) ->
    id, ref (Val_array2 ((s1,s2), Array2.copy a))
| Some (Val_array3 ((n1,n2,n3),a)), (_,[SzConst s1; SzConst s2; SzConst s3]) ->
    id, ref (Val_array3 ((s1,s2,s3), Array3.copy a))
  (* Note 2011-09-22, JS.
     Array copying is required to prevent the builtin sharing of array values in Caml.
     A cleaner approach would be to get back to a purely applicative semantics for process variables *)
| _, _ ->
   failwith "Dynamic.eval_var" (* should not happen ?? *)

and eval_param = function id, (v,ty) -> id, v
    (* Evaluation of box params has been done when instanciating the corresp. actor *)

and get_inp_type = function 
    [] -> fatal_error "Dynamic.get_io_type"
  | (id,(_,ty)) :: _ -> ty

and get_out_type = function 
    [] -> fatal_error "Dynamic.get_io_type"
  | (id,(_,ty)) :: _ -> ty

(* Rule |- Wire => Channel *)

let mk_channel boxes (wid,w)=
  wid,  (* Channels id = wire id *)
  { ch_traced=false;
    ch_stat={max_occ=0};
    ch_cap =
      if (src_box_of_wire boxes w).b_tag = InpB PortIO || (dst_box_of_wire boxes w).b_tag = OutB PortIO
      then 2
      else !default_channel_capacity;
    (* TODO : accept capacity annotation on channels ? *)
    ch_val=[] }
  

(* Rule TE, EE0 |- Program => P,C - Part 1 *)

let build_dynamic tp sp senv =
{ d_vals = senv.se_vals @ sp.e_vals;
  d_procs = List.map (mk_process tp sp) sp.boxes;         (* Rule |- B => P *)
  d_channels = List.map (mk_channel sp.boxes) sp.wires }    (* Rule |- W => C *)

let close_stream (pid,p) = match p.p_kind, p.p_fns with
  RegularP, _ -> ()
| _, Some { pf_close=close} -> close !current_cycle
| _, None -> fatal_error "Dynamic.close_stream" (* should not happen *)

let finalize_env env =
  List.iter close_stream env.d_procs

let extern_fns_required denv =
  List.exists (function (id,Val_extern _) -> true | _ -> false) denv.d_vals

(* Printing *)

let print_value (id, v) =
  Printf.printf "val %s = %s\n" id (Expr.string_of_val v)

let print_channel (cid, c) = 
  Printf.printf "W%d%s : [%s]\n"
    cid
    (if c.ch_traced then " (traced) " else "")
    (Misc.string_of_list Expr.string_of_val "," c.ch_val)

let rec print_process (pid, p) = match p.p_kind with
  RegularP ->
    Printf.printf "P%d (R,%s) : ins=[%s] outs=[%s] params=[%s] vars=[%s]\n"
      pid
      p.p_name
      (Misc.string_of_list string_of_process_inp ";" p.p_ins)
      (Misc.string_of_list string_of_process_outps ";" p.p_outs)
      (Misc.string_of_list string_of_process_param ";" p.p_params)
      (Misc.string_of_list string_of_process_var "," p.p_vars)
| InpP kind ->
    Printf.printf "P%d (I,%s) : out=[%s]\n"
      pid p.p_name (Misc.string_of_list string_of_process_outps ";" p.p_outs)
| OutP kind ->
    Printf.printf "P%d (O,%s) : in=[%s]\n"
      pid p.p_name (Misc.string_of_list string_of_process_inp ";" p.p_ins)

and string_of_process_inp (id,cid) = id ^ "<-W" ^ string_of_int cid
and string_of_process_outps (id,cids) = Misc.string_of_list (string_of_process_outp id) "," cids
and string_of_process_outp id cid = id ^ "->W" ^ string_of_int cid
and string_of_process_var (id,iv) = id ^ "=" ^ Expr.string_of_val !iv
and string_of_process_param (id,v) = id ^ "=" ^ Expr.string_of_val v
and string_of_process_kind = function RegularP -> "P" | InpP _ -> "I" | OutP _ -> "O"

let rec string_of_drule (qpats,guards,qexps) =  
      string_of_list string_of_qpat "," qpats
    ^ (match Syntax.string_of_guards guards with "" -> "" | s -> " " ^ s)
    ^ " -> " ^ string_of_list string_of_qexp "," qexps
and string_of_qpat (q,pat) = Syntax.string_of_rpat pat 
and string_of_qexp (q,exp) = Syntax.string_of_expr exp 


let print_dyn_env denv =
  Printf.printf "Dynamic environment ---------------\n";
  List.iter print_value denv.d_vals;
  List.iter print_process denv.d_procs;
  List.iter print_channel denv.d_channels;
  Printf.printf "----------------------------------\n"
