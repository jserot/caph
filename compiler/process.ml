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
open Dynamic
open Misc
open Error
open Printf
open Trace
open Syntax
open Arrays

let max_run_cycles = ref (-1)
let min_run_cycles = ref (-1)
let warn_on_full_channels = ref false

(* Channel manipulation *)

let push_channel v c = 
  let c' = { c with ch_val = c.ch_val @ [v] } in
  if !Dynamic.dump_channel_stats then Trace.stat_channel c';
  c'

let pop_channel c = 
  match c.ch_val with
    [] -> fatal_error "Process.pop_from_channel: empty channel"  (* should not happen *)
  | v::vs -> 
      let c' = { c with ch_val = vs } in
      if !Dynamic.dump_channel_stats then Trace.stat_channel c';
      c'
      
(* Pattern matching *)

exception Matching_fail
exception UninitializedVar

(* Rule |- pat, v => Bool, E *)

let rec matching v pat =
  match (v, pat.rp_desc) with
  | (Val_int (i1,_), RPatConst(Const.CInt (i2,_,_))) when i1=i2 -> []
  | (Val_bool b1, RPatConst(Const.CBool b2)) when b1=b2 -> []
  | (v, RPatVar id) -> [id, v]
  | (v, RPatWild) -> []
  | Val_con(c1,[Val_tuple vs]), RPatCon(c2,ps) when c1=c2 -> List.concat (List.map2 matching vs ps)
  | Val_con(c1,vs), RPatCon(c2,ps) when c1=c2 -> List.concat (List.map2 matching vs ps)
  | (_, _) -> raise Matching_fail

and is_matching v pat =
  try let _ = matching v pat in true with Matching_fail -> false

(* Semantics of processes *)

(* Rule TE, EE0 |- Program => P,C - Part 2 *)
     
(* Rule TE,EE,C |- P => C', P' *)

let rec eval tenv denv =
  let inactive, active = split_ps tenv denv.d_vals denv.d_channels denv.d_procs in
  let _ = current_cycle := 0 in
  let channels', procs' = run_ps tenv denv.d_vals denv.d_channels (active, inactive) in
  { denv with d_channels = channels'; d_procs = procs' }

(* Rule E, C |- I, A => C', P' *) 

and run_ps tenv env channels = function
    [], inactive when !min_run_cycles < 0 || !current_cycle >= !min_run_cycles ->
        channels, inactive
  | active, inactive ->
      if !Trace.tracing then begin 
        dump_channels "> run_ps" channels;
        dump_proc_sets "> run_ps" active inactive end;
      if !max_run_cycles > 0 && !current_cycle >= !max_run_cycles then
        channels, active@inactive
      else
      let channels', procs' = exec_ps !current_cycle tenv env channels active in
      let inactive', active' = split_ps tenv env channels' (inactive @ procs') in
      let _ = incr current_cycle in
      run_ps tenv env channels' (active', inactive') (* Be sure to be tail-recursive here ! *)

(* Rule E, C |- P => C', P' *)

and exec_ps ncy tenv env channels aps = 
  let channels', aps' = Misc.map_foldl (exec_p ncy tenv env) channels aps in
  (* TODO ? alt semantics: env' = exec_p env channels (choose aps) *)
  channels', aps'

(* Identification of active processes *)

and split_ps tenv env channels procs = 
  List.fold_left (split_p tenv env channels) ([],[]) procs

(*   A regular process is tagged as active if 
     - at least one of its rules is "fireable".
     An input process is tagged as active if
     - one input data is ready for reading and all connected channels are ready for writing
     An output process is tagged as active if
     - one input data is available on input.  *)
(*   A rule is fireable if both all its LHS patterns and RHS expressions are ready. *)

and split_p tenv env channels (ips,aps) ((id,pp) as p) =
  match pp.p_kind with
  RegularP -> 
    begin try
      let r = List.find (fireable_rule tenv env channels pp) pp.p_rules in
      pp.p_rule <- Some r;
      (ips, p::aps)
    with Not_found ->
      (p::ips, aps) end
| InpP _ -> 
    begin match pp.p_fns with
      Some { pf_rdy=f } ->
        if f !current_cycle && List.for_all (function (o,cids) -> List.for_all (ready_outp channels) cids) pp.p_outs
        then (ips, p::aps)
        else (p::ips, aps)
    | None -> fatal_error "Process.split_p: no ready function for input actor" end (* should not happen *)
| OutP _ ->
      if List.for_all (available_input channels) pp.p_ins (* normally, only one input channel *)
      then ips, p::aps 
      else p::ips, aps

and available_input channels (id,cid) =
  let c = List.assoc cid channels in
  match c.ch_val with
    [] -> false
  | _ -> true

and fireable_rule tenv env channels p ((qpats,guards,qexps),no) =
     List.for_all (rpat_rdy tenv env channels p) qpats
  && (let env_p, _ = bind_rpats tenv env false channels p qpats
      and env_v = List.map (function (id, v) -> id, !v) p.p_vars in
      List.for_all (guard_ok tenv (env_p @ env_v @ p.p_params @ env) p) guards)
         (* Note 2013-04-12, JS 
            Added local vars to env for evaluating guards *)
  && List.for_all (rexp_rdy channels p) qexps

and rpat_rdy tenv env channels p qpat = match qpat with
        {q_desc=QIn i}, pat -> ready_or_ignored_inp channels (List.assoc i p.p_ins) pat 
      | {q_desc=QVar (v,k)}, pat -> compatible_variable tenv env p v k pat
      | _ -> fatal_error "Process.rpat_rdy" (* should not happen *)

and ready_or_ignored_inp channels cid pat =
  match pat.rp_desc with
    RPatWild -> true
  | _ -> ready_inp channels pat cid

and ready_inp channels pat cid = 
  let c = List.assoc cid channels in
  match c.ch_val, pat with
    [], _ -> false
  | v::vs, p -> is_matching v p


and compatible_variable tenv env p v k pat =
  let rec compat p v =
    begin match p.rp_desc, v with
      RPatWild, _ -> true    (* _ means "dont care", so matching always succeeds in this case *)
    | RPatVar _, _ -> true   (* matching against a var always succeeds *)
    | RPatConst (Const.CInt (v',_,_)), Val_int (v'',_) -> v' = v''
    | RPatCon (c,[]), Val_con (c',[]) -> c = c'
    | RPatCon (c,ps), Val_con (c',vs) when List.for_all is_simple_rule_pattern ps ->
        c = c' && List.for_all2 compat ps vs
    | _ -> illegal_rule_pattern pat.rp_loc
    end in
  match get_local_var pat.rp_loc tenv env p v k with
  | Val_unknown when pat.rp_desc <> RPatWild -> uninitialized_var_pat pat.rp_loc v
  | vv -> compat pat vv

and get_local_var loc tenv env p v k = 
  match k, !(List.assoc v p.p_vars) with
    [], v' -> v'
  | idxs, a -> 
      let env_v = List.map (function (id, v) -> id, !v) p.p_vars in
      let ixs = List.map (Expr.eval_expression [] (env_v @ env)) idxs in
      Expr.array_get loc a ixs

and guard_ok tenv env p e = match Expr.eval_expression [] env e with
  Val_bool b -> b
| _ -> fatal_error "Process.guard_ok: guard does not evaluate to bool value" (* should bot happen *)

and rexp_rdy channels p qexp = match qexp with
| {q_desc=QVar _}, _ -> true (* Variables are always reading for writing *)
| {q_desc=QOut o}, exp -> ready_or_ignored_output channels (List.assoc o p.p_outs) exp 
| _ -> fatal_error "Process.rexp_rdy" (* should not happen *)

and ready_or_ignored_output channels cids exp = match exp.e_desc with
  EIgnored -> true
| _ -> List.for_all (ready_outp channels) cids

and ready_outp channels cid = 
  let c = List.assoc cid channels in
  if List.length c.ch_val < c.ch_cap(*-1*)
  then true 
  else (if !warn_on_full_channels then warn_full_channel cid; false)
  

(* Individual process execution *)

(*
The execution cycle for a regular active process is as follows :

- step 1 : get the fireable rule (there must be one, since the process has been tagged as active).
- step 2 : bind the rule patterns to the corresponding values (consuming the values on the involved input channels)
- step 3 : evaluate the RHS of the selected rule, producing values to update outputs and local variables

The execution cycle for an input active process is as follows :

- step 1 : get input value (by reading file, socket or any connected input device)
- step 2 : write the value (this must be possible since the process has been tagged as active)

The execution cycle for an output process is as follows : 

- step 1 : read and consume available inputs
- step 2 : put the read values (by writing file, socket or any connected output device)

*)

(* Rule E, C |- Process => C', Process' *)

and exec_p ncy tenv env channels (pid,p) = 
  msg3 "> executing %s%d {%s}...\n"
    (string_of_process_kind p.p_kind)
    pid
    (Misc.string_of_list string_of_process_var "," p.p_vars);
  let channels', p' = match p.p_kind with
    RegularP ->
      begin match p.p_rule with
          Some ((qpats,guards,qexps) as r, no) ->
            let rid = "P" ^ string_of_int pid ^ "." ^ string_of_int no in 
            let env', channels' = bind_rpats tenv env true channels p qpats in
            msg5 "@%d rule %s [%s: %s] selected {env=[%s]}..."
              ncy
              rid
              p.p_name
              (Dynamic.string_of_drule r)
              (Misc.string_of_list Dynamic.string_of_binding ","
                 ((List.map (function (id,v) -> (id,!v)) p.p_vars) @ env'));
            let copy_var v =
              match !v with
              | Val_array1(sz, vs) -> Val_array1 (sz, Array1.copy vs)
              | Val_array2(sz, vs) -> Val_array2 (sz, Array2.copy vs)
              | Val_array3(sz, vs) -> Val_array3 (sz, Array3.copy vs)
              | v -> v in
            let env'' = List.map (function (id,v) -> (id, copy_var v)) p.p_vars in
              (* Note 2015-07-24, JS
                 We add a _copy_ of the local variables to the environment when evaluating the rule RHS.
                 This prevents read/write order dependency pbs *)
            let channels'', p' = eval_qual_exprs tenv (env' @ env'' @ p.p_params @ env) p channels' qexps in
            channels'', p'
        | None -> fatal_error "Process.exec_p: no fireable rule for active process"
          (* This should not happen since the process has been marked as active *)
      end
    | InpP _ ->
        begin match (io_fns p).pf_get !current_cycle with
          Some v -> 
            let cids =
              try List.assoc "o" p.p_outs    (* Warning : the output of input procs MUST be named "o" *)
              with Not_found -> fatal_error "Process.exec_p: cannot get output wire(s) for input process" in
            let channels' = List.fold_left (write_outp v) channels cids in
            channels', p
        | None -> fatal_error "exec_p: cannot read input"
          (* This should not happen since the process has been marked as active *)
        end
    | OutP _ ->
        let cid =
          try List.assoc "i" p.p_ins    (* Warning : the input of output procs MUST be named "i" *)
          with Not_found -> fatal_error "Process.exec_p: cannot get input wire for output process" in
        let v, channels' = read_input true channels cid in
        (io_fns p).pf_put !current_cycle v;
        channels', p
  in
  msg0 "done.\n";
  channels', (pid,p')

(* Binding of rule patterns *)

and bind_rpats tenv env destructive channels p qpats =
  List.fold_left (bind_rpat tenv env destructive p) ([],channels) qpats

and bind_rpat tenv genv destructive p (env,channels) qpat =
  match qpat with
  | {q_desc=QVar (v,k)}, pat ->
      let v' = get_local_var pat.rp_loc tenv (env @ genv) p v k in  
         (* Note 2014-04-23, JS. We need the global env here to evaluate index expressions *)
      let env' = match_var v pat v' in
      env' @ env, channels
  | {q_desc=QIn i}, {rp_desc=RPatWild} -> 
      env, channels
  | {q_desc=QIn i}, pat -> 
      let cid = List.assoc i p.p_ins in
      let v', channels' = read_input destructive channels cid in
      let env' = match_input pat v' in
      env' @ env, channels'
  | _ -> fatal_error "Process.bind_rpat" (* should not happen *)

and match_var vid pat v =
   match pat.rp_desc, v with
     RPatWild, _ -> []
   | _, Val_unknown -> uninitialized_var_pat pat.rp_loc vid
   | _, v' -> matching v' pat

and match_input pat v = 
   match pat.rp_desc, v with
     RPatWild, _ -> []
   | _, v' -> matching v' pat

and read_input destructive channels cid = 
  let c = List.assoc cid channels in
  match c.ch_val with
  | [] -> fatal_error ("read_input: channel " ^ (string_of_int cid) ^ " is empty !")
    (* should not happen, since we are reading for a pattern in a fireable rule *)
  | v::vs -> 
      if destructive then 
        let channels' = Misc.assoc_replace cid pop_channel channels in
        if c.ch_traced then Trace.trace_channel cid v;
        v, channels'
      else
        v, channels

(* Evaluation of rule expressions *)

and eval_qual_exprs tenv env p channels qexps =
  List.fold_left (eval_qual_expr tenv env p) channels qexps, p

and eval_qual_expr tenv env p channels (qual,expr) = 
  match qual.q_desc, Expr.eval_expression [] env expr with
  QVar _, Val_ignored -> channels
| QVar (vid,[]), v -> 
        let v' = List.assoc vid p.p_vars in
        v' := v;                      (* TODO : no mutable data ! *)
        channels
| QVar (vid,idxs), v -> 
    let a = !(List.assoc vid p.p_vars) in
    let ixs = List.map (Expr.eval_expression [] env) idxs in
    Expr.array_set expr.e_loc a ixs v;
    channels
| QOut oid, Val_ignored -> channels (* Should not happen, but anyway, ... *)
| QOut oid, v -> List.fold_left (write_outp v) channels (List.assoc oid p.p_outs)
| _, _ -> fatal_error "Process.eval_qual_expr_" (* should not happen *)

and write_outp v channels cid =
  let c = List.assoc cid channels in
  if List.length c.ch_val < c.ch_cap
  then Misc.assoc_replace cid (push_channel v) channels
  else channel_overwrite cid
