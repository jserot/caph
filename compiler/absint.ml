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

(* Abstract interpretation of boxes *)
(* Used for classifying boxes according to their MoC and to generate FSMs *)

(* The abstract interpreter is a "simplified" version of the interpreter in which 
   dynamic token values are simply ignored. The behavior of a box is then approximated
   ("abstracted") by that which can be deduced from statically computable values of its
   local variables *)

(* Added v2.8.4, July 2017, JS *)

open Expr
open Misc
open Error
open Printf
open Syntax
open Static

type absint_config = {
  mutable ai_max_cycles: int;
  }

let cfg = {
  ai_max_cycles = 32;
}

type ai_val =                  
    AV_Known of Expr.e_val
  | AV_Unknown                

exception Val_of_av

let sv_is_known = function AV_Known _ -> true | AV_Unknown -> false
let val_of_sv = function AV_Known v -> v | AV_Unknown -> raise Val_of_av

let string_of_abstract_value = function
    AV_Known v -> Expr.string_of_val v
  | AV_Unknown -> "?"

type local_env = (string * ai_val) list

let rec eval_static_expression tenv env expr =
  let open Syntax in
  let lookup_env id =
    try List.assoc id env
    with Not_found -> AV_Unknown in
  match expr.e_desc with
  | EConst c -> AV_Known (eval_const expr.e_loc c)
  | EVar id -> 
      begin
        match lookup_env id with
          AV_Known Val_unknown -> Error.uninitialized_var expr.e_loc id
        | v -> v
      end
  | ECon (c,es) -> 
      let vs = List.map (eval_static_expression tenv env) es in
      if List.for_all sv_is_known vs then
        AV_Known (Val_con (c, List.map val_of_sv vs))
      else
        AV_Unknown
  | EApp(fn, _, args) ->
      begin match eval_static_expression tenv env fn with
        AV_Known val_fn ->
          let vs = List.map (eval_static_expression tenv env) args in
          if List.for_all sv_is_known vs then
            let val_args =  List.map val_of_sv vs in
            begin match val_fn with
            | Val_prim primitive ->
                AV_Known (primitive (tuplify_value_list val_args))
            | Val_fun (_, fargs, exp) ->
                let env' = List.combine fargs vs in
                eval_static_expression tenv (env' @ env) exp
            | Val_extern { ef_ml_name = f } ->
                let v = Foreign.call f (List.map (cast_to_int expr.e_loc) val_args) in
                AV_Known (Val_int (v, None))
            | _ ->
                Error.application_of_non_function_err expr.e_loc
            end
          else
            AV_Unknown
      | _ -> AV_Unknown
      end
  | ELet([id,exp], body) ->
      let v = eval_static_expression tenv env exp in
      eval_static_expression tenv ((id,v)::env) body
  | ELet(_, _) ->
      failwith "should not happen" (* Multiple let-defs are only introduced in the interm repr *)
  | ECond(e1, e2, e3) ->
      begin match eval_static_expression tenv env e1 with
        AV_Known (Val_bool true) -> eval_static_expression tenv env e2
      | AV_Known (Val_bool false) -> eval_static_expression tenv env e3
      | AV_Known _ -> fatal_error "Expr.eval_expression : first arg of ECond is not a boolean value"
      | AV_Unknown -> AV_Unknown 
      end
  | ETuple es ->
      let vs = List.map (eval_static_expression tenv env) es in
      if List.for_all sv_is_known vs then
        AV_Known (Val_tuple (List.map val_of_sv vs))
      else
        AV_Unknown
  | _ ->
      Error.illegal_static_expression expr.e_loc

(*   A box is fireable if at least one of its rules is "fireable". *)
(*   A rule is fireable if both all its LHS patterns and RHS expressions are ready. *)

let rec fireable_rules ~strict:strict tenv env b =
  (* [strict] is the list of variables whose actual value of will be taken into account *)
  match b.b_tag with
  | RegularB -> List.filter (is_fireable ~strict:strict tenv env b) b.b_rules
  | _  -> []

and is_fireable ~strict:strict tenv env b ((qpats,guards,qexps),_,_) =
     List.for_all (rpat_rdy ~strict:strict tenv env b) qpats
  && List.for_all (eval_static_guard ~strict:strict tenv env) guards

and rpat_rdy ~strict:strict tenv env b qpat = match qpat with
  {q_desc=QIn i}, pat -> true (* An input channel is always ready and ok for the abstract interpreter *)
| {q_desc=QVar (v,_) as vp}, pat ->
    if List.mem v strict then 
      let vv = get_local_var pat.rp_loc tenv env b vp in
      check_var_pattern v vv pat
    else
      true
| _ -> fatal_error "Absint.rpat_rdy" (* should not happen *)

and get_local_var loc tenv env b vp = match vp with
  QVar (v, []) ->       (* Scalar; ex: v:pat *)
    List.assoc v env
| QVar (v, idxs) ->   (* Array; ex: t[2]:pat *)
    begin match List.assoc v env with
      AV_Known a ->
        let ixs = List.map (eval_static_expression tenv env) idxs in
        if List.for_all sv_is_known ixs then 
          AV_Known (Expr.array_get loc a (List.map val_of_sv ixs))
        else
        unknown_static_value "pattern" loc v
    | AV_Unknown ->
        unknown_static_value "pattern" loc v
    end
| _ -> failwith "Absint.get_local_var" (* should not happen *)

and check_var_pattern vid v pat = 
  let rec check pat v = match pat.rp_desc, v with
    RPatWild, _ -> true    (* _ means "dont care", so matching always succeeds in this case *)
  | RPatVar _, _ -> true   (* matching against a var always succeeds *)  (* TODO : augment bindings ! *)
  | RPatConst (Const.CInt (v',_,_)), AV_Known (Val_int (v'',_)) -> v' = v''
  | RPatConst (Const.CInt (v',_,_)), AV_Unknown -> unknown_static_value "pattern" pat.rp_loc vid
  | RPatCon (c,[]), AV_Known (Val_con (c',[])) -> c = c'
  | RPatCon (c,ps), AV_Known (Val_con (c',vs)) when List.for_all is_simple_rule_pattern ps ->
      c = c' && List.for_all2 check ps (List.map (function v -> AV_Known v) vs)
  | RPatCon (c,_), AV_Unknown -> unknown_static_value "pattern" pat.rp_loc vid
  | _ -> illegal_rule_pattern pat.rp_loc in
  check pat v

and eval_static_guard ~strict:strict tenv env e = match eval_static_expression tenv env e with
| AV_Unknown -> true  (* A guard which cannot be statically evaluated is assumed to be true *)
| AV_Known (Val_bool b) -> b
| AV_Known _ -> fatal_error "Absint.eval_static_guard" (* should not happen *) 

(* Box abstract execution *)

(*
The abstract execution cycle for a box is as follows :
- step 1 : get all fireable rules (using a environment containing the value - even unknown - of all local variables)
- step 2 : select one (the first ?)
- step 3 : evaluate the RHS of the selected rule, producing values to update local variables
*)

(* Evaluation of rule expressions *)

let eval_qual_expr tenv env box acc (qual,expr) = 
  match qual.q_desc, eval_static_expression tenv env expr with
| QVar (id,[]), v -> 
    (id,v) :: acc
| QVar (id,indexes), v -> 
    begin match List.assoc id env with
      AV_Known a ->
        let ixs = List.map (eval_static_expression tenv env) indexes in
        if List.for_all sv_is_known ixs && sv_is_known v then 
          let a' = Expr.array_replace expr.e_loc a (List.map val_of_sv ixs) (val_of_sv v) in
          (id, AV_Known a') :: acc
        else
          unknown_static_value "expression" expr.e_loc id
    | AV_Unknown ->
        unknown_static_value "expression" expr.e_loc id
    end
| _, _ -> acc

let eval_qual_exprs tenv env box qexps =
  List.fold_left (eval_qual_expr tenv env box) [] qexps

let string_of_local_var (n,v)= n ^ "=" ^ string_of_abstract_value v
let string_of_local_env = Misc.string_of_list string_of_local_var ","

let merge_env env env' = 
  let rec merge = function
      [] -> []
    | (n,v)::rest -> 
        let v' = if List.mem_assoc n env' then List.assoc n env' else v in
        (n,v') :: merge rest in
  merge env

let box_static_env b =
      List.map
        (function (n,v) -> n, AV_Known v)
        Builtins.builtin_static_env.se_vals
    @ List.map
        (function (n,(v,_)) -> n, AV_Known v)
        b.b_params

type ai_result = 
  | AI_Cycle of int * (Static.b_rule * local_env) list 
  | AI_Overrun of int * (Static.b_rule * local_env) list 
  | AI_Blocked of int * (Static.b_rule * local_env) list 
  | AI_Nondet of int * (Static.b_rule * local_env) list 

let run_box b = 
  let senv = box_static_env b in
  let lenv0 =
    List.map
      (function
        | (n,(Some v,_)) -> n, AV_Known v
        | (n, (None,_)) -> n, AV_Unknown)
      b.b_vars in
  let tenv = [] in  (* TOFIX ? *)
  let rec step (lenv,acc,ncy) =
    if ncy >= cfg.ai_max_cycles then AI_Overrun (ncy ,List.rev acc)
    else if  ncy > 1 && lenv = lenv0 then AI_Cycle (ncy, List.rev acc)
    else begin
      match fireable_rules ~strict:(List.map fst b.b_vars) tenv (lenv @ senv) b with
      | [] ->
          AI_Blocked (ncy, List.rev acc)
      | [((qpats,gexps,qexps),_,_) as r] ->
          let lenv' = eval_qual_exprs tenv (lenv @ senv) b qexps in
          let lenv'' = merge_env lenv lenv' in
          step (lenv'',(r,lenv)::acc,ncy+1)
      |  rs ->
          AI_Nondet (ncy, List.rev acc)
      end in
  step (lenv0,[],0)

let run sp bid = 
  let b = try List.assoc bid sp.boxes with Not_found -> invalid_box_id bid in
  let open Printf in
  let bid = "B" ^ string_of_int b.b_id ^ "(" ^ b.b_name ^ ")" in
  let print_result msg ncy rs =
    printf "> %s at t=%d:\n" msg ncy;
    List.iter (function (r,env) -> printf ">   [%s] %s\n" (string_of_local_env env) (Static.string_of_rule_with_sig r)) rs in
  printf "> abstract interpretation of box %s ...\n" bid;
  begin match run_box b with
    AI_Cycle (ncy, rs) -> print_result  "cycle detected" ncy rs
  | AI_Blocked (ncy, rs) -> print_result  "blocked (no fireable rule)" ncy rs
  | AI_Overrun (ncy, rs) -> print_result "overrun" ncy rs
  | AI_Nondet (ncy, rs) -> print_result "non determinitic (dynamic) choice" ncy rs
  end;
  printf "> done\n"

