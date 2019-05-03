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

(* FSM dumping.
  Modified since vers 2.8.4 to operate on boxes instead of actors and to use the Absint module *)

open Syntax
open Static

type fsm_state = {
 fs_name: string;
 fs_values: Expr.e_val list;
 fs_ivalue: Expr.e_val option
  }

let string_of_box_id b = "actor " ^ b.b_name ^ " @ box B" ^ string_of_int b.b_id

let find_fsm_state b =
  let rec find = function
      [] -> None
    | (v, (iv,ty)) :: rest ->
        if Types.is_enum_type ty then
          Some {
            fs_name = v;
            fs_values =
              begin try
                match List.assoc (Types.ty_constr_name ty) b.b_types with
                | Ssval.SVEnumDefn cs -> List.map (function c -> Expr.Val_con (c, [])) cs
              with
                Not_found -> Misc.fatal_error "Fsm.find_fsm_state" (* should not happen *);
              end;
            fs_ivalue = iv;
            }
        else if Types.is_range_type ty then
          match Types.type_range_bounds ty with
          | Types.SzConst lo, Types.SzConst hi ->
              Some {
              fs_name = v;
              fs_values = Misc.list_make lo hi (function i -> Expr.Val_int (i, None));
              fs_ivalue = iv;
              }
          | _, _ ->
              Error.warning_dynamic_int_range (string_of_box_id b) v;
              None
        else find rest in
  find b.b_vars

let string_of_state name sv = name ^ Expr.string_of_val sv 

let dump_fsm_state oc name s =
      Printf.fprintf oc "%s [shape=circle,label=\"%s=%s\"];\n" (string_of_state name s) name (Expr.string_of_val s)

let transition_label (qpats,gexps,qexps) =
  "<<table border=\"0\" rows=\"*\"><tr><td>"
^ Misc.string_of_list Syntax.string_of_qualified_pattern ", " qpats
^ Misc.htmlize (Static.string_of_guard_exps gexps)
^ (match qexps with
  [] -> "</td></tr></table>>" 
| _ -> "</td></tr><tr><td>"
    ^ Misc.string_of_list Static.string_of_qualified_expr' ", " qexps
    ^ "</td></tr></table>>")

let dump_fsm_transition oc n (s1,r,s2) =
  Printf.fprintf oc "%s -> %s [label=%s];\n" (string_of_state n s1) (string_of_state n s2) (transition_label r)

let fsm_transitions box stv src =
  let senv = Absint.box_static_env box in
  let lenv =
    List.map
      (function (n,_) -> n, if n=stv then Absint.AV_Known src else Absint.AV_Unknown) (* All vars unknown except state *)
      box.b_vars in
  let tenv = [] in (* TO FIX ? *)
  let rs = Absint.fireable_rules ~strict:[stv] tenv (lenv @ senv) box in
  let rec find_dst_state = function 
      [] -> src  (* No explicit destination state in rule RHS *)
    | (n, Absint.AV_Unknown)::rest -> find_dst_state rest 
    | (n, Absint.AV_Known v)::rest -> if n=stv then v else find_dst_state rest in
  let is_state_test_exp e = match e.e_desc with
  | EApp ({e_desc=EVar op}, _, [{e_desc=EVar v}; _]) when v=stv -> true
  | EApp ({e_desc=EVar op}, _, [_; {e_desc=EVar v}]) when v=stv && List.mem op ["=";"<";">";"<=";">="] -> true
  | _ -> false in
  List.map
    (function ((qpats,gexps,qexps),_,_) -> 
      let lenv' = Absint.eval_qual_exprs tenv (lenv @ senv) box qexps in
      let dst = find_dst_state lenv' in 
      let qpats' = Misc.list_rem (function ({q_desc=QVar (v,[])}, _) when v=stv -> true | _ -> false) qpats in 
      let gexps' = Misc.list_rem is_state_test_exp gexps in
      let qexps' = Misc.list_rem (function ({q_desc=QVar (v,[])}, _) when v=stv -> true | _ -> false) qexps in
      (src, (qpats', gexps', qexps'), dst))
    rs

let dump_fsm (id,b) =
  match b.b_tag with
  | RegularB -> 
      begin match find_fsm_state b with
        None -> ()
      | Some state ->
          Pr_type.TypeVarNames.reset ();
          Pr_type.SizeVarNames.reset ();
          let fname = b.b_name ^ "_act_b" ^ string_of_int b.b_id ^ ".dot" in
          let oc = open_out fname in
          Printf.fprintf oc "digraph %s_act {\n" b.b_name;
          List.iter (dump_fsm_state oc state.fs_name) state.fs_values;
          let ts = Misc.flatmap (fsm_transitions b state.fs_name) state.fs_values in
          List.iter (dump_fsm_transition oc state.fs_name) ts;
          Printf.fprintf oc "}\n";
          Logfile.write fname;
          close_out oc
      end
  | _ ->
      ()

let dump_fsms sp =
  List.iter dump_fsm sp.boxes
