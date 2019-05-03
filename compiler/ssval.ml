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

(* Static semantic domain *)

open Types
open Location
open Expr

type ss_val =
  | SVVal of Expr.e_val  
  | SVTuple of ss_val list
  | SVHoPrim of string * int * ss_val list  (* Higher-order primitive (map, fold, ...); name, higher-order arity and arguments *)
  | SVClos of sv_clos
  | SVAct of sv_act
  | SVLoc of idx * sel * typ * bool (* node index, output selector, type, is_output *)

and sv_clos =
  { cl_pat: Syntax.net_pattern;
    cl_exp: Syntax.net_expr;
    mutable cl_env: (string * ss_val) list }

and idx = int
and sel = int

and sv_loc = idx * sel

and sv_act = {
    sa_id: string;
    sa_typ: typ_scheme;
    sa_fulltyp: typ_scheme;
    sa_params: (string * typ * Expr.e_val option) list;
    sa_ins: (string * typ) list;
    sa_outs: (string * typ) list;
    sa_vars: (string * (Syntax.expr option * typ) * location ) list; 
    sa_rules: (sa_rule * typ * sa_rsig * location) list;
    sa_types: (string * local_type_def) list;    (* locally defined types (enums) *)
    sa_impl: Syntax.actor_impl;
}

and sa_rule = Syntax.qualified_rule_pattern list * Syntax.expr list * Syntax.qualified_expr list

and sa_rsig = int list * int list (* Rule signature, for MoC-based actor classification *)

and local_type_def = SVEnumDefn of string list  (* list of (nullary) value ctors *)

let expr_of_sv loc = function 
    SVVal v -> v
  | _ -> Error.illegal_expression loc

let eval_const loc = function
    Const.CInt (v, Some Const.Signed, Some sz) -> Val_int (v, Some (Signed sz))
  | Const.CInt (v, Some Const.Unsigned, Some sz) -> Val_int (v, Some (Unsigned sz))
  | Const.CInt (v, _, _) -> Val_int (v, None)
  | Const.CBool v -> Val_bool v
  | Const.CFloat v -> Val_float v

let is_static_const = function
    SVVal _ -> true
  | _ -> false

let rec eval_sv expr =
  match expr.Syntax.ne_desc with
  | Syntax.NConst c -> SVVal (eval_const expr.Syntax.ne_loc c)
  | Syntax.NTuple es -> SVTuple (List.map eval_sv es)
  | _ -> Error.illegal_static_constant expr.Syntax.ne_loc

(* Printing *)

let rec string_of_typed_rule sep (r,ty,rs,loc) = string_of_rule sep r rs

and string_of_rule sep (qpats,gexps,qexps) rs =
      Misc.string_of_list Syntax.string_of_qualified_pattern ", " qpats
    ^ string_of_guard_exps gexps
    ^ sep
    ^ Misc.string_of_list Syntax.string_of_qualified_expr ", " qexps
    ^ "  " ^ string_of_rule_sig rs

and string_of_guard_exps = function
    [] -> ""
  | es -> ", " ^ Misc.string_of_list Syntax.string_of_expr ", " es

and string_of_rule_sig (rs1,rs2) =  Misc.string_of_two_lists string_of_int string_of_int "," rs1 rs2

let rec output_ss_value oc v = output_string oc (string_of_ssval v)

and  string_of_ssval v = match v with
  | SVVal v -> string_of_val v
  | SVLoc (l,s,ty,_(*,iv*)) -> 
      let ann = "" in (*match iv with None -> "" | Some v -> ",[" ^ v ^ "]" in *)
      "Loc(" ^ (string_of_int l) ^ "," ^ (string_of_int s) ^ ann ^ ")"
  | SVAct a -> "Act(...)"
  | SVClos _ -> "Clos(...)"
  | SVHoPrim (p,n,args) ->
      String.capitalize p ^ "<" ^ string_of_int n ^ "," ^ Misc.string_of_list string_of_ssval "," args ^ ">"
  | SVTuple vs -> "(" ^ Misc.string_of_list string_of_ssval "," vs ^ ")"

and output_ss_val_list oc sep l = Misc.output_list output_value oc sep l
