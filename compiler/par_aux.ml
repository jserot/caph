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

(* Auxiliary functions for parsing *)

open Location
open Syntax
open Misc

(* Global values *)

let mk_val_decl desc = ValDecl {val_desc = desc; val_loc = get_current_location(); val_typ = Types.no_scheme}
let mk_expr desc = {e_desc = desc; e_loc = get_current_location(); e_typ = Types.no_type }

let mk_binop op ({e_loc=Loc(f1,l1,m1)} as e1) ({e_loc=Loc(f2,l2,m2)} as e2) =
  { e_desc= EApp(mk_expr (EVar op),None,[e1;e2]); e_loc=Loc(f1,l1,m2); e_typ = Types.no_type }

let mk_unop op ({e_loc=Loc(f1,l1,m1)} as e1) =
  { e_desc= EApp(mk_expr (EVar op),None,[e1]); e_loc=Loc(f1,l1,m1); e_typ = Types.no_type }

(* Actors *)

let mk_actor desc = check_actor_desc desc; ActDecl {act_desc = desc; act_loc = get_current_location()}
and mk_ty_decl desc = TyDecl {td_desc = desc; td_loc = get_current_location()}
and mk_val_decl desc = ValDecl {val_desc = desc; val_loc = get_current_location(); val_typ = Types.no_scheme}
and mk_pragma_decl desc = PragmaDecl {pragma_desc = desc; pragma_loc = get_current_location()}
and mk_tyexp desc = {te_desc = desc; te_loc = get_current_location(); te_typ = Types.no_type}
and mk_szexp desc = {se_desc = desc; se_loc = get_current_location()}
and mk_param desc = {param_desc = desc; param_loc = get_current_location()}
and mk_act_io desc = {aio_desc = desc; aio_loc = get_current_location()}
and mk_var desc = {var_desc = desc; var_loc = get_current_location()}
and mk_rule_schema desc = {rsch_desc = desc; rsch_loc = get_current_location()}
and mk_rule desc = {rule_desc = desc; rule_loc = get_current_location()}
and mk_rule_lhs desc = {rlhs_desc = desc; rlhs_loc = get_current_location()}
and mk_rule_rhs desc = {rrhs_desc = desc; rrhs_loc = get_current_location()}
and mk_rule_grd desc = {rgrd_desc = desc; rgrd_loc = get_current_location()}
and mk_expr desc = {e_desc = desc; e_loc = get_current_location(); e_typ = Types.no_type}
and mk_rpat desc = {rp_desc = desc; rp_loc = get_current_location(); rp_typ = Types.no_type}
and mk_qualifier desc = {q_desc = desc; q_loc = get_current_location()}

let empty_rule_schema = { rsch_desc = ([],[]); rsch_loc = Location.no_location }

(* Streams and port IOs *)

let mk_io desc = IoDecl {io_desc = desc; io_loc = get_current_location()}

(* Network level declarations *)

let mk_net_decl desc = NetDecl {nd_desc = desc; nd_loc = get_current_location()}
let mk_net_expr desc = {ne_desc = desc; ne_loc = get_current_location(); ne_typ = Types.no_type }
let mk_net_pat desc = {np_desc = desc; np_loc = get_current_location(); np_typ = Types.no_type}
let mk_net_binding desc = {nb_desc = desc; nb_loc = get_current_location()}

let rec mk_napply = function
  | e1, [] -> e1
  | e1, e2::e2s -> mk_napply (mk_net_expr(NApp(e1, e2)), e2s)

let rec mk_nfun e = function
  | [] -> fatal_error "mk_nfun" (* should not happen *)
  | [p1] -> mk_net_expr(NFun (p1,e))
  | p::ps -> mk_net_expr(NFun (p, mk_nfun e ps))

let negate_expr s e = match s, e with
  | "-", {e_desc = EConst(Const.CInt (n,_,s))} -> mk_expr(EConst(Const.CInt(-n,Some Const.Signed,s)))
  | "-", {e_desc = EConst(Const.CFloat n)} -> mk_expr(EConst(Const.CFloat(-.n)))
  | s, e -> mk_unop ("~" ^ s) e

let negate_constant s c = match s, c with
  | "-", Const.CInt (n,_,s) -> Const.CInt(-n,Some Const.Signed,s)
  | "-", Const.CFloat n -> Const.CFloat(-.n)
  | _, _ -> not_implemented "unary minus only works on integer or float constants"

(* Programs *)

let mk_program decls = List.fold_left add_decl empty_program decls


