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

(* Intermediate representation for DOT, SystemC and VHDL backends *)

open Syntax
open Static
open Ssval
open Printf
open Error
open Types
open Typing

let dump_ir = ref false
let dump_static_fifo_sizes = ref false

type ir_config = {
  mutable ir_pv_prefix: string;  (* Prefix for renaming variables introduced by pattern matching in rule LHS *)
  mutable ir_fifo_size_file_suffix: string;
  }

let cfg = {
  ir_pv_prefix = "p_";
  ir_fifo_size_file_suffix = "fifo_sizes.dat";
}

let pv_name id = cfg.ir_pv_prefix ^ id
let un_pv_name id = Misc.string_after cfg.ir_pv_prefix id

type interm_repr = {
    ir_actors: (string * ir_actor) list;
    ir_consts: (string * Static.gc_desc) list;
    ir_globfns: (string * Syntax.gf_desc) list;
    ir_globtys: (string * Typing.tc_desc) list;   
    ir_extfns: (string * Expr.ext_fn_desc) list;
    ir_boxes: (Static.bid * ir_box) list;
    ir_wires: (Static.wid * ir_wire) list;
    mutable ir_wire_annots: (Static.wid * ir_wire_annot) list
  }

and ir_wire = (Ssval.sv_loc * Ssval.sv_loc) * Types.typ (* * wire_kind *)

and ir_wire_annot = {
    iw_phase: int;
    iw_fifo_sz: int;
    }

and ir_box = {
    ib_tag: Static.box_tag;                                               (* Idem Static.ss_box *)
    ib_name: string;
    ib_wsub: string;
    ib_typ: Types.typ;   (* "Functional" type, i.e. either [t_params -> t_ins -> t_outs] or [t_ins -> t_outs] *)
    ib_tysig: Types.typ;  (* "Signature" type, i.e. [t_params * t_ins * t_vars * t_outs] *)
    ib_tvbs: Types.typ var_bind list;
    ib_svbs: Types.siz var_bind list;
    ib_fpbs: (string * string)  list;
    ib_ninsts: int;
       (* Note 2015-06-12, JS.
         For boxes resulting from actor instanciation, we store the number of distinct instanciations
         of the corresponding actor. This information is used by the backend code generators *)
    ib_ins: (string * (wid * typ)) list;
    ib_outs: (string * (wid list * typ)) list;
    ib_types: (string * Ssval.local_type_def) list;
    ib_params: (string * (Expr.e_val * typ)) list;
    ib_device: string;
    ib_impl: Syntax.actor_impl;
    ib_vars: (string * (Expr.e_val option * Types.typ * var_kind * Syntax.qual_desc)) list;  
    ib_transitions: transition list;
    ib_ival: Expr.e_val option; (* for input ports *)
    ib_moc: Static.b_moc
  }

and ir_actor = {
    ia_name: string;
    ia_tvars: Types.typ var list;                (* List of generic type variables for polymorphic actors. *)
    ia_svars: Types.siz var list;                (* List of generic size variables for polymorphic actors. *)
    ia_ins: (string * Types.typ) list;
    ia_outs: (string * Types.typ) list;
    ia_types: (string * Ssval.local_type_def) list;
    ia_params: (string * Types.typ) list;
    ia_vars: (string * (Syntax.expr option * Types.typ * var_kind * Syntax.qual_desc)) list;
    ia_transitions: transition list;
    ia_insts: (ir_box, bid) ActInsts.t;         (* Instances *)
    ia_impl: Syntax.actor_impl
  }
    
and var_kind =
    IV_Regular    (* Locally declared variables *)
  | IV_Pattern    (* Variables appearing as patterns in rule LHS *)
  | IV_Let        (* Variables declared in let .. in .. exprs *)

and local_var = 
    VSimple of string                         
  | VArrLoc of string * Syntax.array_index list
        (* Note 2014-04-22.
           Variables, appearing as qualifiers both for rule patterns and rules exprs can be either
           - "simple" variables (declared with a scalar) type (ex: a:v)
           - "pseudo" variables, designating a location in an array (ex: t[2]:Data v) *)

and transition = condition list * action list

and condition = 
    CInpRdy of string * Types.typ                      (* Input is ready for reading (upstream channel is not empty) *)
  | COutRdy of string * Types.typ                      (* Output is ready for writing (downstream channel is not full) *)
  | CInpMatch of string * Syntax.rpat_desc * Types.typ (* Input value matches pattern *)
  | CVarMatch of local_var * Syntax.rpat_desc * Types.typ (* Variable matches pattern *)
  | CGuardExp of guard_expr                            (* Guard *)

and guard_expr = {
  ge_desc: guard_expr_desc;
  ge_typ: Types.typ
     (* Note 2011-12-05 : we need the type (of constant subexprs) for the VHDL backend ... *)
  }

and guard_expr_desc =
    GEConst of Const.t
  | GEApp of string * (Syntax.gf_desc * Types.typ) option * guard_expr list
  | GEArrRead of ident * guard_expr list (* name, index(es) *)
  | GECast of guard_expr * Types.typ
  | GEBoundInp of string * Syntax.rpat_desc * Types.typ
  | GEBoundVar of local_var * Syntax.rpat_desc * Types.typ
  | GEVar of string
        (* Note 2013-01-13, JS. Guards can refer to inputs, local vars or vars bound on the LHS of the rule. *)
       
and action =
  | AReadInp of string * Types.typ
                           (* [AReadInp i] means : read input [i] but dont care about its value *)
  | ABindInp of string * Syntax.rpat_desc * Types.typ  
                           (* [ABindInp(i,p,ty)] means : read input [i] and match its value against pattern [p] *)
  | ABindVar of local_var * Syntax.rpat_desc * Types.typ   
                           (* [ABindVar(v,p,ty)] means : read var [v] and match its value against pattern [p] *)
  | AWriteVar of local_var * Syntax.expr_desc * Types.typ 
                           (* [AWriteVar(v,e)] means evaluate expr [e] and write result to var [v] *)
  | AWriteOut of string * Syntax.expr_desc * Types.typ 
                           (* [AWriteOut(o,e)] means evaluate expr [e] and write result to output [v] *)

let find_box boxes bid = 
    try List.assoc bid boxes
    with Not_found -> Misc.fatal_error "Interm.find_box: cannot find box from id" (* should not happen *)

let mk_wire_annot p s = { iw_phase = p; iw_fifo_sz = s }

let string_of_wire_annot a = string_of_int a.iw_phase ^ "/" ^ string_of_int a.iw_fifo_sz

let rec string_of_local_var = function
  VSimple v -> v
| VArrLoc (v,ks) -> v ^ Misc.string_of_list string_of_array_loc "" ks

and string_of_array_loc k =  "[" ^ Syntax.string_of_expr k ^ "]"

let rec string_of_guard_expr_desc e = match e with
    GEConst c -> Const.string_of_const c
  | GEVar v -> v
  | GEApp (op,_,[e]) -> op ^ string_of_guard_expr e
  | GEApp (op,_,[e1;e2]) when is_binop op -> string_of_guard_expr e1 ^ " " ^ op ^ " " ^ string_of_guard_expr e2
  | GEApp (op,_,es) -> op ^ "(" ^ Misc.string_of_list string_of_guard_expr "," es ^ ")"
  | GEArrRead (ar,idxs) -> ar ^ Misc.string_of_list string_of_guard_index_expr "" idxs
  | GECast (e,t) -> "(" ^ string_of_guard_expr e ^ " : " ^ Pr_type.string_of_type t ^ ")"
  | GEBoundInp (i,RPatVar v,_) -> i
  | GEBoundInp (i,RPatCon("Data", [{rp_desc=RPatVar _}]),_) -> i ^ ".data"
  | GEBoundInp (i,_,_) -> failwith "Interm.string_of_guard_expr: cannot translate guard expression"
  | GEBoundVar (v,RPatVar v',_) -> string_of_local_var v
  | GEBoundVar (v,_,_) -> failwith "Interm.string_of_guard_expr: cannot translate guard expression"

and string_of_guard_index_expr e = "[" ^ string_of_guard_expr e ^ "]"

and string_of_guard_expr e = string_of_guard_expr_desc e.ge_desc

let lookup_ctor tp id =
  try List.assoc id tp.tp_ctors
  with Not_found -> failwith "Interm.lookup_ctor" (* should not happen *)

(* Fresh var name generator *)

let reset_var_names, new_var_name =
  let cnt = ref 0 in
  (function () -> cnt := 0),
  (function prefix -> incr cnt; prefix ^ "_" ^ string_of_int !cnt)

(* Let-lifting *)

let rename_let_vars (rule_no,id) exp =
  let rec rename tenv e = match e with
    EConst _ -> [], e
  | EVar v -> [], begin try EVar (Misc.fst13 (List.assoc v tenv)) with Not_found -> e end
  | EAttr (v,attr) -> [], begin try EAttr (Misc.fst13 (List.assoc v tenv),attr) with Not_found -> e end
  | ECon (c, es) ->
      let tenvs', es' = Misc.list_unzip (List.map (rename_e tenv) es) in
      List.concat tenvs', ECon(c, es')
  | ECond (e1,e2,e3) -> 
      let tenv1, e1' = rename_e tenv e1 in
      let tenv2, e2' = rename_e tenv e2 in
      let tenv3, e3' = rename_e tenv e3 in
      tenv1 @ tenv2 @ tenv3, ECond (e1', e2', e3')
  | ELet ([v,e1],e2) ->  
      let v' = new_var_name (v ^ "_r" ^ string_of_int rule_no ^ "_" ^ id) in
      let ty = Types.real_type e1.e_typ in
      let tenv' =[v,(v',rule_no,ty)] in
      let tenv1, e1' = rename_e tenv e1 in
      let tenv2, e2' = rename_e (tenv'@tenv) e2 in
      tenv1 @ tenv' @ tenv2,  ELet([v',e1'], e2')
  | ELet (_, _) -> [], e
  | EApp (op,ann,es) ->
      let tenvs', es' = Misc.list_unzip (List.map (rename_e tenv) es) in
      List.concat tenvs', EApp(op, ann, es')
  | EArrRead (ar,idxs) ->
      let tenv', idxs' =
        Misc.map_foldl
          (fun te idx -> let te', idx' = rename_e tenv idx in te @ te', idx')
          []
          idxs in
      tenv', EArrRead(ar, idxs')
  | EArrayExt1 _
  | EArrayExt2 _
  | EArrayExt3 _
  | EArrayCompr (_,_) -> Misc.fatal_error "Interm.rename_let_vars"  
        (* Note 2014-05-13, JS
           Should note happen since array extensions and comprehensions are no longer valid rule exprs since v2.2 *)
  | EIgnored -> [], e
  | ECast (e,t) -> 
      let tenv', e' = rename_e tenv e in
      tenv', ECast(e', t)
  | ETuple es ->
      let tenvs', es' = Misc.list_unzip (List.map (rename_e tenv) es) in
      List.concat tenvs', ETuple es'
  and rename_e tenv e = 
    let tenv', e' = rename tenv e.e_desc in
    tenv', { e with e_desc = e' }
  in
  reset_var_names();
  rename_e [] exp

let let_lift exp =
  let extract es =
    Misc.map_foldl
      (fun z e -> match e.e_desc with ELet(bs,e') -> z@bs, e' | _ -> z, e)
      []
      es in
  let rec lift exp =
    let mk e' = { e_desc = e'; e_typ=exp.e_typ; e_loc=exp.e_loc } in
    match exp.e_desc with 
    EConst _ -> exp
  | EVar _-> exp
  | EAttr _-> exp
  | ECon (c, es) ->
      begin match extract (List.map lift es) with
        [], es' -> exp
      | bs, es' -> mk (ELet(bs, mk (ECon (c,es')))) end
  | ECond (e1,e2,e3) ->
      begin match extract [lift e1; lift e2; lift e3] with
        [], _ -> exp
      | bs, [e1';e2';e3'] -> mk (ELet(bs, mk (ECond(e1',e2',e3'))))
      | _, _ -> failwith "should not happen" end
  | ELet ([(v,e1)], e2) ->
      begin match lift e1, lift e2 with
      | { e_desc = ELet(bs1,e1'') }, { e_desc = ELet(bs2,e2'') } -> 
          mk (ELet(bs1@[v,e1'']@bs2, e2''))
      | { e_desc = ELet(bs1,e1'') }, e2' ->
          mk (ELet(bs1@[v,e1''], e2'))
      | e1', { e_desc = ELet(bs2,e2'') } ->
          mk (ELet([v,e1']@bs2, e2''))
      | e1', e2' -> exp end
  | ELet (_,_) ->
      exp
  | EApp (op,ann,es) ->
      begin match extract (List.map lift es) with
        [], es' -> exp
      | bs, es' -> mk (ELet(bs, mk (EApp (op,ann,es')))) end
  | EArrRead (ar,[e]) ->
      begin match extract (List.map lift [e]) with
        [], _ -> exp
      | bs, [e'] -> mk (ELet(bs, mk (EArrRead(ar, [e']))))
      | _, _ -> failwith "should not happen" end
  | EArrRead (ar,[e1;e2]) ->
      begin match extract (List.map lift [e1;e2]) with
        [], _ -> exp
      | bs, [e1';e2'] -> mk (ELet(bs, mk (EArrRead(ar,[e1';e2']))))
      | _, _ -> failwith "should not happen" end
  | EArrRead (ar,[e1;e2;e3]) ->
      begin match extract (List.map lift [e1;e2;e3]) with
        [], _ -> exp
      | bs, [e1';e2';e3'] -> mk (ELet(bs, mk (EArrRead(ar,[e1';e2';e3']))))
      | _, _ -> failwith "should not happen" end
  | EArrRead (_,_) -> failwith "Interm.let_lift" (* should not happen *)
  | EArrayExt1 _
  | EArrayExt2 _
  | EArrayExt3 _
  | EArrayCompr (_,_) -> Misc.fatal_error "Interm.let_lift"  
        (* Note 2014-05-13, JS
           Should note happen since array extensions and comprehensions are no longer valid rule exprs since v2.2 *)
  | EIgnored -> exp
  | ECast (e,t) -> 
      begin match extract [lift e] with
        [], _ -> exp
      | bs, [e1'] -> mk (ELet(bs, mk (ECast(e1',t))))
      | _, _ -> failwith "should not happen" end
  | ETuple es ->
      begin match extract (List.map lift es) with
        [], es' -> exp
      | bs, es' -> mk (ELet(bs, mk (ETuple es'))) end
  in
  lift exp

let lift_let_exprs (rule_no,id) e = 
  let tenv, e' = rename_let_vars (rule_no,id) e in
  tenv, let_lift e'

(* Building intermediate representation of actors and boxes *)

let specialize_patvar tvbs svbs (id, (i, qual, n, ty)) = id, (i, qual, n, copy_type tvbs svbs ty)

let specialize_letvar tvbs svbs (id, (i, n, ty)) = id, (i, n, copy_type tvbs svbs ty)

let rec rename_vars_in_expr_desc vars e =
  let rec rename_re = rename_vars_in_expr vars
  and rename_var v = 
    try let v' = List.assoc v vars in v' 
    with Not_found -> v in
  match e with
  | EVar v -> EVar (rename_var v)
  | ECond (e1,e2,e3) -> ECond(rename_re e1, rename_re e2, rename_re e3)
  | ELet (bs,e2) -> ELet(List.map (function (v,e1) -> v,rename_re e1) bs, rename_re e2)
  | EApp (op,ann,es) -> EApp(op, ann, List.map rename_re es)
  | EArrRead (ar,idxs) -> EArrRead(rename_var ar, List.map rename_re idxs)
  | EArrayExt1 es -> EArrayExt1 (List.map rename_re es)
  | EArrayExt2 ess -> EArrayExt2 (List.map (List.map rename_re) ess)
  | EArrayExt3 esss -> EArrayExt3 (List.map (List.map (List.map rename_re)) esss)
  | EArrayCompr ([i,e1,e2],e3) -> EArrayCompr ([i,rename_re e1,rename_re e2], rename_re e3)
  | EArrayCompr (_,_) -> failwith "Interm.rename_vars_in_expr" (* should not happen *)
  | ECon(c,es) -> ECon (c, List.map rename_re es)
  | ECast (e,t) -> ECast (rename_re e, t)
  | ETuple es -> ETuple (List.map rename_re es)
  | e -> e

and rename_vars_in_expr vars e = { e with e_desc = rename_vars_in_expr_desc vars e.e_desc }

let rename_vars_in_local_var vars v = match v with
    VSimple _ -> v
  | VArrLoc (v, idxs) -> VArrLoc (v, List.map (rename_vars_in_expr vars) idxs)

let rec mk_box tp sp (bid,b) = 
  Pr_type.TypeVarNames.reset ();
  Pr_type.SizeVarNames.reset ();
  let local_vars = List.map (function (id,(_,ty)) -> id, ty) b.b_vars in
  let transitions, pat_vars, let_vars, _ = List.fold_left (mk_transition tp local_vars) ([],[],[],1) b.b_rules in
    (* [pat_vars] are variables introduced by pattern matching in rules LHS,
       [let_vars] are variables introduced by let expressions in rules RHS *)
  let pat_vars' = List.map (specialize_patvar b.b_tvbs b.b_svbs) pat_vars in
  let let_vars' = List.map (specialize_letvar b.b_tvbs b.b_svbs) let_vars in
  bid,                  
  { ib_tag = b.b_tag;
    ib_name = b.b_name;
    ib_wsub = b.b_wsub;
    ib_typ = b.b_typ;
    ib_tysig = b.b_tysig;
    ib_tvbs = b.b_tvbs;
    ib_svbs = b.b_svbs;
    ib_fpbs = b.b_fpbs;
    ib_ninsts =
      (match b.b_tag with 
        RegularB -> ActInsts.card (Misc.lookup "Interm.mk_box" b.b_name sp.gacts).ac_insts
      | _ -> 0);
    ib_ins = b.b_ins;
    ib_outs = b.b_outs;
    ib_types = b.b_types;
    ib_params = b.b_params;
    ib_device = b.b_device;
    ib_ival = b.b_ival;
    ib_impl = b.b_impl;
    ib_moc = b.b_moc;
    ib_vars = List.map (function id,(e,ty) -> id,(e,ty,IV_Regular,QVar (id,[]))) b.b_vars
                 (* Locally declared variables *)
            @ List.map (function (id',(id,q,n,ty)) -> id', (None, ty, IV_Pattern,q)) pat_vars'
                 (* Variables appearing in LHS patterns. [id] is the original name, [id'] after renaming, [q] the qualifier,
                    [n] the rule no and [ty] the type of the corresp. input/var *)
            @ List.map (function (id,(id',n,ty)) -> id', (None, ty, IV_Let,QNone)) let_vars'
                 (* Variables declared in [let .. in ..] RHS exprs. [id] is the original name, [id'] after renaming,
                    [n] the rule no and [ty] the infered type *);
    ib_transitions = List.rev transitions }

and mk_actor tp boxes (id,a) =
  let sa = a.ac_defn in
  let local_vars = List.map (function (id,(_,ty),_) -> id, ty) sa.sa_vars in
  let _ = reset_var_names () in
  let transitions, pat_vars, let_vars, _ =
    (* [pat_vars] are variables introduced by pattern matching in rules LHS,
       [let_vars] are variables introduced by let expressions in rules RHS *)
    List.fold_left
      (mk_transition tp local_vars)
      ([],[],[],1)
      (List.map (function (rule,ty,rsig,_) -> rule,ty,rsig) sa.sa_rules) in
  let ir_box_of b = Misc.lookup "Interm.mk_actor" b.b_id boxes in
  id,
  { ia_name = sa.sa_id;
    ia_ins = sa.sa_ins;
    ia_outs = sa.sa_outs;
    ia_params = List.map (function (id,ty,v) -> (id,ty)) sa.sa_params;
    ia_types =  sa.sa_types;
    ia_vars = List.map (function id,(e,ty),loc -> id,(e,ty,IV_Regular,QVar (id,[]))) sa.sa_vars
                 (* Locally declared variables *)
            @ List.map (function (id',(id,q,n,ty)) -> id', (None, ty, IV_Pattern,q)) pat_vars
                 (* Variables appearing in LHS patterns. [id] is the original name, [id'] after renaming, [q] the qualifier,
                    [n] the rule no and [ty] the type of the corresp. input/var *)
            @ List.map (function (id,(id',n,ty)) -> id', (None, ty, IV_Let, QNone)) let_vars
                 (* Variables declared in [let .. in ..] RHS exprs. [id] is the original name, [id'] after renaming,
                    [n] the rule no and [ty] the infered type *);
    ia_tvars = sa.sa_typ.ts_tparams;
    ia_svars = sa.sa_typ.ts_sparams;
    ia_transitions = List.rev transitions;
    ia_insts = ActInsts.map (function (b,bids) -> ir_box_of b, bids) a.ac_insts;
    ia_impl = sa.sa_impl }

and mk_transition tp vars (transitions,pat_vars,let_vars,rule_no) (rule,ty,_) = 
  match rule with
    lhs, guards, rhs  ->
      let conds, acts, pat_vars' = scan_rpats tp rule_no lhs in
      let conds', acts', let_vars', _ =  scan_rexps vars rule_no pat_vars' rhs in
      let conds'' = List.fold_left (scan_guard (acts,pat_vars')) [] guards in
      let t  = conds @ conds'' @ conds', acts @ List.rev acts' in
      t::transitions,
      merge_vars rule_no pat_vars pat_vars',
      let_vars @  let_vars',
      rule_no+1

and merge_vars rule_no vs vs' =
  (* Note 2012-07-06, JS :
     Modified to allow the same lhs pat var, with the same type, to appear in several _distinct_ rules.
     For ex :
       rules (e1,e2) -> s
       |     ( x, _) -> x
       |     ( _, x) -> x
     will be allowed (and will allocate only one local var in the interm repr).
     But 
       rules (e1,e2) -> s
       |     ( x, x) -> x 
     is of course forbidden.
     More annoyingly, the following, if e1 and e2 have different types, is also not supported (it should) :
       rules (e1,e2) -> s
       |     ( x, _) -> x
       |     ( _, x) -> x *)
  let merge vs (id,(id',q,ty)) =
    if List.mem_assoc id vs then
      begin match List.assoc id vs with
        _,_,n,ty' -> 
          if n = rule_no then duplicate_rule_pattern (un_pv_name id) else
          if type_equal ty ty' then vs
          else begin
            duplicate_pattern (un_pv_name id)
          end
      end
    else
      (id,(id',q,rule_no,ty))::vs
  in
  List.fold_left merge vs vs'

and scan_rpats tp rule_no rpats = List.fold_left (scan_qrpat tp rule_no) ([],[],[]) rpats

and scan_qrpat tp rule_no (cs,acs,bvs) (q,pat) = 
  match (q.q_desc,pat), Types.real_type pat.rp_typ with
  | (QIn i, { rp_desc=RPatWild }), ty ->      (* '_' for inputs means read ignore *)
      cs,   
      acs,
      bvs   
  | (QIn i, { rp_desc = RPatConst _ as p }), ty ->
      CInpRdy (i,ty) :: CInpMatch (i, p, ty) :: cs,      (* Conditions *)
      AReadInp (i,ty) :: acs,                            (* Actions *)
      bvs                                                (* Variables introduced (bounded) by pattern matching *)
  | (QIn i, { rp_desc=RPatVar v as p }), ty ->
      let v' = pv_name v in
      let p' = rename_var_in_pat v v' p in
      CInpRdy (i,ty) :: cs,
      ABindInp(i,p',ty) :: acs,
      (v',(v,QIn i,ty)) :: bvs
  | (QIn i, { rp_desc=RPatCon(_,[]) as p }), ty ->
      CInpRdy (i,ty) ::  CInpMatch(i,p,ty) :: cs,
      AReadInp (i,ty) :: acs,
      bvs
  | (QIn i, { rp_desc=RPatCon(cid, [{rp_desc=RPatConst c}]) as p }), ty ->
      CInpRdy (i,ty) :: CInpMatch (i, p, ty) :: cs,
      AReadInp (i,ty) :: acs,
      bvs
  | (QIn i, ({ rp_desc=RPatCon(cid, ps) } as p)), ty when List.for_all is_simple_rule_pattern ps ->  
      let bvs', p' = bind_tycon_pattern tp cid ty p ps in
      let bvs'' = List.map (function (v,ty) -> pv_name v, (v,QIn i,ty)) bvs' in
      CInpRdy (i,ty) ::  CInpMatch (i,p',ty) :: cs,
      ABindInp(i,p',ty) :: acs,
      bvs'' @ bvs 
  | (QVar (v,k), { rp_desc=RPatWild }), ty ->  (* '_' for variables means wildcard *)
      let _ = mk_local_var q.q_loc v k in      (* check index expr, anyway *)
      cs,                   
      acs, 
      bvs
  | (QVar (v,k), { rp_desc=RPatConst _ as p }), ty ->
      let lv = mk_local_var q.q_loc v k in
      CVarMatch (lv,p,ty) :: cs,
      acs,
      bvs
  | (QVar (v,k), { rp_desc=RPatVar v' as p }), ty ->
      let v'' = pv_name v' in
      let p' = rename_var_in_pat v' v'' p in
      let k' = List.map (rename_var_in_index_expr v' v'') k in
      let lv = mk_local_var q.q_loc v k' in
      cs,
      ABindVar(lv,p',ty) :: acs,
      (v'',(v',QVar (v,k),ty)) :: bvs
  | (QVar (v,k), { rp_desc=RPatCon(c,[]) as p }), ty ->
      let lv = mk_local_var q.q_loc v k in
      CVarMatch (lv,p,ty) :: cs,
      acs,
      bvs
  | (QVar (v,[]), ({ rp_desc=RPatCon(cid, ps) } as p)), ty when List.for_all is_simple_rule_pattern ps ->  
      let bvs', p' = bind_tycon_pattern tp cid ty p ps in
      let bvs'' = List.map (function (w,ty) -> pv_name w, (w,QVar (v,[]),ty)) bvs' in
      CVarMatch (VSimple v,p',ty) :: cs,
      ABindVar (VSimple v,p',ty) :: acs,
      bvs'' @ bvs 
  | (_, _), _ ->
      illegal_rule_pattern pat.rp_loc

and bind_tycon_pattern tp cid ty p ps = 
  let c = lookup_ctor tp cid in
  let t = type_instance { ts_tparams=fst c.cs_params; ts_sparams=snd c.cs_params; ts_body=type_arrow c.cs_arg c.cs_res } in
  let ty_arg = new_type_var () in
  begin
    try
      Types.unify t (type_arrow ty_arg ty) 

    with
      Types.TypeConflict _
    | Types.TypeCircularity _ ->
        Misc.fatal_error "Interm.bind_tycon_pattern: cannot retrieve actual type for pattern"
  end;
  let bvs' = extract_pat_var_bindings ps ty_arg in
  let vs' = List.map (function (v,_) -> v, pv_name v) bvs' in
  let { rp_desc = p' } = rename_vars_in_pat vs' p in
  bvs', p' 

and mk_local_var loc v k = match k with
  [] ->
   VSimple v
| ks ->
   if List.for_all is_valid_index_expr ks
   then VArrLoc (v, ks)
   else Error.invalid_index_expression loc

and is_valid_index_expr e = match e.e_desc with
  EConst _ -> true
| EVar _ -> true
| EApp ({e_desc=EVar op}, _, es) (* when Syntax.is_binop op *) -> List.for_all is_valid_index_expr es
| EArrRead (_, idxs) when List.for_all is_valid_index_expr idxs -> true
| ECast (e, _) when is_valid_index_expr e -> true
| _ -> false (* TODO : relax this ? *)
    
and extract_pat_var_bindings pats ty = 
  let extract bs p ty = match p.rp_desc, real_type ty with
  | RPatConst _, t -> p.rp_typ <- t; bs
  | RPatWild, t -> p.rp_typ <- t; bs
  | RPatVar v, t -> p.rp_typ <- t; (v,t)::bs
  | _, _ ->  failwith "Interm.extract_pat_var_bindings"   (* should not happen *) in
  match pats, real_type ty with
  | [p], t when is_simple_rule_pattern p ->
      extract [] p t
  | ps, Tproduct ts when List.for_all is_simple_rule_pattern ps && List.length ps = List.length ts ->
      List.fold_left2 extract [] ps ts
  | _, _ -> failwith "Interm.extract_pat_var_bindings"   (* should not happen *)
  
and rename_var_in_pat v v' p = match p with
  | RPatVar v'' when v'' = v  -> RPatVar v'
  | RPatCon (c, [{rp_desc = RPatVar v''} as p']) when v'' = v -> RPatCon(c, [{p' with rp_desc = RPatVar v'}])
  (* TODO : other cases of RPatCon ? *)
  | p -> p

and rename_vars_in_pat vs p = match p.rp_desc with
  | RPatVar v when List.mem_assoc v vs -> { p with rp_desc = RPatVar (List.assoc v vs) }
  | RPatCon (c, ps) -> { p with rp_desc = RPatCon(c, List.map (rename_vars_in_pat vs) ps) }
  | _ -> p

and rename_var_in_index_expr v v' e = match e.e_desc with
  | EVar v'' -> if v'' = v then { e with e_desc = EVar v' } else e
  | EConst _ -> e
  | EApp ({e_desc=EVar op} as f, ann, es) when Syntax.is_binop op ->
      { e with e_desc = EApp (f, ann, List.map (rename_var_in_index_expr v v') es) }
  | _ -> Error.invalid_index_expression e.e_loc (* TODO: extend  this ? *)

and scan_rexps local_vars rule_no pat_vars rexps =
  let conds, acts, let_vars, rd_vars, wr_vars =
    List.fold_left (scan_qrexp local_vars rule_no pat_vars) ([],[],[],[],[]) rexps in
  let fragile_vars =
     List.fold_left
       (fun vs v -> 
          match v with
            VSimple v', ty ->  if List.mem_assoc v' rd_vars then (v',ty)::vs else vs
          | VArrLoc _, _ -> vs)   (* TO FIX ??? *)
       []
       wr_vars in
     (* The fragile vars are those which are both updated and read in the RHS of the rule *)
     (* Note 2015-07-29, JS. This information is no longer used since v2.6.1 *)
  conds, acts, let_vars, fragile_vars 

and scan_qrexp local_vars rule_no pat_vars (cs,acs,lvars,rd_vars,wr_vars) (q,exp) =
  let pat_vars' = List.map (function (id',(id,q,ty)) -> (id,id')) pat_vars in (* Reverse association *)
  match (q.q_desc,exp), Types.real_type exp.e_typ with
  | (QOut _, { e_desc = EIgnored }), _ 
  | (QVar _, { e_desc = EIgnored }), _ ->
      cs,
      acs,
      lvars,
      rd_vars,
      wr_vars
  | (QOut o, e), ty ->
      let lvars', e' = lift_let_exprs (rule_no,o) e in
      let e'' = rename_vars_in_expr pat_vars' e' in
      COutRdy (o,ty) :: cs,
      AWriteOut(o, e''.e_desc, ty) ::  acs,
      lvars @ lvars',
      extract_vars_of_expr local_vars e @ rd_vars,
      wr_vars
  | (QVar (v,k), e), ty ->
      let lvars', e' = lift_let_exprs (rule_no,v) e in
      let e'' = rename_vars_in_expr pat_vars' e' in
      let k' = List.map (rename_vars_in_expr pat_vars') k in
      let lv = mk_local_var q.q_loc v k' in
      cs,
      AWriteVar (lv, e''.e_desc, ty) ::  acs,
      lvars @ lvars',
      extract_vars_of_expr local_vars e @ rd_vars,
      (lv,ty) :: wr_vars
  | (_, _), _ -> illegal_rule_expression exp.e_loc

and extract_vars_of_expr local_vars e =
  let rec extract_var v = 
    if List.mem_assoc v local_vars
    then  [v, List.assoc v local_vars]
    else []
  and extract_re e = match e with
  | EVar v -> extract_var v
  | ECond (e1,e2,e3) -> extract e1 @ extract e2 @ extract e3
  | ELet (bs,e2) -> Misc.flatmap (function (v,e1) -> extract e1) bs @ extract e2
  | EApp (op,_,es) -> Misc.flatmap extract es
  | EArrayExt1 es ->  Misc.flatmap extract es
  | EArrayCompr ([i,e1,e2],e3) -> extract e1 @ extract e2 @ extract e3
  | EArrayCompr (_,_) -> failwith "Interm.extract_vars_of_expr"   (* should not happen *)
  | EArrRead (ar,idxs) -> extract_var ar @ List.concat (List.map extract idxs)
  | ECon(c,es) -> Misc.flatmap extract es
  | ECast (e,t) -> extract e
  | ETuple es ->  Misc.flatmap extract es
  | _ -> []
  and extract e = extract_re e.e_desc in
  extract e

and scan_guard (bind_acts,pat_vars) cs e =
  let pat_vars' = List.map (function (id',(id,q,ty)) -> (id,(id',q,ty))) pat_vars in (* Reverse association *)
  CGuardExp (translate_guard_exp (bind_acts,pat_vars') e) :: cs

and translate_guard_exp (bind_acts,pat_vars) e =
  let rec translate loc e = match e with
  | EConst c -> GEConst c 
  | EVar v ->
     begin try match List.assoc v pat_vars with
     | _, QIn i, _  ->
         let pat, ty = find_bound_input loc i bind_acts in
         GEBoundInp (i,pat,ty)
     | _, QVar (v,k), _  ->
         let v', pat, ty = find_bound_var loc v bind_acts in
         GEBoundVar (v',pat,ty)
     | _, _, _ ->
         illegal_guard_expr loc 
     with Not_found ->
        (* This should be a parameter *)
       GEVar v
     end
  | EApp ({e_desc=EVar f}, ann, es) -> GEApp (f, ann, List.map transl es)
  | EArrRead (a, idxs) -> GEArrRead (a, List.map transl idxs)
  | ECast (e, ty) -> GECast (transl e, e.e_typ)
  | _ ->
      illegal_guard_expr loc
  and transl e = { ge_desc = translate e.e_loc e.e_desc; ge_typ = e.e_typ } in
  transl e

and find_bound_input loc id acts = 
  let rec find = function 
     [] -> raise Not_found
   | ABindInp (i,p,t) :: _ when id=i-> p,t
   | _ :: rest -> find rest in
  try find acts
  with Not_found -> illegal_guard_expr loc

and find_bound_var loc id acts = 
  let rec find = function 
     [] -> raise Not_found
   | ABindVar (VSimple v' as v, p, t) :: _ when id=v' -> v,p,t
   | ABindVar (VArrLoc (v',k) as v, p, t) :: _ when id=v' -> v,p,t
   | _ :: rest -> find rest in
  try find acts
  with Not_found -> illegal_guard_expr loc

let mk_wire sp (id,(((((s,ss),(d,ds)) as locs),ty) )) = 
  id, (locs, ty)

(* Misc *)

let lookup_type id env =
  try List.assoc id env
  with Not_found -> failwith "Interm.lookup_type" (* should not happen *)

let extract_extfns fns (id,v) = match v with
| Expr.Val_extern fd -> (id,fd) :: fns
| _ -> fns

(* Network transformations *)

(* Insert SPLITters 
 * 
 *        +--------+         +--------+          +--------+         +---------+          +--------+
 *        |        |         |        |          |        |         |         |    w1'   |        |
 *        |        |    w1   |        |          |        |    w'   |        0|--------->|        |
 *        |   A1 k1|-------->|k2 A2   |    ===>  |   A1 k1|-------->|0 Split  |          |k2 A2   |
 *        |        |\        |        |          |        |         |        1|----+     |        |
 *        |        | |       |        |          |        |         |         |    |     |        |
 *        +--------+ |       +--------+          +--------+         +---------+    |     +--------+
 *                   |                                                             |
 *                   |       +--------+                                            |     +--------+
 *                   |       |        |                                        w2' |     |        |
 *                   |  w2   |        |                                            |     |        |
 *                   +------>|k3 A3   |                                            +---->|k3 A3   |
 *                           |        |                                                  |        |
 *                           |        |                                                  |        |
 *                           +--------+                                                  +--------+
*)

let new_splitter_box ty wid wids =
  let bid = Static.new_bid () in
  let bos = Misc.list_map_index (fun i wid -> "o_" ^ string_of_int (i+1), ([wid],ty)) wids in 
  bid, { ib_tag=RegularB; ib_name="_split"; ib_wsub=""; ib_ins=["i",(wid,ty)]; ib_outs=bos; ib_typ=ty; ib_tysig=ty; ib_ninsts=0;
         ib_types=[]; ib_tvbs=[]; ib_svbs=[]; ib_fpbs=[]; ib_params=[]; ib_vars=[]; ib_transitions=[]; ib_device=""; ib_impl=no_impl;
         ib_ival=None; ib_moc=Moc_SDF ([1], List.map (function w -> 1) wids) }

let rec insert_splitter bid oidx (boxes,wires,box) bout = 
  match bout with
  | (id, ([],ty)) -> boxes, wires, box       (* should not happen ? *)
  | (id, ([wid],ty)) -> boxes, wires, box    (* no need to insert here *)
  | (id, (wids,ty)) ->                       (* the relevant case : a box output connected to several wires *)
      let wid' = Static.new_wid() in
      let m, mb = new_splitter_box ty wid' wids in
      let box' = { box with ib_outs = Misc.assoc_replace id (function _ -> [wid'],ty) box.ib_outs } in
      let wires' = Misc.foldl_index (update_wires m) wires wids in
      let boxes' = Misc.assoc_replace bid (function b -> box') boxes in
      (m,mb) :: boxes', (wid',(((bid,oidx),(m,0)),ty)) :: wires', box'

and update_wires s' j wires wid = 
  Misc.assoc_replace wid (function ((s,ss),(d,ds)),ty -> ((s',j),(d,ds)),ty) wires 

let insert_splitter_after (boxes,wires) (bid,box) = 
  match box.ib_tag with
    Static.InpB Syntax.PortIO ->
      (* Do not insert splitters after input ports *)
      boxes, wires
  | _ ->
      let boxes', wires', box' = Misc.foldl_index (insert_splitter bid) (boxes,wires,box) box.ib_outs in
      boxes', wires'


(* Insert FIFOs and BUFFERs
 * 
 *        +-------+         +-------+          +-------+         +--------+          +--------+
 *        |       |    w    |       |          |       |    w'   |        |    w''   |        |
 *        |  A1  k|-------->|k' A2  |    ===>  |  A1  k|-------->|0 FIFO 0|--------->|k' A2   |
 *        |       |         |       |          |       |         |        |          |        |
 *        +-------+         +-------+          +-------+         +--------+          +--------+
 *
 *        +-------+         +-------+          +-------+         +--------+          +-------+
 *        |       |    w    |       |          |       |     w'  |        |    w''   |       |
 *        | PORT 0|-------->|k  A   |    ===>  | PORT 0|-------->|0 BUFF 0|--------->|k  A   |
 *        |       |         |       |          |       |         |        |          |       |
 *        +-------+         +-------+          +-------+         +--------+          +-------+
*
 *        +-------+         +-------+          +-------+         +--------+          +-------+
 *        |       |    w    |       |          |       |     w'  |        |    w''   |       |
 *        |   A  k|-------->|0 PORT |    ===>  |   A  k|-------->|0 BUFF 0|--------->|0 PORT |
 *        |       |         |       |          |       |         |        |          |       |
 *        +-------+         +-------+          +-------+         +--------+          +-------+
 *
 *
*)

let rec insert_fifo_or_buffer (boxes,wires') (wid,wire) =
  match wire with
  | ((s,ss),(d,ds)), ty when is_input_port boxes s || is_output_port boxes d ->
      let wid' = Static.new_wid() in
      let wid'' = Static.new_wid() in
      let f, fb = new_buffer_box ty wid wid' wid'' in
      let w' = ((s,ss),(f,0)), ty in
      let w'' = ((f,0),(d,ds)), ty in
      let boxes' = update_bouts wid wid' s boxes in
      let boxes'' = update_bins wid wid'' d boxes' in
      (f,fb) :: boxes'', (wid',w') :: (wid'',w'') :: wires'
  | ((s,ss),(d,ds)), ty when is_split_box boxes d  ->
      boxes, (wid,wire) :: wires'
        (* Do not insert anything between a box output and a splitter.
           This is useless since buffering will be done in the FIFOs _after_ the splitter.
           Moreover, this will require SPLITters to be true actors (and not just combinatorial operators). *)
  | ((s,ss),(d,ds)), ty ->                   (* Insert FIFO *)
      let wid' = Static.new_wid() in
      let wid'' = Static.new_wid() in
      let f, fb = new_fifo_box ty wid wid' wid'' in
      let w' = ((s,ss),(f,0)), ty in
      let w'' = ((f,0),(d,ds)), ty in
      let boxes' = update_bouts wid wid' s boxes in
      let boxes'' = update_bins wid wid'' d boxes' in
      (f,fb) :: boxes'', (wid',w') :: (wid'',w'') :: wires'

and is_split_box boxes bid = (find_box boxes bid).ib_name = "_split"

and is_input_port boxes bid = match (find_box boxes bid).ib_tag with Static.InpB PortIO -> true | _ -> false

and is_output_port boxes bid = match (find_box boxes bid).ib_tag with Static.OutB PortIO -> true | _ -> false

and new_fifo_box ty w wid' wid'' = new_channel_box "fifo" ty w wid' wid''

and new_buffer_box ty w wid' wid'' = new_channel_box "buffer" ty w wid' wid''

and new_channel_box kind ty w wid' wid'' =  (* FIFO or Port boxes *)
  let bid = Static.new_bid () in
  bid, { ib_tag=RegularB; ib_name=kind; ib_wsub="w" ^ string_of_int w; ib_device=""; ib_tvbs=[]; ib_svbs=[]; ib_fpbs=[];
         ib_typ=ty; ib_tysig=ty; ib_ins=["i",(wid',ty)]; ib_outs=["o",([wid''],ty)]; ib_ninsts=0;
         ib_types=[]; ib_params=[]; ib_vars=[]; ib_impl=no_impl; ib_transitions=[]; ib_ival=None; ib_moc=Moc_Unknown }

and update_bouts wid wid' s boxes = 
  let replace_wire b_outs =
    List.map (function
        id,([w],ty) when w = wid -> id,([wid'],ty)   (* TODO : handle case when an output is bound to several wires ? *)
                                                     (* Maybe not necessary if splitters have been inserted *)
      | o -> o) b_outs  in
  Misc.assoc_replace s (function b -> { b with ib_outs = replace_wire b.ib_outs }) boxes 

and update_bins wid wid' s boxes = 
  let replace_wire b_ins =
    List.map (function
        id,(w,ty) when w = wid -> id,(wid',ty)
      | i -> i) b_ins  in
  Misc.assoc_replace s (function b -> { b with ib_ins = replace_wire b.ib_ins }) boxes 

let is_fifo_wire boxes (wid,wire) =
  match wire with
  | ((s,ss),(d,ds)), ty when is_input_port boxes s || is_output_port boxes d -> false
  | _, _ -> true

(* Static program => Interm repr *)

let mk_interm tp sp =
  let boxes = List.map (mk_box tp sp) sp.boxes in
  { ir_actors = List.map (mk_actor tp boxes) sp.gacts;
    ir_consts = sp.gcsts;
    ir_globfns = sp.gfuns;
    ir_globtys = sp.gtyps;
    ir_extfns = List.fold_left extract_extfns [] sp.e_vals;
    ir_boxes = boxes;
    ir_wires = List.map (mk_wire sp) sp.wires;
    ir_wire_annots = [] }

(* Transformations *)

let insert_splitters ir = 
  let boxes', wires' = List.fold_left insert_splitter_after (ir.ir_boxes, ir.ir_wires) ir.ir_boxes in
  { ir with ir_boxes = boxes'; ir_wires = wires' }

let insert_fifos_and_buffers ir =
  let boxes', wires' = List.fold_left insert_fifo_or_buffer (ir.ir_boxes, []) ir.ir_wires in
  { ir with ir_boxes = boxes'; ir_wires = wires' }

(* Extractors *)

let extract_split_boxes fanouts (bid,box) =
  match box.ib_tag with
  | RegularB when box.ib_name = "_split" ->
      let fanout = List.length box.ib_outs in
      if List.mem fanout fanouts then fanouts else fanout::fanouts
  | _ -> fanouts

(* Printing *)

let string_of_type_var v = "'" ^ Pr_type.TypeVarNames.name_of v
let string_of_size_var v = "#" ^ Pr_type.SizeVarNames.name_of v

let rec dump_actor (id,a) = 
  Pr_type.TypeVarNames.reset ();
  Pr_type.SizeVarNames.reset ();
  Printf.printf "Actor %s : tvars=[%s] svars=[%s] params=[%s] ins=[%s] outs=[%s] vars=[%s]\n"
    a.ia_name
    (Misc.string_of_list string_of_type_var "," a.ia_tvars)
    (Misc.string_of_list string_of_size_var "," a.ia_svars)
    (Misc.string_of_list string_of_typed_param ","  a.ia_params)
    (Misc.string_of_list string_of_typed_in ","  a.ia_ins)
    (Misc.string_of_list string_of_typed_out ","  a.ia_outs)
    (Misc.string_of_list string_of_typed_var ","  a.ia_vars);
  List.iter (function t -> Printf.printf "        # %s\n" (string_of_transition t)) a.ia_transitions;
  if a.ia_impl.ai_systemc <> "" then Printf.printf "  * using %s for SystemC implementation\n" a.ia_impl.ai_systemc;
  if a.ia_impl.ai_vhdl <> "" then Printf.printf "  * using %s for VHDL implementation\n" a.ia_impl.ai_vhdl;
  ActInsts.iter Static.print_actor_inst a.ia_insts

and string_of_typed_in (id,ty) = id ^ ":" ^ Pr_type.string_of_type ty
and string_of_typed_out (id,ty) = id ^ ":" ^ Pr_type.string_of_type ty 
and string_of_typed_param (id,ty) = id ^ ":" ^ Pr_type.string_of_type ty
and string_of_typed_param' (id,(v,ty)) = id ^ ":" ^ Pr_type.string_of_type ty ^ "=" ^ Expr.string_of_val v
and string_of_typed_var (id,(iv,ty,_,_)) = id ^ ":" ^ Pr_type.string_of_type ty ^ string_of_opt_expr iv
and string_of_opt_expr = function None -> "" | Some e -> "=" ^ Syntax.string_of_expr e

and string_of_transition (conds,acts) = 
  string_of_conds conds ^ " / " ^ string_of_actions acts 

and string_of_frag_vars fvars = Misc.string_of_list (function (v,_) -> v) "," fvars

and string_of_conds acs = Misc.string_of_list string_of_cond ", " acs

and string_of_cond = function 
    CInpRdy (i,ty) -> i ^ ".rdy"
  | COutRdy (o,ty) -> o ^ ".rdy"
  | CInpMatch (i, p, ty) -> i ^ "=" ^ Syntax.string_of_rpat_desc p
  | CVarMatch (v, p, ty) -> string_of_local_var v ^ "=" ^ Syntax.string_of_rpat_desc p
  | CGuardExp e -> string_of_guard_expr e

and string_of_actions acs = Misc.string_of_list string_of_action ", " acs

and string_of_action act = match act with
  | AReadInp (i,_) -> "rd(" ^  i ^ ")"
  | ABindInp (i,RPatVar v,_) -> v ^ "=" ^ i
  | ABindInp (i,RPatCon(cid, [{rp_desc=RPatVar v}]),_) -> v ^ "=" ^ i ^ "." ^ String.uncapitalize cid
  | ABindInp (i,RPatCon(cid, ps),_) when List.for_all is_simple_rule_pattern ps ->
      Misc.string_of_list (string_of_binding i cid) ", " ps 
  | ABindVar (v,RPatVar v',_) -> v' ^ "=" ^ string_of_local_var v
  | ABindVar (v,RPatCon(cid, [{rp_desc=RPatVar v'}]),_) -> v' ^ "=" ^ string_of_local_var v  ^ "." ^ String.uncapitalize cid (* TOFIX ? *)
  | ABindVar (VSimple v,RPatCon(cid, ps),_) when List.for_all is_simple_rule_pattern ps -> 
      Misc.string_of_list (string_of_binding v cid) ", " ps 
  | AWriteVar (v,e,_) -> string_of_local_var v ^ ":=" ^ Syntax.string_of_exp e
  | AWriteOut (o,e,_) -> "wr(" ^ o ^ "," ^ Syntax.string_of_exp e ^ ")"
  | _ ->
     failwith "Interm.string_of_action: cannot translate rule action"

and string_of_binding b cid p = match p.rp_desc with
  RPatConst _ -> ""
| RPatWild -> ""
| RPatVar v -> v ^ "=" ^ b ^ "." ^ String.uncapitalize cid
| _ -> failwith "Interm.string_of_binding" (* should not happen *)

let rec dump_box (id,b) = 
  Pr_type.TypeVarNames.reset ();
  Pr_type.SizeVarNames.reset ();
  Printf.printf "Box B%d(%s) : tvars=[%s] svars=[%s] fparams=[%s] params=[%s] ins=[%s] outs=[%s] vars=[%s]%s\n"
    id
    b.ib_name
    (Misc.string_of_list Static.string_of_typ_var_inst "," b.ib_tvbs)
    (Misc.string_of_list Static.string_of_siz_var_inst "," b.ib_svbs)
    (Misc.string_of_list Static.string_of_fparam_inst "," b.ib_fpbs)
    (Misc.string_of_list string_of_typed_param' ","  b.ib_params)
    (Misc.string_of_list Static.string_of_typed_bin ","  b.ib_ins)
    (Misc.string_of_list Static.string_of_typed_bout ","  b.ib_outs)
    (Misc.string_of_list string_of_typed_bvar ","  b.ib_vars)
    (Misc.string_of_opt (function v -> " ival=" ^ Expr.string_of_val v) b.ib_ival);
  List.iter (function t -> Printf.printf "        # %s\n" (string_of_transition t)) b.ib_transitions

and string_of_typed_bvar (id,(iv,ty,_,_)) = id ^ ":" ^ Pr_type.string_of_type ty ^ string_of_opt_value iv
and string_of_opt_value = function None -> "" | Some e -> "=" ^ "<iv>" (* Expr.string_of_val e *)
  
let print_wire (i,(((s,ss),(d,ds)),ty(*,k*))) =
  Printf.printf "W%d: %s: (B%d,%d) -> (B%d,%d)\n" i (Pr_type.string_of_type ty) s ss d ds (* (string_of_wire_kind k)*)

let dump_interm_repr ir = 
  printf "Intermediate representation ---------------\n";
  printf "- Global constants ------------------------\n";
  List.iter Static.print_global_const ir.ir_consts;
  printf "- Global functions ------------------------\n";
  List.iter Static.print_global_fn ir.ir_globfns;
  printf "- Global types ----------------------------\n";
  List.iter Typing.print_type ir.ir_globtys;
  printf "- Actors ----------------------------------\n";
  List.iter dump_actor ir.ir_actors;
  printf "- Boxes  ----------------------------------\n";
  List.iter dump_box ir.ir_boxes;
  printf "- Wires  ----------------------------------\n";
  List.iter print_wire ir.ir_wires
