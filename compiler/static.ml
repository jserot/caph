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

open Syntax
open Error
open Misc
open Types
open Typing
open Printf
open Pr_type
open Ssval
open Location
open Arrays

let max_label_width = ref 16

let compute_moc = ref false

type box_tag = 
    RegularB
  | InpB of io_kind
  | OutB of io_kind
  | DummyB  (* Temporary boxes used for handling recursive defns *)

and ss_box = {
    b_tag: box_tag;
    b_id: int;
    b_name: string;                           (* For regular boxes, name of the instanciated actor *)
    b_wsub: string;                           (* For FIFO boxes, name of the substituted wire *)
    b_typ: typ;                               (* "Functional" type, i.e. either [t_params -> t_ins -> t_outs] or [t_ins -> t_outs] *)
    b_tysig: typ;                             (* "Signature" type, i.e. [t_params * t_ins * t_vars * t_outs] *)
    b_tvbs: typ var_bind list;                (* Type var instantiations (when the box derives from a polymorphic actor) *)
    b_svbs: siz var_bind list;                (* Size var instantiations (when the box derives from a polymorphic actor) *)
    b_fpbs: (string * string)  list;          (* Function parameter instantiations *)
    b_types: (string * local_type_def) list;  (* locally defined types (enums) *)
    b_ins: (string * (wid * typ)) list;
    b_outs: (string * (wid list * typ)) list;
    b_params: (string * (Expr.e_val * typ)) list;      (* Parameters, with their actual values *)
    b_vars: (string * (Expr.e_val option * typ)) list; (* Local variables, with their initial values *)
    b_rules: b_rule list;                              (* The (instanciated, specialized) rule set *)
    b_device: string;                                  (* For I/O boxes, name of the file, port ... *)
    b_impl: Syntax.actor_impl;
    mutable b_moc: b_moc;
    b_ival: Expr.e_val option                          (* For port inputs, initial value *)
}

and wid = int
and bid = int

and b_rule = sa_rule * typ * sa_rsig

and b_moc =
  | Moc_SDF of sa_rsig
  | Moc_CSDF of sa_rsig list
  | Moc_DDF
  | Moc_Unknown

type ss_wire = (sv_loc * sv_loc) * typ 

(* The result of the static analysis *)

module ActInsts =   (* Keeping track of actor instanciations *)
  Itbl.Make
    (struct
      type t = typ * string list            (* Actual type, actual name of function parameters *)
      let equal (t1,fs1) (t2,fs2) = type_equal t1 t2 && fs1 = fs2
    end)

type static_program = { 
    e_vals: (string * Expr.e_val) list;
    n_vals: (string * ss_val) list;
    gacts: (string * ga_desc) list;
    boxes: (bid * ss_box) list;
    wires: (wid * ss_wire) list;
    gfuns: (string * Syntax.gf_desc) list;      (* Global fns *)
    gcsts: (string * gc_desc) list;             (* Global constants *)
    gtyps: (string * Typing.tc_desc) list;      (* Globally defined types *)
  }


and ga_desc = {                                                     (* Actors *)
    ac_defn: Ssval.sv_act;                                          (* Definition *)
    mutable ac_insts: (ss_box, bid) ActInsts.t;                     (* Instances *)
  }

and gc_desc = {                                                     (* Constants (monomorphic) *)
    gc_val: Expr.e_val;
    gc_typ: Types.typ;
    gc_annot: Syntax.const_annot option
  }

(* A (mutable) global descriptor will keep track of each distinct instance for actors,
  global functions and constants *)

type global_desc = {
  mutable gl_actors: (string * ga_desc) list;
  mutable gl_funs: (string * Syntax.gf_desc) list;
  mutable gl_csts: (string * gc_desc) list;
}

let rec mk_globals tp vals actors val_decls = 
  { gl_actors = List.fold_left extract_global_actors [] actors; 
    gl_funs = List.fold_left (extract_global_fns tp) [] val_decls;
    gl_csts = List.fold_left (extract_global_csts tp vals) [] val_decls; }

and extract_global_actors acc (id,v) = match v with
| SVAct sa -> 
    (id, {ac_defn=sa; ac_insts=ActInsts.empty ()}) :: acc
| _ -> acc

and extract_global_fns tp acc v = match v.val_desc with
| VFun (fid,args,exp,_) -> 
    let ty = List.assoc fid tp.tp_vals in
    (fid, {gf_def={gf_args=args; gf_body=exp; gf_typ=ty}; gf_insts=TypeInsts.empty ()}) :: acc
| _ -> acc

and extract_global_csts tp vals acc v = match v.val_desc with
| VConst (id,exp,annot) -> 
    let ty = List.assoc id tp.tp_vals in
    let v = List.assoc id vals in
    (id, {gc_val=v; gc_typ=type_instance ty; gc_annot=annot}) :: acc   (* constants are monomorphic *)
| _ -> acc

let add_actor_instance globals id (ty,fns) box bid =
  try
    let ad = List.assoc id globals.gl_actors in
    ActInsts.add (ty,fns) (box,bid) ad.ac_insts
  with
    Not_found -> failwith "Static.add_actor_instance" (* should not happen *)
      
(* Type specialisation and derefencing *)

let rec specialize_exp tp globals tvbs svbs fpbs e =
  { e_desc = specialize_exp_desc e.e_loc tp globals tvbs svbs fpbs e.e_desc;
    e_typ = copy_type tvbs svbs e.e_typ;
    e_loc = e.e_loc } 

and specialize_exp_desc loc tp globals tvbs svbs fpbs e =
  let specialize = specialize_exp tp globals tvbs svbs fpbs in
  match e with
  | ECon (cid, []) -> ECon (cid, [])
  | ECon (cid, es) -> 
      let es' = List.map specialize es in
      ECon (cid, es')
  | ECond (e1,e2,e3) -> ECond (specialize e1, specialize e2, specialize e3)
  | ELet (bs,e2) -> ELet (List.map (function (v,e) -> v, specialize e) bs, specialize e2)
  | EApp ({e_desc=EVar fid} as fn, _, es) ->
      let fn' = if List.mem_assoc fid fpbs then { fn with e_desc = EVar (List.assoc fid fpbs) } else fn in
      let es' = List.map specialize es in
      if List.mem_assoc fid globals.gl_funs then begin
        let f = List.assoc fid globals.gl_funs in
        let ty_arg =
          begin match es' with
          [e] -> e.e_typ
        | es -> Types.type_product (List.map (function e -> e.e_typ) es)
          end in
        let ty_fn, ty_res, tvbs', svbs' = type_application (EItem fid) f.gf_def.gf_typ ty_arg in
        let body = specialize_exp tp globals tvbs' svbs' fpbs f.gf_def.gf_body in
        let _ = TypeInsts.add ty_fn (body,loc) f.gf_insts in
        EApp (fn', Some (f,ty_arg), es')
          (* Note 2015-06-12, JS.
             For global functions, we annote the application with the global function descriptor and the
             actual argument type. These informations will be used by the backend code generators *)
          (* Note 2016-05-25, JS.
             When applying a function listed in the box parameters, we also substitute the name of
             the actual (global) function to the name of the parameter *)
      end
      else
        EApp (fn', None, es')
  | EArrRead (a,idxs) -> EArrRead (a, List.map specialize idxs)
  | EArrayExt1 es -> EArrayExt1 (List.map specialize es)
  | EArrayCompr ([i,e1,e2],e3) -> EArrayCompr ([i,specialize e1,specialize e2], specialize e3)
  | EArrayCompr (ixrs,e3) ->
      EArrayCompr (List.map (function (i,e1,e2) -> i,specialize e1, specialize e2) ixrs, specialize e3)
  | ETuple es -> ETuple (List.map specialize es)
  | ECast (e,te) -> ECast (specialize e, specialize_type_exp tvbs svbs te)
  | e -> e

and specialize_type_exp tvbs svbs te = { te with te_typ = copy_type tvbs svbs te.te_typ }

(* Dereferencing *)

let rec deref_rpat renv p =
  { rp_desc = p.rp_desc;
    rp_typ = Types.deref renv p.rp_typ;
    rp_loc = p.rp_loc } 

and deref_exp renv e =
  { e_desc = deref_exp_desc renv e.e_desc;
    e_typ = Types.deref renv e.e_typ;
    e_loc = e.e_loc } 

and deref_exp_desc renv e =
  let deref = deref_exp renv in
  let deref_index = deref_exp renv in
  match e with
  | ECon (cid, es) -> 
      let es' = List.map deref es in
      ECon (cid, es')
  | ECond (e1,e2,e3) -> ECond (deref e1, deref e2, deref e3)
  | ELet (bs,e2) -> ELet (List.map (function (v,e) -> v, deref e) bs, deref e2)
  | EApp (fn, vbs, args) -> EApp (deref fn, vbs, List.map deref args)
  | EArrRead (a,idxs) -> EArrRead (a, List.map deref_index idxs)
  | EArrayExt1 es -> EArrayExt1 (List.map deref es)
  | EArrayCompr (ixrs,e3) -> EArrayCompr (List.map (function (i,e1,e2) -> i, deref e1, deref e2) ixrs, deref e3)
  | ETuple es -> ETuple (List.map deref es)
  | ECast (e,te) -> ECast (deref e, deref_type_exp renv te)
  | e -> e

and deref_type_exp renv te = { te with te_typ = Types.deref renv te.te_typ }

let rec deref_rule renv (lhs,gds,rhs) = 
  (List.map (function (q,p) -> q, deref_rpat renv p) lhs),
  (List.map (deref_exp renv) gds),
  (List.map (function (q,e) -> q, deref_exp renv e) rhs)

let rec specialize_rule tp globals tvbs svbs fpbs (lhs,gds,rhs) = 
  List.map (function (q,p) -> q, specialize_rpat tp globals tvbs svbs p) lhs,
  List.map (specialize_exp tp globals tvbs svbs fpbs) gds,
  List.map (function (q,e) -> q, specialize_exp tp globals tvbs svbs fpbs e) rhs

and specialize_rpat tp globals tvbs svbs p =
  { rp_desc=specialize_rpat_desc tp globals tvbs svbs p.rp_desc;
    rp_typ=copy_type tvbs svbs p.rp_typ;
    rp_loc=p.rp_loc } 

and specialize_rpat_desc tp globals tvbs svbs p =
  let specialize = specialize_rpat tp globals tvbs svbs in
  match p with
  | RPatCon (c,ps) -> RPatCon (c, List.map specialize ps)
  | p -> p

(* Box creation *)

let new_bid = 
  let cnt = ref 0 in
  function () -> incr cnt; !cnt

let new_wid = 
  let cnt = ref 0 in
  function () -> incr cnt; !cnt

let new_rbox name ty typarams tyins tyvars tyouts tvbs svbs fpbs ins outs types params vars rules impl =
  let bid = new_bid () in
  bid, { b_id=bid; b_tag=RegularB; b_name=name; b_wsub=""; b_ins=ins; b_outs=outs; b_types=types; b_typ=ty;
         b_tysig=type_product [typarams;tyins;tyvars;tyouts];
         b_params=params; b_tvbs=tvbs; b_svbs=svbs; b_fpbs=fpbs; b_vars= vars; b_rules=rules; b_device="";
         b_impl=impl; b_moc=Moc_Unknown; b_ival=None }

let new_ibox kind name ty dev ival =
  let bid = new_bid () in
  let dev' = match dev with
      "" -> ""
     | _ -> Genmake.add_input_file (dev,ty) in
  if !Genmake.safe_mode && not (List.mem dev' Genmake.safe_cfg.Genmake.allowed_ifiles) then unsafe_run dev';
  bid, { b_id=bid; b_tag=InpB kind; b_name=name; b_wsub=""; b_ins=[]; b_outs=["o",([0],ty)]; b_typ=ty; 
         b_tysig=ty;
         b_types=[]; b_params=[]; b_tvbs=[]; b_svbs=[]; b_fpbs=[];  b_vars=[]; b_rules=[]; b_device=dev';
         b_impl=no_impl; b_moc=Moc_Unknown; b_ival=ival }

let new_obox kind name ty dev ival =
  let bid = new_bid () in
  let dev' = Genmake.add_output_file (dev,ty) in
  if !Genmake.safe_mode && not (List.mem dev' Genmake.safe_cfg.Genmake.allowed_ofiles) then unsafe_run dev';
  bid, { b_id=bid; b_tag=OutB kind; b_name=name; b_wsub=""; b_ins=["i",(0,ty)]; b_outs=[]; b_typ=ty; 
         b_tysig=ty;
         b_types=[]; b_params=[]; b_tvbs=[]; b_svbs=[]; b_fpbs=[];  b_vars=[]; b_rules=[]; b_device=dev';
         b_impl=no_impl; b_moc=Moc_Unknown; b_ival=ival }

let new_dummy_box name ty =
  let bid = new_bid () in
  bid, { b_id=bid; b_tag=DummyB; b_name=name; b_wsub=""; b_ins=[]; b_outs=["r",([0],ty)]; b_typ=ty; 
         b_tysig=ty;
         b_types=[]; b_params=[]; b_tvbs=[]; b_svbs=[]; b_fpbs=[];  b_vars=[]; b_rules=[]; b_device="";
         b_impl=no_impl; b_moc=Moc_Unknown; b_ival=None }

(* let type_of_obox b = *)
(* match b with *)
(*   { b_tag=OutB _; b_ins=["i",(_,ty)] } -> ty *)
(* | _ -> fatal_error "Static.type_of_obox" *)

(* let type_of_ibox b = match b with *)
(*   { b_tag=InpB _; b_outs=["o",(_,ty)] } -> ty *)
(* | _ -> fatal_error "Static.type_of_ibox" *)

let boxes_of_wire boxes (((s,ss),(d,ds)),ty) = 
  try
    List.assoc s boxes, List.assoc d boxes
  with Not_found -> 
    fatal_error "Static.boxes_of_wire"  (* should not happen *)

let src_box_of_wire boxes (w:ss_wire) = fst (boxes_of_wire boxes w)
let dst_box_of_wire boxes (w:ss_wire) = snd (boxes_of_wire boxes w)

(*** NETWORK LEVEL ***)

(* Rules NE [,B] |-n NPat, rho => NE', W *)

exception Matching_fail

let is_output nenv id =
  try match List.assoc id nenv with
      SVLoc(_,_,_,true) as loc -> Some loc
    | _ -> None
    with Not_found -> None

let rec net_matching toplevel nenv npat r = match npat.np_desc, r with
  | NPat_var id, r' ->
      begin match toplevel, is_output nenv id, r' with
      | false, _, _ -> [id, r], []
      | true, None, _ -> [id, r], []
      | true, Some (SVLoc(l',_,ty',_)), SVLoc(l,s,ty,_) -> [], [new_wid(),(((l,s),(l',0)),ty')]
      | true, _, _ -> fatal_error "matching: cannot bind output" end
  | NPat_tuple ps, SVTuple rs when List.length ps = List.length rs ->
      let nenvs, ws = List.split (List.map2 (net_matching toplevel nenv) ps rs) in
      List.concat nenvs, List.concat ws
  | NPat_unit, _ -> [], []
  | _, _ -> raise Matching_fail

(* Rule: NE |- NExp => rho,B,W *)

let rec eval_net_expr tp globals nenv expr =
  match expr.ne_desc with
  | NVar v ->
      if List.mem_assoc v nenv then List.assoc v nenv, [], []
      else unbound_value_err v expr.ne_loc
  | NConst c -> SVVal (eval_const expr.ne_loc c), [], []
  | NUnit -> SVUnit, [], []
  | NArray1Const vs -> 
      let a = Array1.of_list (List.map (Expr.eval_const expr.ne_loc) vs) in
      SVVal (Expr.Val_array1 (Array1.size a, a)), [], []
  | NArray2Const vss -> 
      let a = Array2.of_list (List.map (List.map (Expr.eval_const expr.ne_loc)) vss) in
      SVVal (Expr.Val_array2 (Array2.size a, a)), [], []
  | NArray3Const vsss -> 
      let a = Array3.of_list (List.map (List.map (List.map (Expr.eval_const expr.ne_loc))) vsss) in 
      SVVal (Expr.Val_array3 (Array3.size a, a)), [], []
  | NArrayItem (id,idxs) ->
      let a = expr_of_sv expr.ne_loc (lookup "Static.eval_net_expr" id nenv) in
      let ixs = List.map (function idx -> let v, _, _ = eval_net_expr tp globals nenv idx in expr_of_sv expr.ne_loc v) idxs in
      SVVal (Expr.array_get expr.ne_loc a ixs), [], []
  | NCast (e,te) ->
      eval_net_expr tp globals nenv e   (* Type refinement has no impact at the static level *)
  | NTuple es ->
      let rs, bs, ws = List.fold_right
          (fun e (rs,bs,ws) -> let r',bs',ws' = eval_net_expr tp globals nenv e in (r'::rs,bs'@bs,ws'@ws))
          es
          ([],[],[]) in
      SVTuple rs, bs, ws
  | NApp (fn, arg) ->
      let val_fn, bs_f, ws_f = eval_net_expr tp globals nenv fn in
      let val_arg, bs_a, ws_a = eval_net_expr tp globals nenv arg in
      begin match val_fn with
      | SVVal (Expr.Val_hprim (p,n)) ->
          SVHoPrim (p, n, [val_arg]), bs_a@bs_f, ws_a@ws_f
      | SVHoPrim (p, n, pre_args) -> 
          if List.length pre_args = n then 
            let r, bs', ws' = 
              eval_ho_prim_application tp globals (bs_a,ws_a) nenv expr.ne_loc p pre_args val_arg (real_type arg.ne_typ) in
            r, bs'@bs_a, ws'@ ws_a
          else
          SVHoPrim (p, n, pre_args @ [val_arg]), bs_a@bs_f, ws_a@ws_f
      | _ -> 
          let r, bs'', ws'' =
            eval_net_application tp globals (bs_a@bs_f,ws_a@ws_f) nenv expr.ne_loc val_fn val_arg (real_type arg.ne_typ) in
          r, bs'' @ bs_a @ bs_f, ws'' @ ws_a @ ws_f
      end
  | NFun (npat,nexp) ->
      SVClos {cl_pat=npat; cl_exp=nexp; cl_env=nenv}, [], []
  | NLet (isrec, defns, body) ->
      let nenv',boxes,wires = eval_net_defns expr.ne_loc false isrec tp globals nenv defns in
      let r',boxes',wires' = eval_net_expr tp globals (nenv' @ nenv) body in
      r', boxes' @ boxes, wires' @ wires

and eval_net_application tp globals (bs,ws) nenv loc val_fn val_arg ty_arg = 
  match val_fn with
  | SVClos {cl_pat=npat; cl_exp=nexp; cl_env=nenv'} ->   (* RULE NAPP CLO *)
      let nenv'', _ =
        begin try net_matching false [] npat val_arg
        with Matching_fail -> binding_error npat.np_loc
        end in
      eval_net_expr tp globals (nenv'' @ nenv') nexp
  | SVAct a ->
      if List.length a.sa_params > 0                     (* RULE NAPP ACT 2 *)
      && List.for_all (function _,_,None -> true | _,_,Some _ -> false) a.sa_params then
        let actual_params =
          begin
            match val_arg, ty_arg with
            | SVVal v, ty ->
                [v,ty]
            | SVTuple vs, Tproduct ts ->
                begin try
                  List.map2 (fun v t -> match v with SVVal v' -> v',t | _ -> raise (Invalid_argument "")) vs ts
                with
                  Invalid_argument _ -> failwith "Static.eval_net_application" (* should not happen *)
                end
            | _, _ ->
                illegal_parameters a.sa_id loc "must be int(s) or bool(s)"
          end in
        instanciate_actor_desc a actual_params, [], []
      else                                               (* Rule NAPP ACT 1 *)
        instanciate_actor tp globals nenv loc a val_arg
  | _ ->
          illegal_application loc

and eval_ho_prim_application tp globals (bs,ws) nenv loc p pre_args arg tyarg = match p with
      | "map" -> 
          begin match pre_args, arg, tyarg with
            [f], SVTuple vs, Tproduct (ty'::_)
          | [f], SVTuple vs, Tconstr({tc_name="bundle"},[ty'], [_]) -> 
              let rs, bs', ws' =
                List.fold_left
                  (fun (rs,bs,ws) v ->
                    let r', bs', ws' =
                      eval_net_application tp globals (bs,ws) nenv loc f v ty' in
                    rs@[r'], bs@bs', ws@ws')
                  ([],[],[])
                  vs in
              SVTuple rs, bs', ws'
          | _, _, _ -> fatal_error "Static.eval_ho_prim(map)" (* should not happen, thx to TC *) end
      | "mapi"->
          begin match pre_args, arg, tyarg with
            [f], SVTuple vs, Tproduct (ty'::_)
          | [f], SVTuple vs, Tconstr({tc_name="bundle"},[ty'],[_]) ->
              let rs, bs', ws' =
                Misc.foldl_index
                  (fun i (rs,bs,ws) v ->
                    let v_i = SVVal (Expr.Val_int (i, None)) in
                    let ty_i = type_unsigned (new_size_var ()) in
                    let r', bs', ws' =
                      let f', bs'', ws'' =
                        eval_net_application tp globals (bs,ws) nenv loc f v_i ty_i in
                      eval_net_application tp globals (bs@bs'',ws@ws'') nenv loc f' v ty' in
                    rs@[r'], bs@bs', ws@ws')
                  ([],[],[])
                  vs in
              SVTuple rs, bs', ws'
          | _, _, _ -> fatal_error "Static.eval_ho_prim(mapi)" (* should not happen, thx to TC *) end
      | "map2" ->
          begin match pre_args, arg, tyarg with
            [f], SVTuple [SVTuple vs1; SVTuple vs2], Tproduct [Tproduct (ty1'::_); Tproduct (ty2'::_)]
          | [f], SVTuple [SVTuple vs1; SVTuple vs2], Tproduct [Tproduct (ty1'::_); Tconstr({tc_name="bundle"},[ty2'],[_])]
          | [f], SVTuple [SVTuple vs1; SVTuple vs2], Tproduct [Tconstr({tc_name="bundle"},[ty1'],[_]); Tproduct (ty2'::_)]
          | [f], SVTuple [SVTuple vs1; SVTuple vs2], Tproduct [Tconstr({tc_name="bundle"},[ty1'],[_]); Tconstr({tc_name="bundle"},[ty2'],[_]) ] ->
              let rs, bs', ws' =
                List.fold_left2
                  (fun (rs,bs,ws) v1 v2 ->
                    let r', bs', ws' =
                      let v = SVTuple [v1;v2]
                      and ty' = Tproduct [ty1';ty2'] in
                      eval_net_application tp globals (bs,ws) nenv loc f v ty' in
                    rs@[r'], bs@bs', ws@ws')
                  ([],[],[])
                  vs1
                  vs2 in
              SVTuple rs, bs', ws'
          | _, _, _ -> fatal_error "Static.eval_ho_prim(map2)" (* should not happen, thx to TC *) end
      | "map2i" ->
          begin match pre_args, arg, tyarg with
            [f], SVTuple [SVTuple vs1; SVTuple vs2], Tproduct [Tproduct (ty1'::_); Tproduct (ty2'::_)]
          | [f], SVTuple [SVTuple vs1; SVTuple vs2], Tproduct [Tproduct (ty1'::_); Tconstr({tc_name="bundle"},[ty2'],[_])]
          | [f], SVTuple [SVTuple vs1; SVTuple vs2], Tproduct [Tconstr({tc_name="bundle"},[ty1'],[_]); Tproduct (ty2'::_)]
          | [f], SVTuple [SVTuple vs1; SVTuple vs2], Tproduct [Tconstr({tc_name="bundle"},[ty1'],[_]); Tconstr({tc_name="bundle"},[ty2'],[_]) ] ->
              let rs, bs', ws' =
                Misc.foldl2_index
                  (fun i (rs,bs,ws) v1 v2 ->
                    let v_i = SVVal (Expr.Val_int (i, None)) in
                    let ty_i = type_unsigned (new_size_var ()) in
                    let v = SVTuple [v1;v2] in
                    let ty' = Tproduct [ty1';ty2'] in
                    let r', bs', ws' =
                      let f', bs'', ws'' = eval_net_application tp globals (bs,ws) nenv loc f v_i ty_i in
                      eval_net_application tp globals (bs,ws) nenv loc f' v ty' in
                    rs@[r'], bs@bs', ws@ws')
                  ([],[],[])
                  vs1
                  vs2 in
              SVTuple rs, bs', ws'
          | _, _, _ -> fatal_error "Static.eval_ho_prim(map2i)" (* should not happen, thx to TC *) end
      | "foldl" ->
          begin match pre_args, arg, tyarg with
            [f], SVTuple (v1::v2::vs), Tproduct (ty'::_)
          | [f], SVTuple (v1::v2::vs), Tconstr({tc_name="bundle"},[ty'],[_]) ->
              let v = SVTuple [v1;v2] in
              let ty'' = Tproduct [ty';ty'] in
              let r1, bs1, ws1 = eval_net_application tp globals ([],[]) nenv loc f v ty'' in
              let r, bs', ws' =
                List.fold_left
                  (fun (v,bs,ws) v' ->
                    let v'' = SVTuple [v;v'] in
                    let ty'' = Tproduct [ty';ty'] in
                    let r', bs', ws' =
                      eval_net_application tp globals ([],[]) nenv loc f v'' ty'' in
                    r', bs@bs', ws@ws')
                  (r1,bs1,ws1)
                  vs in
              r, bs', ws'
          | _, SVTuple _, _ -> invalid_foldl_argument loc
          | _, _, _ -> fatal_error "Static.eval_ho_prim(foldl)" (* should not happen, thx to TC *) end
      | "foldt" ->
          begin match pre_args, arg, tyarg with
            [f], SVTuple vs, Tproduct (ty'::_)
          | [f], SVTuple vs, Tconstr({tc_name="bundle"},[ty'],[_]) ->
              if Misc.is_pow2 (List.length vs) then begin
                let vs' = List.map (function v -> v,[],[]) vs in
                let r, bs', ws' =
                  Misc.fold_tree
                    (fun (v1,bs1,ws1) (v2,bs2,ws2) ->
                      let v'' = SVTuple [v1;v2] in
                      let ty'' = Tproduct [ty';ty'] in
                      let r', bs', ws' = eval_net_application tp globals ([],[]) nenv loc f v'' ty'' in
                      r', bs1@bs2@bs', ws1@ws2@ws')
                    vs' in
              r, bs', ws'
              end
              else
                invalid_foldt_argument loc
          | _, _, _ -> fatal_error "Static.eval_ho_prim(foldt)" (* should not happen, thx to TC *) end
      | "foldli" ->
          begin match pre_args, arg, tyarg with
            [f], SVTuple (v1::v2::vs), Tproduct (ty'::_)
          | [f], SVTuple (v1::v2::vs), Tconstr({tc_name="bundle"},[ty'],[_]) ->
              let eval_node i v1 v2 ty = 
                let v_i = SVVal (Expr.Val_int (i, None)) in
                let ty_i = type_unsigned (new_size_var ()) in
                let v' = SVTuple [v1;v2] in
                let ty' = Tproduct [ty;ty] in
                let f', bs', ws' = eval_net_application tp globals ([],[]) nenv loc f v_i ty_i in
                eval_net_application tp globals (bs',ws') nenv loc f' v' ty' in
              let r1, bs1, ws1 = eval_node 0 v1 v2 ty' in
              let r, bs', ws' =
                Misc.foldl_index
                  (fun i (v,bs,ws) v' ->
                    let r', bs', ws' = eval_node (i+1) v v' ty' in
                    r', bs@bs', ws@ws')
                  (r1,bs1,ws1)
                  vs in
              r, bs', ws'
          | _, SVTuple _, _ -> invalid_foldl_argument loc
          | _, _, _ -> fatal_error "Static.eval_ho_prim(foldli)" (* should not happen, thx to TC *) end
      | "pipe" ->
          begin match pre_args with
            [SVVal (Expr.Val_int (n,_)); ff] when n > 0 ->
              let rec apply n (v,bs,ws) =
                if n=0 then
                  (v,bs,ws)
                else
                  let v', bs', ws' = eval_net_application tp globals ([],[]) nenv loc ff v tyarg in
                  apply (n-1) (v',bs@bs',ws@ws') in
              apply n (arg,[],[])
          | [_; _] -> invalid_pipe_argument loc "pipe"
          | _ -> fatal_error "Static.eval_ho_prim(pipe)" (* should not happen, thx to TC *) end
      | "chain" ->
          begin match pre_args with
            [SVVal (Expr.Val_int (n,_)); ff] when n > 0 ->
             let rec apply n (vs,bs,ws) =
               begin match n, vs with
                | 0, vs -> List.rev vs, bs, ws
                | n, v::_ ->
                  let v', bs', ws' = eval_net_application tp globals ([],[]) nenv loc ff v tyarg in
                  apply (n-1) (v'::vs,bs@bs',ws@ws')
                | _, _ -> fatal_error "Static.eval_ho_prim(chain)" 
               end in
              let vs, bs, ws = apply n ([arg],[],[]) in
              SVTuple (List.tl vs), bs, ws
          | [_; _] -> invalid_pipe_argument loc "chain"
          | _ -> fatal_error "Static.eval_ho_prim(chain)" (* should not happen, thx to TC *) end
      | "repln" -> 
          begin match pre_args, arg, tyarg with
            [SVVal (Expr.Val_int (n,_))], v, _ when n > 0 ->
              let r = SVTuple (Misc.list_repl n v) in
              r, bs, ws
          | _, _, _ -> fatal_error "Static.eval_ho_prim(repln)" (* should not happen, thx to TC *) end
      | "napp" -> 
          begin match pre_args with
            [SVVal (Expr.Val_int (n,_)); ff] ->
              let args = SVTuple (Misc.list_repl n arg) in
              let tyargs = type_bundle tyarg (SzConst n) in
              eval_ho_prim_application tp globals (bs,ws) nenv loc "map" [ff] args tyargs
          | _ -> fatal_error "Static.eval_ho_prim(napp)" (* should not happen, thx to TC *) end
      | "nappi" -> 
          begin match pre_args with
            [SVVal (Expr.Val_int (n,_)); ff] ->
              let args = SVTuple (Misc.list_repl n arg) in
              let tyargs = type_bundle tyarg (SzConst n) in
              eval_ho_prim_application tp globals (bs,ws) nenv loc "mapi" [ff] args tyargs
          | _ -> fatal_error "Static.eval_ho_prim(nappi)" (* should not happen, thx to TC *) end
      | _ -> fatal_error "Static.eval_ho_prim"  (* should not happen, thx to TC *)

(* Rule TE,EE,NE |- let/net [rec] npat1=nexp1 ... npatn=nexpn => NE', B, W *)

and eval_net_defns loc toplevel isrec tp globals nenv bindings =
  if not isrec then                                        (* NON RECURSIVE CASE *)
    let eval_net_binding {nb_desc=npat,nexp} =
      let v, bs', ws' = eval_net_expr tp globals nenv nexp in
      let nenv', ws'' =
        begin try 
          if toplevel
          then net_matching true nenv npat v
          else net_matching false [] npat v
        with Matching_fail ->  binding_error npat.np_loc end in
      nenv', bs', ws'@ws'' in
    let nenvs', boxes', wires' = Misc.list_split3 (List.map eval_net_binding bindings) in 
    List.concat nenvs',
    List.concat boxes',
    List.concat wires'
  else                                                     (* RECURSIVE CASE *)
    if List.for_all is_fun_definition bindings             (* All recursive _functions_ *)
    then begin
      let rec_cls =
        List.map
          (function
              {nb_desc={np_desc=NPat_var v}, {ne_desc=NFun (p,e)}; nb_loc=loc} ->
                v, {cl_pat=p; cl_exp=e; cl_env=[]}
            | _ ->
                failwith "Static.eval_net_defns")  (* should not happen *)
          bindings in
      let nenv' = List.map (function (v,cl) -> (v, SVClos cl)) rec_cls in
      List.iter (function (v,cl) -> cl.cl_env <- nenv' @ nenv) rec_cls;
      nenv', [], []
    end
    else if not (List.exists is_fun_definition bindings)   (* No recursive function *)
    then begin
      let rec_env, bs =
        List.fold_left
          (fun (env,bs) {nb_desc=npat,nexp} ->
            let env',bs' = create_rec_bindings toplevel nenv npat in
            env @ env', bs @ bs')
          ([],[])
          bindings in
      let vs', bs', ws' =
        List.fold_left
          (fun (vs,bs,ws) {nb_desc=npat,nexp} ->
             let v',bs',ws' = eval_net_expr tp globals (nenv @ rec_env) nexp in
             vs @ [v'], bs @ bs', ws @ ws')
          ([],[],[])
          bindings in
      let nenv', ws'' =
        List.fold_left2
          (fun (env,ws) {nb_desc=npat,nexp} v ->
            let env',ws' =
              begin try
                if toplevel
                then net_matching true nenv npat v
                else net_matching false [] npat v
              with Matching_fail ->  binding_error npat.np_loc end in
            env @ env', ws @ ws')
          ([],[])
          bindings vs' in
      let substs = List.map (mk_subst loc nenv') rec_env in
      let ws''' = List.fold_left (fun ws subst -> List.map (apply_subst subst) ws) (ws'@ws'') substs in
      nenv', bs', ws'''
    end
    else
      illegal_rec_definition loc

(* RULES REC PATTERN 1, 2 :  |-r NPat => NE, B *)

and create_rec_bindings toplevel nenv npat =
  match npat.np_desc with
    | NPat_var id ->
        begin match toplevel, is_output nenv id with
          false, _
        | true, None ->
            let ty = type_copy npat.np_typ in
            let l, b = new_dummy_box id ty in
            [id, SVLoc(l,0,ty,false)], [(l,b)]
        | true, Some l ->
            (* Note 2013-04-12, JS
               Outputs are _not_ added to the recursive environment *)
            [], []
        end
    | NPat_tuple ps ->
        List.fold_left
          (fun (ne,bs) p ->
            let ne', bs' = create_rec_bindings toplevel nenv p in
            ne' @ ne, bs' @ bs)
          ([],[])
          ps
    | NPat_unit -> not_implemented "recursive unit pattern"
(*     | NPat_bundle ps -> not_implemented "recursive bundle pattern" *)

and mk_subst loc nenv (rid,rv) =
  match rv, List.assoc rid nenv with
    SVLoc(l,s,_,_), SVLoc(l',s',_,_) -> (l,s),(l',s')
  | _, _ -> illegal_rec_definition loc

and apply_subst ((i,s),(i',s')) (wid,((src,dst),ty)) =
  let sub (k,l) =
    if k=i then (i',s') else (k,l) in
  (wid, ((sub src, sub dst), ty))


(* Auxilliaries *)

and instanciate_actor_desc a actual_params = 
  let renv =
    Misc.foldl_index
      (fun i r (v,_) -> match v with
      | Expr.Val_int (n,_) -> (i+1,n)::r
      | _ -> r)
      []
     actual_params in
  SVAct
    { sa_id = a.sa_id;
      sa_params = List.map2
        (fun (id,formal_ty,_) (v,actual_ty) -> (id,actual_ty,Some v))
        a.sa_params
        actual_params;
      sa_typ = { a.sa_typ with ts_body = Types.deref renv a.sa_typ.ts_body };
      sa_fulltyp = { a.sa_fulltyp with ts_body = Types.deref renv a.sa_fulltyp.ts_body };
      sa_ins = List.map (function (id,ty) -> (id, Types.deref renv ty)) a.sa_ins;
      sa_outs = List.map (function (id,ty) -> (id, Types.deref renv ty)) a.sa_outs;
      sa_vars = List.map (function (id,(v,ty),loc) -> (id, (apply_option (deref_exp renv) v,Types.deref renv ty), loc)) a.sa_vars;
      sa_rules = List.map (function (rule,ty,rsig,loc) -> (deref_rule renv rule, Types.deref renv ty, rsig, loc)) a.sa_rules;
      sa_impl = a.sa_impl;
      sa_types = a.sa_types }
          
and instanciate_actor tp globals nenv loc a args =
  let senv = List.fold_left (fun acc (id,v) -> match v with SVVal v' -> (id,v')::acc | _ -> acc) [] nenv in
  let tyins, tyouts, typarams, tyact, tvbs, svbs = instanciate_actor_ios loc a args in
  let bins =
    if is_unit_type tyins then []
    else List.map2 (fun (id,_) ty -> (id,(0,ty))) a.sa_ins (list_of_types tyins) in
  let bouts =
    if is_unit_type tyouts then []
    else List.map2 (fun (id,_) ty -> (id,([0],ty))) a.sa_outs (list_of_types tyouts) in
  let bparams, fpbs = List.fold_left2
      (fun (acc,acc') p ty -> match p, ty with
       | (id,_,Some (Expr.Val_fun (fid, _, _))), Tarrow _ ->  (* function parameter *)
           acc, acc' @ [id, fid]
       | (id,_,Some v), t ->
           acc @ [id, (v,t)], acc'
       | _, _ -> illegal_actor_instanciation loc)
      ([],[])
      a.sa_params
      (list_of_types typarams) in 
  let senv' = List.map (fun (id,(v,ty)) -> id,v) bparams @ senv  in
  let bvars =
    List.map
      (function (id,(e,ty),loc) ->
        let ty' = copy_type tvbs svbs ty in
        let v = begin match e with
             None -> None
           | Some e' ->
               let e'' = specialize_exp tp globals tvbs svbs fpbs e' in
               let v' = Expr.eval_expression [id,ty] senv' e'' in
               (* Note 2015-12-11, JS.
                  Adding [(id,ty)] to the local type environment is a hack for being able to interpret decls like :
                     var z: array[k] = [ v | i=0 to z`size-1] *)
               if Expr.is_static_constant v' then Some v'
               else illegal_static_constant e'.e_loc end in
        Typing.update_tc_insts tp.tp_types loc ty';
        id, (v, ty'))
      a.sa_vars in
  let tyvars = type_product (List.map (function (_,(_,t)) -> t) bvars) in
(*   let senv'' = List.map (function id,(v,ty) -> id, match v with Some v -> v | None -> Expr.Val_unknown) bvars @ senv' in *)
(*   let tenv' = { tenv with te_values = lenv @ tenv.te_values } in *)
  let brules = 
    List.map
      (function (rule,ty,rsig,loc) ->
        let ty' = copy_type tvbs svbs ty in
        let rule' = specialize_rule tp globals tvbs svbs fpbs rule in
        (rule', ty', rsig))
      a.sa_rules in
  let l, b = new_rbox a.sa_id tyact typarams tyins tyvars tyouts tvbs svbs fpbs bins bouts a.sa_types bparams bvars brules a.sa_impl in
  let actual_type = type_product [typarams;tyins;tyvars;tyouts] in
  let actual_fn_params = List.map snd fpbs in
  add_actor_instance globals a.sa_id (actual_type,actual_fn_params) b l;
  let mk_wire l v = match v with
    SVLoc(i,j,ty,_) -> new_wid(), (((i,j),l),ty)
  | _ -> illegal_application loc in
  let tyins' = list_of_types tyins in
  let tyouts' = list_of_types tyouts in
  match tyins', a.sa_ins, tyouts', a.sa_outs, args with
  | [], [_], [], [_], SVUnit ->                                                 (* APP_0_0 *)
     SVUnit,
     [l,b],
     []
  | [t], [_], [], [_], SVLoc(l1,s1,ty,false) ->                                 (* APP_1_0 *)
     let w = ((l1,s1),(l,0)), t in
     SVUnit,
     [l,b],
     [new_wid(),w]
  | ts, _, [], [_], SVTuple vs when List.length ts > 1 ->                       (* APP_m_0 *)
     let ws'' = Misc.list_map_index (fun i v -> mk_wire (l,i) v) vs in
     SVUnit,
     [l,b],
     ws''
  | [], [_], [t], [_], SVUnit ->                                                (* APP_0_1 *)
      SVLoc (l,0,t,false),
      [l,b],
      []
  | [], [_], ts, _, SVUnit when List.length ts > 1 ->                           (* APP_0_n *)
      SVTuple (Misc.list_map_index (fun i ty -> SVLoc(l,i,ty,false)) ts),
      [l,b],
      []
  | [t], _, [t'], _, SVLoc(l1,s1,ty,false) ->                                   (* APP_1_1 *)
      let w = ((l1,s1),(l,0)), t in
      SVLoc (l,0,t',false),
      [l,b],
      [new_wid(),w]
  | [t], _, ts', _, SVLoc(l1,s1,ty,false) when List.length ts' > 1 ->           (* APP_1_n *)
      let w = ((l1,s1),(l,0)), t in
      SVTuple (Misc.list_map_index (fun i ty -> SVLoc(l,i,ty,false)) ts'),
      [l,b],
      [new_wid(),w]
  | ts, _, [t'], _, SVTuple vs when List.length ts > 1 ->                       (* APP_m_1 *)
      let ws'' = Misc.list_map_index (fun i v -> mk_wire (l,i) v) vs in
      SVLoc (l,0,t',false),
      [l,b],
      ws''
  | ts, _, ts', _, SVTuple vs when List.length ts > 1 && List.length ts' > 1 -> (* APP_m_n *)
      let ws'' = Misc.list_map_index (fun i v -> mk_wire (l,i) v) vs in
      SVTuple (Misc.list_map_index (fun i ty -> SVLoc(l,i,ty,false)) ts'),
      [l,b],
      ws''
  | _ ->
      illegal_application loc

and instanciate_actor_ios loc a args =
  let ty_param = match a.sa_params with
  | [] -> type_unit
  | [_,ty,_] -> type_copy ty
  | ps -> type_product (List.map (function (_,ty,_) -> type_copy ty) ps) in
  let ty_arg = match args with
    (* TODO : handle the case of actors with NO arg ? *)
    SVLoc(l,s,ty,false) -> ty
  | SVTuple vs ->
      type_product
        (List.map
           (function
               SVLoc(l,s,ty,false) -> ty
             | _ -> illegal_application loc)
           vs)
  | SVUnit -> type_unit
  | _ -> illegal_application loc in
  match a.sa_params (* ,a.sa_vars *) with
    [] ->
      let ty_fn, ty_res, tv_bindings, sv_bindings = type_application (ELoc loc) a.sa_typ ty_arg in
      ty_arg, ty_res, type_unit, ty_fn, tv_bindings, sv_bindings
  | _ ->
      let ty_fn, ty_res, tv_bindings, sv_bindings = type_application2 (ELoc loc) a.sa_typ ty_param ty_arg in
      ty_arg, ty_res, ty_param, ty_fn, tv_bindings, sv_bindings

(* Rule signatures (since v2.8.4) *)

let ipat_sig rp = match rp.Syntax.rp_desc with
| _ -> 1  (* There's no "dont read" pattern *)
 
let rec oexp_sig e = match e.Syntax.e_desc with
| EIgnored -> 0
| _ -> 1  (* Very crude approximation.. Can we (statically) do better ? *)

let rule_signature (inps,outps) (rpats,rguards,rexprs) =
  let get_ipat i =
    let rec find = function
      [] -> raise Not_found
    | ({Syntax.q_desc=QIn i'}, rp)::_ when i=i' -> rp
    | _::rest -> find rest in
    find rpats in
  let get_oexp o =
    let rec find = function
      [] -> raise Not_found
    | ({Syntax.q_desc=QOut o'}, e)::_ when o=o' -> e
    | _::rest -> find rest in
    find rexprs in
  let inp_signature (i,ty) = try ipat_sig (get_ipat i) with Not_found -> 0 in
  let outp_signature (o,ty) = try oexp_sig (get_oexp o) with Not_found -> 0 in
  List.map inp_signature inps,
  List.map outp_signature outps

(* RULE |- ActDecl => NE *)

let rec eval_actor_decl tp { act_desc = a } =
  let ta = List.assoc a.a_id tp.tp_actors in
  let rules =
      List.map2
        (fun {rule_desc={rlhs_desc=rpats;rlhs_loc=Loc(f,l1,_)},
              {rgrd_desc=rguards},
              {rrhs_desc=rexprs;rrhs_loc=Loc(_,_,l2)}} ty ->
          (rpats,rguards,rexprs), ty, rule_signature (ta.at_ins,ta.at_outs) (rpats,rguards,rexprs), Loc(f,l1,l2))
        a.a_rules
        ta.at_rules in
  let sa = {
    sa_id = a.a_id;
    sa_typ = ta.at_sig;
    sa_fulltyp = ta.at_fullsig;
    sa_params = List.map (function (id,ty) -> (id,ty,None)) ta.at_params;
    sa_ins = ta.at_ins;
    sa_outs = ta.at_outs;
    sa_types = 
      List.fold_left 
        (fun ts (id,t) -> match t.tc_defn with
          | {ty_desc=Variant_type ([],[],cts)} ->
              (id, SVEnumDefn (List.map (function c -> c.cs_name) cts)) :: ts
          | _ -> ts)
        []
        ta.at_types;
    sa_vars =
      List.map
        (* Local variables will be evaluated when the actor is instanciated as a box since
           their initial value may depend on the actual box parameters.. *)
        (function { var_desc=id, _, e; var_loc=loc } ->
          id, (e, Misc.lookup "Static.eval_actor_decl" id ta.at_vars), loc)
        a.a_vars;
    sa_rules = rules;
    sa_impl = a.a_impl;
  } in
  a.a_id, SVAct sa

(* RULE |- IoDecl => NE,B *)

let rec eval_io_decl tp (ne,bs) d =
  match d.io_desc with
    kind, id, ty, dir, dev, ival ->
      let ty = List.assoc id tp.tp_ios in
      let (l, b), is_output = begin match kind, dir, ival with
      | StreamIO, IoIn, _ -> new_ibox kind id ty dev None, false
      | StreamIO, IoOut, _ -> new_obox kind id ty dev None, true
      | PortIO, IoIn, None -> new_ibox kind id ty dev None, false
      | PortIO, IoIn, Some e -> new_ibox kind id ty dev (Some (Expr.eval_expression [] [] e)), false
      | PortIO, IoOut, _ -> new_obox kind id ty dev None, true end in
      (id,SVLoc (l,0,ty,is_output)) :: ne, (l,b) :: bs

and eval_simple_net_expr nenv expr =
  match expr.ne_desc with
  | NVar v ->
      if List.mem_assoc v nenv then 
        begin match List.assoc v nenv with
          SVVal v -> v
        | _ -> illegal_expression expr.ne_loc
        end
      else
        unbound_value_err v expr.ne_loc
  | NConst c -> Expr.eval_const expr.ne_loc c
  | NArray1Const es -> 
      let a = Array1.of_list (List.map (Expr.eval_const expr.ne_loc) es) in
      Expr.Val_array1 (Array1.size a, a)
  | NArray2Const ess -> 
      let a = Array2.of_list (List.map (List.map (Expr.eval_const expr.ne_loc)) ess) in
      Expr.Val_array2 (Array2.size a, a)
  | NArray3Const esss -> 
      let a = Array3.of_list (List.map (List.map (List.map (Expr.eval_const expr.ne_loc))) esss) in
      Expr.Val_array3 (Array3.size a, a)
  | _ -> illegal_expression expr.ne_loc

(* RULE NE,B,W |- NetDecl => NE',B',W *)

let eval_net_decl tp globals (nenv,boxes,wires) { nd_desc = isrec, defns; nd_loc=loc } = 
  (* TODO: inject senv here below .. *)
  let nenvs', boxes', wires' = eval_net_defns loc true isrec tp globals nenv defns in 
  nenv @ nenvs',
  boxes @ boxes',
  wires @ wires'

(* RULE W |- B => B' *)

exception BoxWiring of string * bid * wid

let rec update_wids wires (bid,b) =
  try
    bid, { b with b_ins = Misc.list_map_index (fun sel (id,(_,ty)) -> id, (find_src_wire wires bid sel, ty)) b.b_ins;
           b_outs = Misc.list_map_index (fun sel (id,(_,ty)) -> id, (find_dst_wire wires bid sel, ty)) b.b_outs }
  with
    BoxWiring (where,bid,sel) -> unwired_box where b.b_name sel

and find_src_wire wires bid sel =
  let find wids (wid, ((_,(d,ds)),_)) = if d=bid && ds=sel then wid::wids else wids in
  match List.fold_left find [] wires with
    [] -> raise (BoxWiring ("input",bid,sel))
  | [w] -> w
  | ws -> fatal_error "find_src_wire: more than one source wire" (* should not happen ! *)

and find_dst_wire wires bid sel =
  let find wids (wid, (((s,ss),_),_)) = if s=bid && ss=sel then wid::wids else wids in
  match List.fold_left find [] wires with
    [] ->  raise (BoxWiring ("output",bid,sel))
  | ws -> ws

(* RULE TE,EE |- ValDecl => EE' *)

let eval_val_decl tp nenv env d =
    match d.val_desc with
      VConst(id, exp, _) ->
        let v = Expr.eval_expression [] (nenv @ env) exp in
        if Expr.is_static_constant v
        then (id,v) :: env
        else illegal_static_constant d.val_loc
    | VFun(id,pats,exp,_) ->
        (id, Expr.Val_fun (id, pats, exp)) :: env
    | VExtern(id,_,c_name,vhd_name,ml_name) ->
        (id, 
        Expr.Val_extern
          { Expr.ef_c_name = c_name;
            Expr.ef_vhd_name = vhd_name;
            Expr.ef_ml_name = ml_name;
            Expr.ef_type = type_instance d.val_typ }) :: env

(* RULE NE |- Program => NE,B,W *)

let build_static tp senv p =
(*   let senv' = List.fold_left add_type_ctors senv tp.tp_types in *)
     (* Note 2015-05-20, JS. Add the _user-defined_ values ctors to the static environment.
        Do we really need this ? The ctors have been registered (and checked) in [tp] *)
  let ne_v  = List.fold_left (eval_val_decl tp senv.Expr.se_vals) [] p.vals in
     (* Note 2015-05-20, JS. [senv.se_ctors] is not in scope here. Should it ? 
        It should if we want builtin and user-defined value ctors to appear in static exprs.
        Like, for ex: [type tau = Foo | Bar; ... const z = Foo; ...; const zz = SoS; ...].
        But do we really want (need) this ? *)
  let ne_a  = List.map (eval_actor_decl tp) p.actors in
  let ne_s, bs = List.fold_left (eval_io_decl tp) ([],[]) p.ios in
  let globals = mk_globals tp ne_v ne_a p.vals in
  let ne', bs', ws' =
    List.fold_left
      (eval_net_decl tp globals)
      (ne_s @ ne_a @ List.map (function (id,v) -> id, SVVal v) (ne_v @ senv.Expr.se_vals), bs, [])
      p.nets in
  let bs'' = List.map (update_wids ws') bs' in
  { e_vals = ne_v;
    n_vals = ne';
    boxes = bs'';
    wires =  ws';
    gacts = globals.gl_actors;
    gfuns = globals.gl_funs;
    gcsts = globals.gl_csts;
    gtyps =
      (* Only monomorphic types and polymorphic types which have been instanciated at least one are collected here *)
      List.fold_left
        (fun acc (name,td) -> match td.tc_defn with
         | { ty_arity=m,n; ty_desc=Variant_type (_,_,_) }
              when m=0 && n=0 || TyconInsts.card td.tc_insts > 0 -> (name, td) :: acc
         | _ -> acc)
      []
      tp.tp_types }

(* Printing *)

let print_net_value pfx tp (name,r) =
  let type_of name = 
    try
      string_of_type_scheme
        (List.assoc
           name
           ((List.map (function (id,t) -> id,trivial_scheme t) tp.tp_ios)))
    with Not_found -> 
      begin try
        string_of_type_scheme
          ((List.assoc name (tp.tp_actors)).at_sig)
      with Not_found -> "?" end in
  printf "%sval %s : %s = %a\n" pfx name (type_of name) output_ss_value r;
  flush stdout

let string_of_typ_var_inst (tv,ty) = "'" ^ Pr_type.TypeVarNames.name_of tv ^ "=" ^ string_of_type ty
let string_of_siz_var_inst (sv,sz) = "#" ^ Pr_type.SizeVarNames.name_of sv ^ "=" ^ string_of_size sz
let string_of_fparam_inst (f,f') = f ^ "=" ^ f'
let string_of_typed_bin (id,(wid,ty)) = id ^ ":" ^ string_of_type ty ^ "(<-W" ^ string_of_int wid ^ ")"
let string_of_typed_bout (id,(wids,ty)) = 
    id ^ ":" ^ string_of_type ty ^ "(->["
  ^ (Misc.string_of_list (function wid -> "W" ^ string_of_int wid) "," wids) ^ "])"
let rec string_of_typed_bvar (id,(iv,ty)) = id ^ ":" ^ string_of_type ty ^ string_of_opt_val iv
and string_of_opt_val = function None -> "" | Some v -> "=" ^ Expr.string_of_val v
let string_of_typed_bparam (id,(v,ty)) = id ^ ":" ^ string_of_type ty ^ "=" ^ Expr.string_of_val v

let string_of_typed_io (id,ty) = id ^ ":" ^ string_of_type ty
let string_of_typed_param (id,ty,v) =
  id ^ ":" ^ string_of_type ty ^ (match v with None -> "" | Some v -> "=" ^ Expr.string_of_val v)
let string_of_typed_var (id,(e,ty),_) = match e with
| None ->  id ^ ":" ^ string_of_type ty
| Some e -> id ^ ":" ^ string_of_type ty ^ "=" ^ Syntax.string_of_expr e

let string_of_moc = function 
    Moc_SDF r -> "sdf<" ^ string_of_rule_sig r ^ ">"
  | Moc_CSDF rs -> "csdf<" ^ Misc.string_of_list (function r -> "[" ^ string_of_rule_sig r ^ "]") "," rs ^ ">"
  | Moc_DDF -> "ddf"
  | Moc_Unknown -> "???"

let rec string_of_rule ((qpats,guards,qexps),_,_) =
  Misc.string_of_list Syntax.string_of_qualified_pattern ", " qpats
^ string_of_guard_exps guards
^ " -> "
^ Misc.string_of_list string_of_qualified_expr' ", " qexps

and string_of_qualified_expr' (q,e) =
  Syntax.string_of_qualifier q.q_desc ^ ":" ^ Misc.trim !max_label_width (Misc.htmlize (Syntax.string_of_expr e))

and string_of_guard_exps = function
    [] -> ""
  | es -> ", " ^ Misc.string_of_list Syntax.string_of_expr ", " es

let string_of_rule_with_sig ((rd,_,s) as r) = 
  string_of_rule r ^ " [" ^ string_of_rule_sig s ^ "]"

let print_actor_inst ((ty,fns),(_,bids)) =
  Printf.printf "  instanciated as %s with type %s and function(s) %s\n"
    (Misc.string_of_list (function bid -> "B" ^ string_of_int bid) ","  bids)
    (string_of_type ty)
    (Misc.string_of_list (function fid -> fid) ","  fns)

let print_actor (id,ac) = 
  Pr_type.TypeVarNames.reset ();
  Pr_type.SizeVarNames.reset ();
  let a = ac.ac_defn in 
  Printf.printf "%s: %s : params=[%s] ins=[%s] outs=[%s] vars=[%s]\n"
    a.sa_id
    (string_of_type_scheme a.sa_typ)
    (Misc.string_of_list string_of_typed_param ","  a.sa_params)
    (Misc.string_of_list string_of_typed_io ","  a.sa_ins)
    (Misc.string_of_list string_of_typed_io ","  a.sa_outs)
    (Misc.string_of_list string_of_typed_var ","  a.sa_vars);
  (ActInsts.iter print_actor_inst ac.ac_insts)

let print_box (i,b) =
  Pr_type.TypeVarNames.reset ();
  Pr_type.SizeVarNames.reset ();
    (* Note 2013-04-25, JS
       No free type variable should be remain at the box level (polymorphic actors have normally 
       been instanciated as monomorphic boxes.
       The above statement is just for helping debug.. *)
  match b.b_tag with
    RegularB ->
      Printf.printf "B%d: %s (tvbs=[%s],svbs=[%s],fpbs=[%s]) params=[%s] ins=[%s] outs=[%s] vars=[%s] moc=%s)\n"
        i
        b.b_name
        (Misc.string_of_list string_of_typ_var_inst ","  b.b_tvbs)
        (Misc.string_of_list string_of_siz_var_inst ","  b.b_svbs)
        (Misc.string_of_list string_of_fparam_inst ","  b.b_fpbs)
        (Misc.string_of_list string_of_typed_bparam ","  b.b_params)
        (Misc.string_of_list string_of_typed_bin ","  b.b_ins)
        (Misc.string_of_list string_of_typed_bout ","  b.b_outs)
        (Misc.string_of_list string_of_typed_bvar ","  b.b_vars)
        (string_of_moc b.b_moc)
  | InpB kind ->
      Printf.printf "I%d: %s: %s (outs=[%s] kind=%s)\n"
        i
        b.b_name
(*         (string_of_type (type_of_ibox b)) *)
        (string_of_type b.b_typ)
        (Misc.string_of_list string_of_typed_bout ","  b.b_outs)
        (string_of_io_kind kind)
  | OutB kind ->
      Printf.printf "O%d: %s: %s (ins=[%s] kind=%s)\n"
        i
        b.b_name
        (string_of_type b.b_typ)
(*         (string_of_type (type_of_obox b)) *)
        (Misc.string_of_list string_of_typed_bin ","  b.b_ins)
        (string_of_io_kind kind)
  | DummyB ->   (* Should not appear in the final network.. *)
      Printf.printf "D%d: %s (ins=[%s] outs=[%s])\n"
        i
        b.b_name
        (Misc.string_of_list string_of_typed_bin ","  b.b_ins)
        (Misc.string_of_list string_of_typed_bout ","  b.b_outs)

let print_wire (i,(((s,ss),(d,ds)),ty)) =
  Printf.printf "W%d: %s: (B%d,%d) -> (B%d,%d)\n" i (string_of_type ty) s ss d ds

let print_value tp (name,v) =
  let type_of name = 
      begin try string_of_type_scheme (List.assoc name tp.tp_vals)
      with Not_found -> "?" end in
  printf "val %s : %s = %s\n" name (type_of name) (Expr.string_of_val v);
  flush stdout

let print_global_fn_inst (ty,(expr,loc)) =
  Pr_type.TypeVarNames.reset ();
  Pr_type.SizeVarNames.reset ();
  Printf.printf "  instanciated as %s : %s (@ %s)\n"
(*     (Misc.string_of_two_lists string_of_typ_var_inst string_of_siz_var_inst ","  tvbs svbs) *)
    (Syntax.string_of_expr expr)
    (Pr_type.string_of_type ty)
    (Misc.string_of_list Location.string_of_location "," loc)

let print_global_fn (id,f) = 
  Printf.printf "%s: %s\n" id (string_of_type_scheme f.gf_def.gf_typ); 
  (TypeInsts.iter print_global_fn_inst f.gf_insts)

(* let print_global_const_inst ((tvbs,svbs),((ty,v),_)) = *)
(*   Pr_type.reset_type_var_names (); *)
(*   Pr_type.reset_size_var_names (); *)
(*   Printf.printf "  instanciated with %s as %s : %s\n" *)
(*     (Misc.string_of_two_lists string_of_typ_var_inst string_of_siz_var_inst ","  tvbs svbs) *)
(*     (Expr.string_of_val v) *)
(*     (Pr_type.string_of_type ty) *)

let print_global_const (id,c) = 
  Printf.printf "const %s: %s = %s %s\n"
    id
    (Pr_type.string_of_type c.gc_typ)
    (Expr.string_of_val c.gc_val)
    (string_of_const_annot c.gc_annot)
(*   List.iter print_global_const_inst (Var_insts.to_list c.gc_insts) *)

let print_static_environment tp sp =
  printf "Static environment ---------------\n";
  printf "- Values --------\n";
  List.iter (print_value tp) sp.e_vals;
  printf "- Global consts ---------------------\n";
  List.iter print_global_const sp.gcsts;
  printf "- Global fns ---------------------\n";
  List.iter print_global_fn sp.gfuns;
  printf "- Global types ---------------------\n";
  List.iter Typing.print_type sp.gtyps;
  printf "- Wires --------------------------\n";
  printf "- Actors --------------------------\n";
  List.iter print_actor sp.gacts;
  printf "- Boxes --------------------------\n";
  List.iter print_box sp.boxes;
  printf "- Wires --------------------------\n";
  List.iter print_wire sp.wires;
  printf "----------------------------------\n"

let print_static_boxes sp =
  printf "Boxes --------------------------\n";
  List.iter print_box sp.boxes

