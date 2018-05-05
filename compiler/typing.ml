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
open Types
open Error
open Printf
open Pr_type
open Misc

(* Specialized module for keeping track of type constructor instanciations *)

module TyconInsts =
  Itbl.Make
    (struct
      type t = typ list * siz list
      let equal (ts1,ss1) (ts2,ss2) =
        try
          List.for_all2 type_equal ts1 ts2 && List.for_all2 size_equal ss1 ss2
        with Invalid_argument _ ->
          Misc.fatal_error "Typing.TyconInsts.equal"   (* should not happen *)
    end)

type typing_env = {
  mutable te_types: (string * tc_desc) list;     (* Type constructors *)
  mutable te_ctors: (string * constr_desc) list; (* Value constructors *)
  mutable te_values: (string * typ_scheme) list; (* Values (all) *)
  mutable te_consts: (string * int) list         (* Global integer constants *)  
     (* Note v2.6.1 :
       We need to keep track of the _value_ of global int constants in order to type program like :
           const ntaps = 8 : unsigned<8>;
           ...
           actor fir (..) var z : signed<..> array[ntaps]; *)
  }

and tc_desc = { 
    tc_defn: Types.type_desc;                                                (* Definition *)
    mutable tc_insts: (Types.type_desc, Location.location) TyconInsts.t;     (* Instances *)
  }

(* The following fn is used for generating monomorphic instances of type- and size-polymorphic type defns *)

let rec specialize_type_defn tvbs svbs td = match td with
  Variant_type (tvs,svs,cds) -> Variant_type ([], [], List.map (specialize_cdesc tvbs svbs) cds)
| Abbrev_type _ -> td    (* should not be used *)
| Abstract_type -> td    (* should not be used *)

and specialize_cdesc tvbs svbs c =
  { c with
    cs_params = [],[];    (* Specialisation turns a polymorphic value constr into a monomorphic one *)
    cs_arg = Types.copy_type tvbs svbs c.cs_arg;
    cs_res = Types.copy_type tvbs svbs c.cs_res }

let update_tc_insts env loc ty = 
  match real_type ty with
    Tconstr ({tc_name=name}, ty_args, sz_args) when not (Misc.string_is_prefix "_enum" name) ->
      (* Locally defined enums are not taken into account here *)
      let td =
        begin try List.assoc name env
          with Not_found -> unbound_type_err name loc (* should not happen *)
        end in
      begin match td.tc_defn.ty_desc, td.tc_defn.ty_arity, ty_args, sz_args with
      | Variant_type ([], [], cds), (0,0), _, _ -> ()
      | Variant_type (tvs, svs, cds), (n,m), ty_args, sz_args ->
          if List.for_all (is_ground_type ~strict:true) ty_args && List.for_all is_ground_size sz_args then
            begin
              let tvbs = List.combine tvs (List.map real_type ty_args) in
              let svbs = List.combine svs (List.map size_repr sz_args) in
              let td' = { td.tc_defn with ty_desc = specialize_type_defn tvbs svbs td.tc_defn.ty_desc } in
              TyconInsts.add (ty_args,sz_args) (td',loc) td.tc_insts
            end
      | _ -> () (* should not happen *)
      end
  | _ -> ()

let empty_tenv={ te_types=[]; te_values=[]; te_ctors=[]; te_consts=[] }

let trivial_env = List.map (function (id,ty) -> id, trivial_scheme ty)

let augment_tenv new_types new_ctors new_values new_consts tenv =
  { te_types = tenv.te_types @ new_types;
    te_ctors = tenv.te_ctors @ new_ctors;
    te_values = tenv.te_values @ new_values;
    te_consts = tenv.te_consts @ new_consts }

type var_env = {
    ve_tvars: (string * typ var) list;
    ve_svars: (string * siz var) list;
    ve_srefs: (string * int) list }

let empty_venv={ ve_tvars=[]; ve_svars=[]; ve_srefs=[] }

let the_venv = ref empty_venv

(* Unification *)

let try_unify site ty1 ty2 loc =
  try
    Types.unify ty1 ty2
  with 
    TypeConflict(t1, t2) -> wrong_type_err site t1 t2 loc
  | TypeCircularity(t1, t2) -> circular_type_err site t1 t2 loc

let try_unify_size site (ty1,ty2) sz1 sz2 loc =
  try
    Types.unify_size (ty1,ty2) sz1 sz2
  with 
    TypeConflict(t1, t2) -> wrong_type_err site t1 t2 loc
  | TypeCircularity(t1, t2) -> circular_type_err site t1 t2 loc

and lookup_value tenv loc id =
  try List.assoc id tenv.te_values
  with Not_found -> unbound_value_err id loc

and lookup_ctor tenv loc id =
  try List.assoc id tenv.te_ctors
  with Not_found -> unbound_constr_err id loc

(* Result of the typing phase *)

type typed_program = {
  tp_types: (string * tc_desc) list;     (* Type constructors *)
  tp_ctors: (string * constr_desc) list; (* Value constructors *)
  tp_vals: (string * typ_scheme) list;   (* Global values (constants and functions) *)
  tp_actors: (string * actor_type_desc) list;
  tp_ios: (string * typ) list;                     (* IOs must be monophormic *)
  tp_nets: (string * typ_scheme) list;
  }

and actor_type_desc = {
   at_sig: typ_scheme;                    (* external type signature *)
     (* This signature is either [t_params -> t_ins -> t_outs] or [t_ins -> t_outs] *)
   at_fullsig: typ_scheme;                (* internal type signature *)
     (* This signature is either [t_params -> t_ins -> t_outs * t_vars] or [t_ins -> t_outs * t_vars] *)
   at_types: (string * tc_desc) list;     (* locally defined constructors (enums) *)
   at_ins: (string * typ) list;           (* inputs *)
   at_outs: (string * typ) list;          (* outputs *)
   at_params: (string * typ) list;        (* parameters *)
   at_vars: (string * typ) list;          (* local vars *)
   at_rules: typ list                     (* rules *)
  }

let type_desc_of_type tp ty = match real_type ty with
  | Tconstr ({tc_name=name}, _, _) when List.mem_assoc name tp.tp_types ->
      (List.assoc name tp.tp_types).tc_defn
  | _ -> fatal_error "Typing.type_desc_of_type: not a type constructor" (* should not happen *)

let the_typing_env = ref empty_tenv
  (* This value will be updated by [type_program].
     This is an ugly hack to give access to this environment w/o having to pass an extra parameter to all backend functions *)

(* The special cases of unification are used for retrieving the actual values of generic
   variables when a polymorphic function is specialized by application. *)

let type_application loc ty_fn ty_arg = 
  let ty_fn', tvs, svs = full_type_instance ty_fn in
  let ty_result = new_type_var () in
  try_unify "application" ty_fn' (type_arrow ty_arg ty_result) loc;
  ty_fn', ty_result, tvs, svs

let type_application2 loc ty_fn ty_param ty_arg = 
  let ty_fn', tvs, svs = full_type_instance ty_fn in
  let ty_result = new_type_var () in
  try_unify "application" ty_fn' (type_arrow2 ty_param ty_arg ty_result) loc;
  ty_fn', ty_result, tvs, svs

let mk_array_type loc tyof elems = match elems with
  [] -> empty_array loc
| e::es ->
    let t = tyof e in
    List.iter (function e' -> let t' = tyof e' in try_unify "array" t t' (ELoc loc)) es;
    let sz = SzConst (List.length elems) in
    type_array t sz

(* Type casting / refinement *)

type refine_rel = 
    LessPrecise
  | EquallyPreciseButDifferent
  | Equal
  | MorePrecise

let rec type_cast loc t1 t2 =
    let ty1 = real_type t1
    and ty2 = real_type t2 in
    begin match ty1, ty2 with
      (* INT -> INT *)
    | Tconstr({tc_name="int"}, [sg1], [sz1]), Tconstr({tc_name="int"}, [sg2], [sz2]) ->
        (* Casting [s1 int<z1>] to [s2 int<z2>] is accepted only in two situations :
           1. If it correspond to a "refinement" of the LHS type.
           Examples :
           - [(int<8>:unsigned<8>)] (i.e. ['a int<8>:_unsigned int<8>]) is ok
           - [(int<n>:int<8>)] (i.e. [_a int<s>:'b int<8>]) is ok
           - [(int<n>:signed<8>)] (i.e. [_a int<s>:_signed int<8>]) is ok
           - [(int<n>:signed<n>)] (i.e. [_a int<s>:_signed int<s>]) is ok
           2. If it corresponds to a modification of an already known signness or size
           Examples :
           - [(signed<8>:unsigned<8>)] (i.e. [_signed int<8>:_unsigned int<8>]) is ok
           - [(signed<8>:signed<16>)] (i.e. [_signed int<8>:_signed int<16>]) is ok
           - [(signed<8>:unsigned<16>)] (i.e. [_signed int<8>:_unsigned int<16>]) is ok
           All other cases are illegal.
           Examples :
           - [(int<8>:int<s>)] (i.e. ['a int<8>:'b int<s>]) is not allowed (the known size 8 cannot be "erased")
           - [(signed<8>:int<8>)] (i.e. [_signed int<8>:'a int<8>]) is not allowed (the known signness cannot be "erased")
           Casts of type 1 will always be accepted silently.
           Casts of type 2 will emit a warning when the signness/sizes of the LHS and RHS is/are different. *)
        begin
          match compare_int_signness sg1 sg2, compare_int_size sz1 sz2 with
            RR_Equal, RR_Equal -> ()
          | RR_Equal, RR_Diff -> dubious_cast loc ty1 ty2
          | RR_Equal, RR_Less -> ()
          | RR_Diff, RR_Equal -> dubious_cast loc ty1 ty2 
          | RR_Diff, RR_Diff -> dubious_cast loc ty1 ty2
          | RR_Diff, RR_Less -> dubious_cast loc ty1 ty2
          | RR_Less, RR_Equal -> ()
          | RR_Less, RR_Diff -> dubious_cast loc ty1 ty2
          | RR_Less, RR_Less -> ()
          | _, _ -> illegal_cast loc ty1 ty2
        end
      (* INT -> BOOL or BOOL -> INT *)
      (*  (int/signed/unsigned<z>:bool) is ok (the resulting value is true if the original value is != 0, false otherwise
          (bool:int/signed/unsigned<z>) is ok (the resulting value is 1 for true, 0 for false *)
    | Tconstr({tc_name="int"}, [sg], [sz]), Tconstr({tc_name="bool"}, [], [])
    | Tconstr({tc_name="bool"}, [], []), Tconstr({tc_name="int"}, [sg], [sz]) ->
        ()
      (* INT or BOOL -> FLOAT *)
    | Tconstr({tc_name="int"}, [sg], [sz]), Tconstr({tc_name="float"}, [], [])
    | Tconstr({tc_name="bool"}, [sg], [sz]), Tconstr({tc_name="float"}, [], []) ->
        ()
      (* FLOAT -> BOOL or INT *)
      (*  (float:bool) is ok (the resulting value is true if the original value is != 0.0, false otherwise
          (float:[un]signed<m>) is ok (the frac part is discarded) *)
    | Tconstr({tc_name="float"}, [], []), Tconstr({tc_name="bool"}, [], [])  ->
        ()
    | Tconstr({tc_name="float"}, [], []), Tconstr({tc_name="int"}, [sg], [sz]) ->
        ()
      (* TUPLES *)
    | Tproduct ts1, Tproduct ts2  when List.length ts1 = List.length ts2 ->
        List.iter2 (type_cast loc) ts1 ts2
      (* FNS *)
    | Tarrow (ty1,ty2), Tarrow (ty1',ty2') ->
        type_cast loc ty1 ty1';
        type_cast loc ty2 ty2'
        (* Note 2015-05-23, JS.
           No contravariance here ! For ex
           [int<s> -> int<s'>] can be cast to [int<8> -> int<16>] (or [unsigned<8> -> signed<9>], etc..) 
           but _not the opposite_ *)
      (* ARRAYS and others CONSTRUCTED TYPES *)
    | Tconstr ({tc_name=name1}, ts1, ss1), Tconstr({tc_name=name2}, ts2, ss2) when name1=name2 ->
        List.iter2 (type_cast loc) ts1 ts2;
        List.iter2 (size_cast loc ty1 ty2) ss1 ss2
      (* VAR -> OTHER TYPE : unify *)
    | (Tvar _ as ty1), ty2  ->
        try_unify "type_cast" ty1 ty2 (ELoc loc)
(*     | ty1, (Tvar _ as ty2) -> *)
(*         try_unify "type_cast" ty1 ty2 (ELoc loc) *)
    | _, _ ->
      illegal_cast loc t1 t2 end

and size_cast loc t1 t2 s1 s2 =
    let sz1 = size_repr s1
    and sz2 = size_repr s2 in
    match compare_int_size sz1 sz2 with
      RR_Equal -> ()
    | RR_Diff -> dubious_cast loc t1 t2
    | RR_Less -> ()
    | RR_Greater -> illegal_cast loc t1 t2

(* RULE TE,V,U,R |- tyexpr => tau,V',U' *)

let rec type_of_full_type_expression tenv venv typexp =
  match typexp.te_desc with
      (* The int/signed/unsigned types are handled specially *)
    | Typeconstr("int", [], []) ->
        venv, type_int (new_type_var ()) (new_size_var ()), [], []
    | Typeconstr("int", [], [se]) ->
        let venv', sz = type_of_size_expression tenv venv se in
        venv', type_int (new_type_var ()) sz, [], []
    | Typeconstr("int", [te], [se]) ->
        let venv', sg = type_of_type_expression tenv venv te in
        let venv'', sz = type_of_size_expression tenv venv' se in
        venv'', type_int sg sz, [], []
    | Typeconstr("signed", [], [se]) ->
        let venv', sz = type_of_size_expression tenv venv se in
        venv', type_signed sz, [], []
    | Typeconstr("unsigned", [], [se]) ->
        let venv', sz = type_of_size_expression tenv venv se in
        venv', type_unsigned sz, [], []
    | TypeIntRange(l,h) ->
        let venv', lo = type_of_size_expression tenv venv l in
        let venv'', hi = type_of_size_expression tenv venv' h in
        venv'', type_range lo hi, [], []
    | Typeconstr(name, targs, sargs) ->
        let td =
          try List.assoc name tenv.te_types
          with Not_found -> unbound_type_err name typexp.te_loc in
        if (List.length targs, List.length sargs) <> td.tc_defn.ty_arity then
          type_arity_err td.tc_defn targs sargs typexp.te_loc
        else
          let venv', ty_args = Misc.map_foldl (type_of_type_expression tenv) venv targs in
          let venv'', sz_args = Misc.map_foldl (type_of_size_expression tenv) venv' sargs in
          venv'',
          type_cstr td.tc_defn.ty_constr ty_args sz_args,
          [], []
    | Typevar v ->
        begin try
          venv, Tvar (List.assoc v venv.ve_tvars), [], []
        with Not_found ->
          let tv = mk_type_var () in
          {venv with ve_tvars = (v,tv)::venv.ve_tvars}, Tvar tv, [], []
        end
  | Typetuple ts ->
      let venv', tys = Misc.map_foldl (type_of_type_expression tenv) venv ts in
      venv', type_product tys, [], []
  | Typearrow (t1,t2) ->
      let venv', ty1 = type_of_type_expression tenv venv t1 in
      let venv'', ty2 = type_of_type_expression tenv venv' t2 in
      venv'', type_arrow ty1 ty2, [], []
  | TypeEnum cts ->
      let stamp = new_enum_type_stamp () in
      let ty_name = "_enum" ^ (string_of_int stamp) in
      let ty_con = { tc_name=ty_name; tc_abbr=Tnotabbrev } in
      let ty_ctors = List.map (mk_enum_ctor tenv ty_con) cts in
      let ty_desc = { ty_constr = ty_con; ty_arity = 0,0; ty_desc = Variant_type ([],[],ty_ctors) } in
      venv,
      type_cstr ty_con [] [],
      [ty_name,{tc_defn=ty_desc;tc_insts=TyconInsts.empty ()}],
      List.map (function c -> c.cs_name, c) ty_ctors

and new_enum_type_stamp =
  let cnt = ref 0 in
  function () -> incr cnt; !cnt

and mk_enum_ctor tenv ty_res cid = 
    { cs_name = cid;
      cs_tag = None;
      cs_arity = 0;
      cs_params = [],[];
      cs_res = Tconstr(ty_res, [], []);
      cs_arg = type_unit }
      (* We could check here that [cid] does not appear [tenv].
         Not doing this means that a duplicate ctor will actually shadow a previously declared one *)

and type_of_type_expression tenv venv typexp =
    let venv', t, _, _ = type_of_full_type_expression tenv venv typexp in
    typexp.te_typ <- t;
    venv', t

and type_of_size_expression tenv venv se = 
  match se.se_desc with
    Sizeconst n -> venv, SzConst n
  | Sizevar "" -> venv, new_size_var ()  (* no size specified *)
  | Sizevar v when List.mem_assoc v tenv.te_consts -> (* Added in v2.6.1 *)
      let n = List.assoc v tenv.te_consts in
      venv, SzConst n
  | Sizevar v when List.mem_assoc v venv.ve_srefs ->
      venv, SzRef (List.assoc v venv.ve_srefs)
  | Sizevar v when List.mem_assoc v venv.ve_svars ->
      venv, SzVar (List.assoc v venv.ve_svars)
  | Sizevar v ->
      let sv = mk_size_var () in
      {venv with ve_svars = (v,sv)::venv.ve_svars}, SzVar sv

and is_ground_type ty =
  match real_type ty with
    Types.Tconstr(name, ts, ss) -> List.for_all is_ground_type ts && List.for_all is_ground_size ss
  | _ -> false

and is_ground_size sz = 
  match size_repr sz with
    SzConst _ -> true
  | _ -> false


(* RULE TE |- tydecl => tau,TE' *)

let rec type_type_decl tenv tydecl = 
  let check_unique_var venv v loc =
    if List.mem_assoc v venv.ve_tvars || List.mem_assoc v venv.ve_svars
    then duplicate_param_in_type_decl_err loc in
  match tydecl.td_desc with
  | Syntax.Abbrev_type_decl(name, tyexp) ->
      let venv, ty_body = type_of_type_expression tenv empty_venv tyexp in
      let ty_constr =  { tc_name = name; tc_abbr = Tabbrev ty_body } in
      let ty_desc =
        { ty_constr = ty_constr;
          ty_arity = 0, 0;
          ty_desc = Abbrev_type ty_body } in
      (* No need to check whether the type abbrev is recursive here *)
      { tenv with te_types = tenv.te_types @ [name, {tc_defn=ty_desc; tc_insts=TyconInsts.empty ()}] }
  | Syntax.Variant_type_decl(name, targs, sargs, cdecls) ->
      let ty_constr =  { tc_name = name; tc_abbr = Tnotabbrev } in
      let venv0 =
        List.fold_left
          (fun venv v ->
            check_unique_var venv v tydecl.td_loc;
            { venv with ve_tvars = (v,mk_type_var())::venv.ve_tvars })
          empty_venv
          targs in
      let venv =
        List.fold_left
          (fun venv v ->
            check_unique_var venv v tydecl.td_loc;
            { venv with ve_svars = (v,mk_size_var())::venv.ve_svars })
          venv0
          sargs in
      let tvars = List.map snd (List.rev venv.ve_tvars) in
      let svars = List.map snd (List.rev venv.ve_svars) in
      let ty_res =
        Tconstr (ty_constr,
                 List.map (function v -> Tvar v) tvars,
                 List.map (function v -> SzVar v) svars) in
      let constr_descs =
        List.map
          (function
            | Constr0_decl (id, tag) ->
                id, 0, ty_res, type_unit, tag
            | Constr1_decl (id, arg, tag) ->
                let venv', ty_arg = type_of_type_expression tenv venv arg in
                let arity = 
                  begin match ty_arg with 
                    Tproduct ts -> List.length ts
                  | _ -> 1
                  end in
                id, arity, ty_res, ty_arg, tag)
          cdecls in
      let cdescs =
        List.map
          (function (id,arity,ty_res,ty_arg,tag) ->
            let ts = generalize tenv.te_values (type_arrow ty_arg ty_res) in
            { cs_name = id;
              cs_tag = tag;
              cs_arity = arity;
              cs_params = ts.ts_tparams, ts.ts_sparams;
              cs_res = ty_res;
              cs_arg = ty_arg })
          constr_descs in
      let ty_desc =
        { ty_constr = ty_constr;
          ty_arity = List.length tvars, List.length svars;
          ty_desc = Variant_type (tvars, svars, cdescs) 
          } in
      { tenv with te_types = tenv.te_types @ [name, {tc_defn=ty_desc; tc_insts=TyconInsts.empty ()}];
                  te_ctors = tenv.te_ctors @ List.map (function cd -> cd.cs_name, cd) cdescs }

(* RULE TE, VE |- ValDecl => VE' *)

let rec type_val_decl (tenv,env) vdecl = 
  let augment te ve = { te with te_values = te.te_values @ ve } in
  let type_decl vd = 
    match vd.val_desc with
      VConst (id, exp, _) -> 
        let t = type_expression (augment tenv env) empty_venv exp in
        id, generalize tenv.te_values t
          (* Note 2015-05-14, JS
             We now generalize the type of constants.
             For ex., [const k = 12] has types [for all #s. int<s>] *)
    | VFun (id, pats, exp, None) ->
        let t = type_function (augment tenv env) pats exp vd.val_loc in
        id, generalize tenv.te_values t
    | VFun (id, pats, exp, Some tyexp) ->
        let t = type_function (augment tenv env) pats exp vd.val_loc in
        let venv', t' = type_of_type_expression (augment tenv env) empty_venv tyexp in
        type_cast vd.val_loc t t';
          (* Note 2014-06-16, JS. This is a cast, not a unification ! *)
        id, generalize tenv.te_values t'
    | VExtern (id, tyexp, _, _, _) -> 
        let venv', t = type_of_type_expression (augment tenv env) empty_venv tyexp in
        id, generalize tenv.te_values t
          (* Note 2013-03-30, JS
             Same as for constants. Should we generalize the type of ext fns ? 
             In other words, can we accept polymorphic ext fns ?
             It can  make sense if generic variables refer to int sizes, since a certain amount of
             polymorphism is offered, in VHDL for ex, by (unsized) signed and unsigned quantities.. 
             TO SEE *)
  in
  let id, ty = type_decl vdecl in
  let tenv' = match vdecl.val_desc with
    VConst (id, {e_desc=ECast({e_desc=EConst (Const.CInt (n, _, _))},_)}, _)
  | VConst (id, {e_desc=EConst (Const.CInt (n, _, _))}, _) -> { tenv with te_consts = (id,n)::tenv.te_consts }
  | _ -> tenv in
  vdecl.val_typ <- ty;
  tenv', (id,ty)::env

and type_function tenv pats exp loc =
  let ty_pat, env' = type_fun_pattern pats in
  let ty_result = new_type_var () in
  let ty_expr = type_expression { tenv with te_values = env' @ tenv.te_values } empty_venv exp in
  try_unify "function expression" ty_expr ty_result (ELoc loc);
  type_arrow ty_pat ty_result

and type_fun_pattern pats = match pats with
  | [] ->
      fatal_error "Typing.type_fun_patterns"   (* should not happen *)
  | [id] ->
      let ty = new_type_var () in
      ty, [id, trivial_scheme ty]
  | ids ->
      let tys = List.map (function id -> new_type_var ()) ids in
      type_product tys, List.map2 (fun id ty -> id, trivial_scheme ty) ids tys

(* RULE TE,VE |- Exp => tau *)

(* Note 2015-05-29, JS. The variable environment [VE] is required to handle cast expressions. *)

and type_expression tenv venv expr = 
  let type_expr e = match expr.e_desc with
    EConst c -> 
      type_of_constant tenv expr.e_loc c
  | EVar id ->
      type_instance (lookup_value tenv expr.e_loc id)
  | EAttr(id,attr) ->
      type_attribute expr attr
  | ECon (cid, []) ->
      let c = lookup_ctor tenv expr.e_loc cid in
      if c.cs_arity = 0 then
        type_instance { ts_tparams=fst c.cs_params; ts_sparams=snd c.cs_params; ts_body=c.cs_res }
      else
        non_constant_constr_err c expr.e_loc
  | ECon (cid, [arg]) ->
      let c = lookup_ctor tenv expr.e_loc cid in
      if c.cs_arity = 0 then
        constant_constr_err c expr.e_loc
      else
        let ty_arg = type_expression tenv venv arg in
        let ty_res = new_type_var () in
        let t = type_instance { ts_tparams=fst c.cs_params; ts_sparams=snd c.cs_params; ts_body=type_arrow c.cs_arg c.cs_res } in
        try_unify "expression" t (type_arrow ty_arg ty_res) (ELoc expr.e_loc);
        ty_res
  | ECon (cid, args) ->
      let c = lookup_ctor tenv expr.e_loc cid in
      if c.cs_arity = 0 then
        constant_constr_err c expr.e_loc
      else
        let ty_arg = type_product (List.map (type_expression tenv venv) args) in
        let ty_res = new_type_var () in
        let t = type_instance { ts_tparams=fst c.cs_params; ts_sparams=snd c.cs_params; ts_body=type_arrow c.cs_arg c.cs_res } in
        try_unify "expression" t (type_arrow ty_arg ty_res) (ELoc expr.e_loc);
        ty_res
  | ETuple es ->
      type_product (List.map (type_expression tenv venv) es)
  | ECond (e1,e2,e3) ->
      let ty_e1 = type_expression tenv venv e1 in
      let ty_e2 = type_expression tenv venv e2 in
      let ty_e3 = type_expression tenv venv e3 in
      try_unify "expression" ty_e1 type_bool (ELoc e1.e_loc);
      try_unify "expression" ty_e2 ty_e3 (ELoc expr.e_loc);
      ty_e2
  | ELet([id,exp], body) ->
      let ty_id = new_type_var () in
      let ty_exp = type_expression tenv venv exp in
      try_unify "let-definition" ty_id ty_exp (ELoc expr.e_loc);
      let tenv' = { tenv with te_values = (id, generalize tenv.te_values ty_id) :: tenv.te_values }  in
      type_expression tenv' venv body
  | ELet(_, _) ->
      failwith "should not happen" (* Multiple let-defs are only introduced in the interm repr *)
  | EApp(fn, _, args) ->
      let ty_fn = type_expression tenv venv fn in
      let ty_arg = begin match args with
        [arg] -> type_expression tenv venv arg
      | args -> type_product (List.map (type_expression tenv venv) args) end in
      let ty_result = new_type_var () in
      try_unify "expression" ty_fn (type_arrow ty_arg ty_result) (ELoc expr.e_loc);
      ty_result
  | EArrayExt1 es ->
      mk_array_type expr.e_loc (type_expression tenv venv) es
  | EArrayExt2 ess ->
      mk_array_type expr.e_loc (mk_array_type expr.e_loc (type_expression tenv venv)) ess
  | EArrayExt3 esss ->
      mk_array_type expr.e_loc (mk_array_type expr.e_loc (mk_array_type expr.e_loc (type_expression tenv venv))) esss
  | EArrayCompr (ixrs,e) ->
      let tenv' =
        List.map
          (function (i,lo,hi) -> (i, trivial_scheme (type_int (new_type_var ()) (new_size_var ()))))
          ixrs in
      let ty_elem =  type_expression { tenv with te_values = tenv' @ tenv.te_values } venv e in
      let szs =
        List.map
          (function (i,lo,hi) ->
            match lo.e_desc, hi.e_desc with
              EConst Const.CInt (0,_,_), EConst Const.CInt (n,_,_) ->
                SzConst (n+1)   (* Special case when size is syntactically given *)
            | EConst Const.CInt _, EConst Const.CInt _ ->
                partial_array_comprehension expr.e_loc;
                new_size_var ()
            | _, _ ->
                new_size_var ())
          ixrs in
      List.fold_right (fun sz t -> type_array t sz) szs ty_elem 
  | EArrRead(id, [idx]) ->
      let ty_array = type_instance (lookup_value tenv expr.e_loc id) in
      let ty_index =  type_expression tenv venv idx in
      try_unify "expression" ty_index (type_unsigned (new_size_var ())) (ELoc expr.e_loc);
      let ty_result = new_type_var () in
      let sz = new_size_var () in
      try_unify "expression" ty_array (type_array ty_result sz) (ELoc expr.e_loc);
      ty_result
  | EArrRead(id, [idx1;idx2]) ->
      let ty_array = type_instance (lookup_value tenv expr.e_loc id) in
      let ty_index1 =  type_expression tenv venv idx1 in
      let ty_index2 =  type_expression tenv venv idx2 in
      try_unify "expression" ty_index1 (type_unsigned (new_size_var ())) (ELoc expr.e_loc);
      try_unify "expression" ty_index2 (type_unsigned (new_size_var ())) (ELoc expr.e_loc);
      let ty_result = new_type_var () in
      let sz1 = new_size_var () in
      let sz2 = new_size_var () in
      try_unify "expression" ty_array (type_array (type_array ty_result sz2) sz1) (ELoc expr.e_loc);
      ty_result
  | EArrRead(id, [idx1;idx2;idx3]) ->
      let ty_array = type_instance (lookup_value tenv expr.e_loc id) in
      let ty_index1 =  type_expression tenv venv idx1 in
      let ty_index2 =  type_expression tenv venv idx2 in
      let ty_index3 =  type_expression tenv venv idx3 in
      try_unify "expression" ty_index1 (type_unsigned (new_size_var ())) (ELoc expr.e_loc);
      try_unify "expression" ty_index2 (type_unsigned (new_size_var ())) (ELoc expr.e_loc);
      try_unify "expression" ty_index3 (type_unsigned (new_size_var ())) (ELoc expr.e_loc);
      let ty_result = new_type_var () in
      let sz1 = new_size_var () in
      let sz2 = new_size_var () in
      let sz3 = new_size_var () in
      try_unify "expression" ty_array (type_array (type_array (type_array ty_result sz3) sz2) sz1) (ELoc expr.e_loc);
      ty_result
  | EArrRead(_,_) -> failwith "Typing.type_expression" (* should not happen *)
  | ECast (e,te) ->
      let ty = type_expression tenv venv e in
      let _, ty' = type_of_type_expression tenv venv te in
        (* Note 2015-05-19, JS. Could we type [te] in an empty VE here ? *)
        (* Note 2015-05-29, JS. The answer is no ! For instance, when typing [e:unsigned<k>],
           where [k] is the nth parameter ...  *)
      type_cast e.e_loc ty ty';
      ty'
  | EIgnored ->
      new_type_var ()
  in
  let ty = type_expr expr in
  expr.e_typ <- ty;
  ty

and type_of_constant tenv loc = function
  | Const.CInt (v, Some Const.Signed, Some sz) -> type_signed (SzConst sz)
  | Const.CInt (v, Some Const.Unsigned, Some sz) -> type_unsigned (SzConst sz)
  | Const.CInt (v, Some Const.Signed, None) -> type_signed (new_size_var ())
  | Const.CInt (v, Some Const.Unsigned, None) -> type_unsigned (new_size_var ())
  | Const.CInt (v, None, Some sz) -> type_int (new_type_var ()) (SzConst sz)
  | Const.CInt (v, None, None) -> type_int (new_type_var ()) (new_size_var())
  | Const.CBool v -> type_bool
  | Const.CFloat v -> type_float

and type_attribute exp attr = match attr with
  "size" -> type_unsigned (new_size_var ())
| _ -> illegal_attribute exp.e_loc attr

(* RULE TE,VE |- RExp => tau *)

let type_rule_expr (tenv,venv,tvars,touts) (q,e) =
  let lookup q =
    begin try match q.q_desc with
      QOut i -> List.assoc i touts
    | QVar (v,[]) -> List.assoc v tvars 
    | QVar (a,idxs) -> 
        let ty_array = List.assoc a tvars in
        let ty_elem = new_type_var () in
        let ty_res = 
          List.fold_right 
            (fun idx t ->
              let ty_idx =  type_expression tenv venv idx in
              try_unify "qualifier" ty_idx (type_unsigned (new_size_var ())) (ELoc q.q_loc);
              let ty_sz = new_size_var () in
              type_array t ty_sz)
            idxs
            ty_elem in
        try_unify "rule expression" ty_array ty_res (ELoc e.e_loc);
        ty_elem
    | _ -> failwith "Typing.type_rule_expr: invalid qualifier"
    with Not_found ->
      failwith "Typing.type_rule_expr: cannot retrieve type from qualifier" (* should not happen *)
    end
  in
  let ty = type_expression tenv venv e in
  let ty' = lookup q in
  try_unify "rule expression" ty ty' (ELoc e.e_loc);
  ty

(* RULE |-p RPat, tau => VE *)

let rec type_top_rule_pattern (tenv,venv,tvars,tins) (tys,bindings) (q,p) = 
  let tenv' =
    { tenv with te_values =
        List.map (function id, ty -> id, Types.generalize tenv.te_values ty) tvars
      @ tenv.te_values } in
  let lookup q =
    begin try match q.q_desc with
      QIn i -> List.assoc i tins
    | QVar (v,[]) -> List.assoc v tvars 
    | QVar (a,idxs) -> 
        let ty_array = List.assoc a tvars in
        let ty_elem = new_type_var () in
        let ty_res = 
          List.fold_right 
            (fun idx t ->
              let ty_idx =  type_expression tenv' venv idx in
              try_unify "index of rule pattern" ty_idx (type_unsigned (new_size_var ())) (ELoc idx.e_loc);
              let ty_sz = new_size_var () in
              type_array t ty_sz)
            idxs
            ty_elem in
        try_unify "rule pattern" ty_array ty_res (ELoc p.rp_loc);
        ty_elem
    | _ -> failwith "Typing.type_rule_pattern: invalid qualifier"
    with Not_found ->
      failwith "Typing.type_rule_pattern: cannot retrieve type from qualifier" (* should not happen *)
    end
  in
  let ty, new_bindings = type_rule_pattern tenv p in
  let ty' = lookup q in
  try_unify "rule pattern" ty ty' (ELoc p.rp_loc);
  p.rp_typ <- ty;
  ty::tys, new_bindings @ bindings

and type_rule_pattern tenv p = match p.rp_desc with
  | RPatConst c ->
      let ty = type_of_constant tenv p.rp_loc c in
      ty, []
  | RPatVar id ->
      let ty = new_type_var () in
(*       ty, [id, generalize tenv.te_values ty] *)
      ty, [id, trivial_scheme ty]  (* Note 2015-05-27, JS. Variables in rule patterns are _never_ generalized *)
  | RPatCon (cid,[]) ->
      let c = lookup_ctor tenv p.rp_loc cid in
      if  c.cs_arity = 0 then 
        let ty = type_instance { ts_tparams=fst c.cs_params; ts_sparams=snd c.cs_params; ts_body=c.cs_res } in 
        ty, []
      else
        non_constant_constr_err c p.rp_loc
  | RPatCon (cid, ps) ->
      let c = lookup_ctor tenv p.rp_loc cid in
      if  c.cs_arity = 0 then
        constant_constr_err c p.rp_loc
      else
        let ty_args, bindings =
          List.fold_left 
            (fun (tys,bs) p -> let ty, bs' = type_rule_pattern tenv p in tys@[ty], bs@bs')
            ([],[])
            ps in
        let ty_arg = match ty_args with
          [] -> failwith "Typing.type_rule_pattern"  (* should not happen *)
        | [t] -> t
        | ts -> type_product ts in
        let ty_res = new_type_var () in
        let t = type_instance { ts_tparams=fst c.cs_params; ts_sparams=snd c.cs_params; ts_body=type_arrow c.cs_arg c.cs_res } in
        try_unify "rule pattern" t (type_arrow ty_arg ty_res) (ELoc p.rp_loc);
        ty_res, bindings
  | RPatWild ->
      let ty = new_type_var () in
      ty, []

(* RULE TE,VE |- ActRule => tau *)

and type_actor_rule (tenv,venv,tparams,tvars,tins,touts) {rule_desc = lhs,grd,rhs; rule_loc = loc} =
  let generalize_env = List.map (function (id,ty) -> id, generalize tenv.te_values ty) in
  let ty_pat, bindings = List.fold_left (type_top_rule_pattern (tenv,venv,tvars,tins)) ([],[]) lhs.rlhs_desc in
  let tyl = type_product (List.rev ty_pat) in
  let augmented_env =
    { tenv with te_values = bindings @ generalize_env tparams @ generalize_env tvars @ tenv.te_values } in
  let ty_guards = List.map (type_expression augmented_env venv) grd.rgrd_desc in
  List.iter (function ty -> try_unify "rule guard" ty type_bool (ELoc grd.rgrd_loc)) ty_guards;
  let ty_exp = List.map (type_rule_expr (augmented_env,venv,tvars,touts)) rhs.rrhs_desc in
  let tyr = type_product ty_exp in
  type_arrow tyl tyr


(* RULE TE |- ActorDecl => TE' *)

let rec type_actor_decl tenv d =
   let a_id = d.act_desc.a_id in
   let t_params, venv = type_actor_params tenv empty_venv d.act_desc.a_params in
   let t_ins, venv' = type_actor_ios tenv venv d.act_desc.a_ins in
   let t_outs, venv'' = type_actor_ios tenv venv' d.act_desc.a_outs in
   let local_tycons, local_ctors, t_vars, venv''' = 
     type_actor_vars { tenv with te_values = trivial_env t_params @ tenv.te_values } venv d.act_desc.a_vars in
   let tenv' = augment_tenv local_tycons local_ctors [] [] tenv in
   let t_rules = List.map (type_actor_rule (tenv',venv''',t_params,t_vars,t_ins,t_outs)) d.act_desc.a_rules in
   let t_p = normalize_io_type t_params in
   let t_i = normalize_io_type t_ins in
   let t_o = normalize_io_type t_outs in
   let t_v = normalize_io_type t_vars in
   let t = match t_params with
       [] -> type_arrow t_i t_o
     | _  -> type_arrow2 t_p t_i t_o in
   let t' = match t_params, t_vars with
       [], [] -> type_arrow t_i t_o
     |  _, [] -> type_arrow2 t_p t_i t_o
     | [],  _ -> type_arrow t_i (type_pair t_o t_v)
     |  _,  _ -> type_arrow2 t_p t_i (type_pair t_o t_v) in
   (a_id,
    { at_types = local_tycons;
      at_ins = t_ins;
      at_outs = t_outs;
      at_sig = generalize tenv.te_values t;
      at_fullsig = generalize tenv.te_values t';
      at_params = t_params;
      at_vars = t_vars;
      at_rules = t_rules
    })

and normalize_io_type =  function
      [] -> type_unit
    | [t] -> snd t
    | ts -> type_product (List.map snd ts)

(* RULE TE,VE |- ActIOs => tau,VE' *)

and type_actor_ios tenv venv ios =
  List.fold_left
    (fun (tys,ve) {aio_desc=id,texp; aio_loc=loc} ->
      let ve',ty = type_of_type_expression tenv ve texp in
      tys@[id,ty], ve')
    ([],venv)
    ios

(* RULE TE,VE |- ActParams => tau,VE' *)

and type_actor_params tenv venv params =
  Misc.foldl_index
    (fun i (tys,ve) { param_desc=id,texp; param_loc=loc } ->
      let ve',ty = type_of_type_expression tenv ve texp in
      let ve'' = { ve' with ve_srefs = (id,i+1)::ve'.ve_srefs } in
      tys@[id,ty], ve'')
    ([],venv)
    params

and type_actor_vars tenv venv var_decls =
  List.fold_left (type_actor_var tenv) ([],[],[],venv) var_decls

and type_actor_var tenv (tycons,ctors,vals,venv) { var_desc=id,tyexp,iv; var_loc=loc } =
  let venv', t, new_tycons, new_ctors = type_of_full_type_expression tenv venv tyexp in
(*   check_type "local variable" (is_var_type) tenv t loc; *)
  begin match iv with
      None -> ()
    | Some exp ->
        let tenv' = augment_tenv new_tycons new_ctors [] [] tenv in
        let t' = type_expression tenv' venv' exp in
        try_unify "local variable" t t' (ELoc exp.e_loc)
  end;
(*         check_type "variable init value" (is_var_type) tenv' t (ELoc exp.e_loc) end; *)
  tycons@new_tycons, ctors@new_ctors, vals@[id,t], venv'



(*** NETWORK LEVEL ***)

(* RULE |-p NPat,tau => VE *)

let rec type_net_pattern p = 
  let type_pat p = match p.np_desc with
  | NPat_var id ->
      let ty = new_type_var () in
      ty, [id, trivial_scheme ty]
  | NPat_tuple ps ->
      let env', tys =
        List.fold_left 
          (fun (env,ts) p -> let t, env' = type_net_pattern p in (env@env', t::ts))
          ([], [])
          ps in
      (type_product (List.rev tys), env')
  in
  let ty, env' = type_pat p in
  p.np_typ <- ty;
  ty, env'

(* RULE TE [,VE] |- NExp => tau *)

and type_network_expr tenv expr = 
  let type_array_const type_of_item tenv loc vs = match vs with
    v::vs' -> 
      let t = type_of_item tenv loc v in
      List.iter
        (function v' -> 
          let t' = type_of_item tenv loc v' in
           try_unify "array" t t' (ELoc loc))
        vs'; 
      type_array t (SzConst (List.length vs))
  | _ -> failwith "Typing.type_network_expr" (* should not happen *) in
  let type_expr e = match e.ne_desc with
  | NVar id ->
      begin try type_instance (List.assoc id tenv.te_values)
      with Not_found -> unbound_value_err id expr.ne_loc end
  | NConst c -> 
      type_of_constant tenv expr.ne_loc c
  | NArray1Const [] ->
      empty_array expr.ne_loc
  | NArray1Const vs ->
      type_array_const type_of_constant tenv expr.ne_loc vs
  | NArray2Const [] ->
      empty_array expr.ne_loc
  | NArray2Const vss ->
      if List.for_all (function [] -> false | _ -> true) vss
      then type_array_const (type_array_const type_of_constant) tenv expr.ne_loc vss
      else empty_array expr.ne_loc
  | NArray3Const [] ->
      empty_array expr.ne_loc
  | NArray3Const vsss ->
      if List.for_all (function [] -> false | _ -> true) vsss
      && List.for_all (List.for_all (function [] -> false | _ -> true)) vsss
      then type_array_const (type_array_const (type_array_const type_of_constant)) tenv expr.ne_loc vsss
      else empty_array expr.ne_loc
  | NArrayItem (id,[idx]) ->
      let ty_array = type_instance (lookup_value tenv expr.ne_loc id) in
      let ty_index =  type_network_expr tenv idx in
      try_unify "expression" ty_index (type_unsigned (new_size_var ())) (ELoc expr.ne_loc);
      let ty_result = new_type_var () in
      let sz = new_size_var () in
      try_unify "expression" ty_array (type_array ty_result sz) (ELoc expr.ne_loc);
      ty_result
  | NArrayItem (id,[idx1;idx2]) ->
      let ty_array = type_instance (lookup_value tenv expr.ne_loc id) in
      let ty_index1 =  type_network_expr tenv idx1 in
      let ty_index2 =  type_network_expr tenv idx2 in
      try_unify "expression" ty_index1 (type_unsigned (new_size_var ())) (ELoc expr.ne_loc);
      try_unify "expression" ty_index2 (type_unsigned (new_size_var ())) (ELoc expr.ne_loc);
      let ty_result = new_type_var () in
      let sz1 = new_size_var () in
      let sz2 = new_size_var () in
      try_unify "expression" ty_array (type_array (type_array ty_result sz2) sz1) (ELoc expr.ne_loc);
      ty_result
  | NArrayItem (id,[idx1;idx2;idx3]) ->
      let ty_array = type_instance (lookup_value tenv expr.ne_loc id) in
      let ty_index1 =  type_network_expr tenv idx1 in
      let ty_index2 =  type_network_expr tenv idx2 in
      let ty_index3 =  type_network_expr tenv idx3 in
      try_unify "expression" ty_index1 (type_unsigned (new_size_var ())) (ELoc expr.ne_loc);
      try_unify "expression" ty_index2 (type_unsigned (new_size_var ())) (ELoc expr.ne_loc);
      try_unify "expression" ty_index3 (type_unsigned (new_size_var ())) (ELoc expr.ne_loc);
      let ty_result = new_type_var () in
      let sz1 = new_size_var () in
      let sz2 = new_size_var () in
      let sz3 = new_size_var () in
      try_unify "expression" ty_array (type_array (type_array ty_result sz2) sz1) (ELoc expr.ne_loc);
      try_unify "expression" ty_array (type_array (type_array (type_array ty_result sz3) sz2) sz1) (ELoc expr.ne_loc);
      ty_result
  | NArrayItem (_, _) ->
      failwith "Typing.type_network_expr"  (* should not happen *)
  | NTuple es -> type_product (List.map (type_network_expr tenv) es)
  | NApp (fn, arg) ->
      let ty_fn = type_network_expr tenv fn in
      let ty_arg = type_network_expr tenv arg in
      let ty_result = new_type_var () in
      try_unify "expression" ty_fn (type_arrow ty_arg ty_result) (ELoc expr.ne_loc);
      begin match Types.refs_of ty_result with
        [] -> ty_result
      | _ -> 
          let renv =
            begin match Ssval.eval_sv arg with
            | Ssval.SVTuple vs ->
                Misc.foldl_index
                  (fun i r v -> match v with
                  | Ssval.SVVal (Expr.Val_int (n,_)) -> (i+1,n)::r
                  | _ -> r)
                  []
                  vs
            | Ssval.SVVal (Expr.Val_int (n,_)) ->
                [1, n]
            | _ ->
                illegal_static_value "int" arg.ne_loc
            end in
          Types.deref renv ty_result
      end
  | NFun (npat,nexp) ->
      let ty_argument = new_type_var ()
      and ty_result = new_type_var () in
      let (ty_pat, augmented_env) = type_net_pattern npat in
      try_unify "pattern" ty_pat ty_argument (ELoc npat.np_loc);
      let ty_expr = type_network_expr { tenv with te_values = augmented_env @ tenv.te_values } nexp in
      try_unify "expression" ty_expr ty_result (ELoc nexp.ne_loc);
      type_arrow ty_argument ty_result
  | NLet (rec_flag, bindings, body) ->
      let augmented_env = type_definitions rec_flag tenv bindings in
      type_network_expr { tenv with te_values = augmented_env @ tenv.te_values } body
  | NCast (e,te) ->
      let ty = type_network_expr tenv e in
      let _, ty' = type_of_type_expression tenv empty_venv te in
        (* Note 2015-05-19, JS. Should we really type [te] in an empty VE here ? *)
      type_cast e.ne_loc ty ty';
      ty' in
  let ty = type_expr expr in
  expr.ne_typ <- ty;
  update_tc_insts tenv.te_types expr.ne_loc ty;
  ty

and type_definitions is_rec tenv defns =
  let ty_pats, aug_envs =
    List.split
      (List.map
        (function { nb_desc=(npat,nexp); nb_loc=loc } -> type_net_pattern npat)
        defns) in
  let tenv' =
    if is_rec
    then { tenv with te_values = List.concat aug_envs @ tenv.te_values }
    else tenv in
  let ty_exps = List.map
      (function { nb_desc = (npat,nexp); nb_loc=loc } ->
        type_network_expr tenv' nexp, loc)
      defns in
  List.iter2
    (fun ty1 (ty2,loc) -> try_unify (if is_rec then "recursive definition" else "definition") ty1 ty2 (ELoc loc))
    ty_pats ty_exps;
  let tenv'' = List.flatten
      (List.map2
         (fun {nb_desc=(npat,_); nb_loc=loc} (texp,loc) -> 
             extract_type_bindings loc tenv' npat (real_type texp))
         defns
         ty_exps) in
  tenv''

(* Rule TE |-p npat, tau => TE' *)

and extract_type_bindings loc tenv pat ty = match pat.np_desc, real_type ty with
  | NPat_var id, ty ->
      [id, generalize tenv.te_values ty]
  | NPat_tuple ps, Tproduct ts ->
      List.flatten (List.map2 (extract_type_bindings loc tenv) ps ts)
  | NPat_tuple ps, Tconstr ({tc_name="bundle"}, [ty'], [sz]) ->
      List.flatten (List.map (function p -> extract_type_bindings p.np_loc tenv p ty') ps)
  | _, _ -> illegal_definition loc

(* RULE TE |- IoDecl => TEi,TEo *)

let type_io_decl tenv (ie,oe) d =
  let type_of ty = 
    (* Note 2011-11-17, JS : types of ios are _not_ generalized ! *)
    let _, t = type_of_type_expression tenv empty_venv ty
    in t in
  let id, t, dir = match d.io_desc with 
    (StreamIO, id, ty, dir, _, _) -> id, type_of ty, dir
  | (PortIO, id, ty, dir, _, None) -> id, type_of ty, dir
  | (PortIO, id, ty, dir, _, Some exp) ->
      let t = type_of ty in
      let t' = type_expression tenv empty_venv exp in
      type_cast d.io_loc t' t;
      id, t, dir in 
  update_tc_insts tenv.te_types d.io_loc t;
  match dir with
    IoIn -> (id, t)::ie, oe
  | IoOut -> ie, (id, t)::oe

(* RULE TE |- NDecl => TE' *)

let type_netw_decl tenv d = match d.nd_desc with
  (rec_flag, bindings) -> type_definitions rec_flag tenv bindings

(* RULE TE |- Program => TE' *)

let rec type_program tyenv p = 
  let tenv = List.fold_left type_type_decl tyenv p.types in
  let tenv',te_v = List.fold_left type_val_decl (tenv,[]) p.vals in
  let te_a = List.map (type_actor_decl { tenv' with te_values = te_v @ tenv'.te_values }) p.actors in
  let te_i,te_o = List.fold_left (type_io_decl tenv') ([],[]) p.ios in
  let tenv'' = 
    { tenv' with te_values =
        tenv'.te_values
      @ te_v
      @ List.map (function (id,a) -> id,a.at_sig) te_a
      @ List.map (function (id,ty) -> id, trivial_scheme ty) te_i } in
  the_typing_env := tenv'';
  let te_n =
    List.fold_left
      (fun env d ->
        let env' = type_netw_decl { tenv'' with te_values = tenv''.te_values @ env } d in
        env @ env')
      []
      p.nets in
  let te_n' =  List.map (unify_io te_i te_o) te_n in
  { tp_types = tenv.te_types;
    tp_ctors = tenv.te_ctors;
    tp_vals = (* tenv.te_values @ *) te_v;
    tp_actors = te_a;
    tp_ios = te_i @ te_o;
    tp_nets = te_n' }

and unify_io ienv oenv (id,ty) =
  let ty' = type_instance ty in
  (* Note 2015-06-11, JS. Unification with a IO turns a polymorphic type into a monomorphic one .. *)
  if List.mem_assoc id ienv then
    try_unify "input" (List.assoc id ienv) ty' (EItem id)
  else if List.mem_assoc id oenv then
    try_unify "output" (List.assoc id oenv) ty' (EItem id);
  id, trivial_scheme ty'

(* Misc *)

let function_types where fid t = match real_type t with
|  Tarrow ((Tproduct ts as t),t') -> t, ts, t'
|  Tarrow (t,t') -> t, [t], t'
| _ -> Error.illegal_function_type where fid t

let is_variant_type name =
  try
    match (List.assoc name !the_typing_env.te_types).tc_defn with
      { ty_desc = Variant_type _ } -> true
    | _ -> false
  with
    Not_found -> false

let find_type_constr id = List.assoc id !the_typing_env.te_types

(* Printing *)

let rec print_typed_program tp =
  printf "Typed program ---------------\n";
  List.iter print_type tp.tp_types;
  List.iter print_typed_val tp.tp_vals;
  List.iter print_typed_actor tp.tp_actors;
  List.iter print_typed_io tp.tp_ios;
  List.iter (print_value "net") tp.tp_nets;
  printf "----------------------------------\n"

and print_type (name, td) =
  match td.tc_defn.ty_desc with 
  | Abstract_type -> printf "type %s = <abstract>\n" td.tc_defn.ty_constr.tc_name
  | Abbrev_type ty -> printf "type %s == %s\n" td.tc_defn.ty_constr.tc_name (Pr_type.string_of_one_type ty)
  | Variant_type (tvs, svs, cds) -> 
      Pr_type.TypeVarNames.reset ();
      Pr_type.SizeVarNames.reset ();
      printf "type %s %s = %s\n" 
        (string_of_type_params (tvs,svs))
        td.tc_defn.ty_constr.tc_name
        (string_of_variant_ctors cds);
      List.iter print_type_inst (TyconInsts.to_list td.tc_insts)

and string_of_variant_ctors cds = Misc.string_of_list string_of_variant_ctor " | " cds

and string_of_type_params = function
    ([],[]) -> ""
  | ([],[sv]) -> string_of_size_var sv
  | ([tv],[]) -> string_of_type_var tv
  | tvs,svs ->  "(" ^ Misc.string_of_two_lists string_of_type_var string_of_size_var "," tvs svs ^ ")"

and string_of_type_var v = "'" ^ Pr_type.TypeVarNames.name_of v
and string_of_size_var v = "#" ^ Pr_type.SizeVarNames.name_of v

and print_type_inst (_,(td,loc)) =
  match td.ty_desc with 
    Variant_type (_,_,cds) ->
      Printf.printf "  instanciated as %s @ %s\n"
(*     (Misc.string_of_two_lists string_of_typ_var_inst string_of_siz_var_inst "," tvbs svbs) *)
        (string_of_variant_ctors cds)
        (Misc.string_of_list Location.string_of_location "," loc)
  | _ ->
      failwith "Typing.print_type_inst"  (* should not happen *)

and string_of_typ_var_inst (tv,ty) = "'" ^ Pr_type.TypeVarNames.name_of tv ^ "=" ^ string_of_type ty
and string_of_siz_var_inst (sv,sz) = "#" ^ Pr_type.SizeVarNames.name_of sv ^ "=" ^ string_of_size sz

and string_of_variant_ctor c =
  match c.cs_arity with
    0 -> c.cs_name
  | _ -> c.cs_name ^ " of " ^ string_of_type c.cs_arg

and print_typed_val (name, ty) =
  Pr_type.TypeVarNames.reset ();
  Pr_type.SizeVarNames.reset ();
   printf "val %s : %s\n" name (string_of_type_scheme ty);
   flush stdout

and print_typed_actor (name, a) =
  Pr_type.TypeVarNames.reset ();
  Pr_type.SizeVarNames.reset ();
  printf "actor %s : %s\n" name (string_of_type_scheme a.at_fullsig);
  flush stdout

and print_typed_io (name, ty) =
   printf "io %s : %s\n" name (string_of_one_type ty);
   flush stdout

and print_value tag (name, ty_sch) =
  Pr_type.TypeVarNames.reset ();
  Pr_type.SizeVarNames.reset ();
   printf "%s %s : %s\n" tag name (string_of_type_scheme ty_sch);
   flush stdout

let print_typing_environment te = 
  printf "Typing environment ---------------\n";
  List.iter print_type te.te_types;
(*   List.iter print_ctor te.te_ctors; *)
  List.iter (print_value "val") te.te_values;
  printf "----------------------------------\n"
