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

(* Basic definitions and manipulation fns on types *)
(* The core mechanism is largely inspired from the Caml Light implementation *)

open Misc

(* Type constructors *)

type type_constr =
  { tc_name: string;
    mutable tc_abbr: type_abbrev }      (* Abbreviation or not *)

and type_abbrev =
    Tnotabbrev
  | Tabbrev of typ

(* Type expressions *)

and typ =
   | Tvar of typ var
   | Tarrow of typ * typ
   | Tproduct of typ list         
   | Tconstr of type_constr * typ list * siz list

and siz = 
   | SzConst of int
   | SzVar of siz var
   | SzRef of int

and 'a var =
  { stamp: string;             (* for debug only *)
    mutable value: 'a value }

and 'a value =
  | Unknown
  | Known of 'a

type typ_scheme =
  { ts_tparams: (typ var) list;
    ts_sparams: (siz var) list;
    ts_body: typ }

exception TypeConflict of typ * typ
exception TypeCircularity of typ * typ

let type_arrow t1 t2 = Tarrow (t1,t2)      (* Wrappers, for backward compatibility *)
let type_arrow2 t1 t2 t3 = Tarrow (t1,Tarrow (t2,t3))
let type_arrow3 t1 t2 t3 t4 = Tarrow (t1,Tarrow (t2, Tarrow (t3,t4)))
let type_product ts = Tproduct ts
let type_pair t1 t2 = type_product [t1;t2]
let type_cstr tc ts ss = Tconstr (tc, ts, ss)
let no_type = Tproduct []
let no_scheme = { ts_tparams=[]; ts_sparams=[]; ts_body=no_type }

let constr_type_int = { tc_name="int"; tc_abbr=Tnotabbrev }
let constr_type_bool = { tc_name="bool"; tc_abbr=Tnotabbrev }
let constr_type_unit = { tc_name="unit"; tc_abbr=Tnotabbrev }
let constr_type_float = { tc_name="float"; tc_abbr=Tnotabbrev }
let constr_type_array = { tc_name="array"; tc_abbr=Tnotabbrev }
let constr_type_bundle = { tc_name="bundle"; tc_abbr=Tnotabbrev }
let constr_type_signed = { tc_name="_signed"; tc_abbr=Tnotabbrev }  (* Phantom ctor *)
let constr_type_unsigned = { tc_name="_unsigned"; tc_abbr=Tnotabbrev }  (* Phantom ctor *)

(* Type constructor descriptions *)

type type_desc =
  { ty_constr: type_constr;             (* The constructor *)
    ty_arity: int * int;                (* Its arity (# of type and size parameters, resp.) *)
    mutable ty_desc: type_components }  (* Its description *)

and type_components =
    Abstract_type                                                    
  | Variant_type of typ var list * siz var list * constr_desc list   
  | Abbrev_type of typ                                 

and constr_desc =
  { cs_name: string;
    cs_tag: int option;                      (* Optional explicit binary encoding for VHDL code *)
    cs_arity: int;                           (* ctor arity; 0 for constant ctors *)
    cs_params: typ var list * siz var list;  (* The signature of the ctor is the type scheme [params. cs_args -> cs_res] *)
    cs_res: typ;                             (* Result type *)
    cs_arg: typ; }                           (* Argument type *)

(* Type builders *)

let type_int sg sz = Tconstr ({tc_name="int"; tc_abbr=Tnotabbrev}, [sg], [sz])
let type_unsigned sz = type_int (Tconstr ({tc_name="_unsigned"; tc_abbr=Tnotabbrev}, [], [])) sz
let type_signed sz = type_int (Tconstr ({tc_name="_signed"; tc_abbr=Tnotabbrev}, [], [])) sz
let type_range lo hi = Tconstr ({tc_name="int"; tc_abbr=Tnotabbrev}, [], [lo;hi]) 
let type_bool = Tconstr({tc_name="bool"; tc_abbr=Tnotabbrev},[],[])
let type_float = Tconstr({tc_name="float"; tc_abbr=Tnotabbrev},[],[])
let type_unit = Tconstr({tc_name="unit"; tc_abbr=Tnotabbrev},[],[])
let type_array t sz = Tconstr({tc_name="array"; tc_abbr=Tnotabbrev}, [t], [sz])
let type_bundle t sz = Tconstr({tc_name="bundle"; tc_abbr=Tnotabbrev}, [t], [sz])

(* Type and size variable manipulation *)

let var_cnt = ref 0   (* for debug only *)
let new_stamp () =
  let pfx = "v" in
  incr var_cnt; pfx ^ string_of_int !var_cnt

let mk_type_var () = { value = Unknown; stamp=new_stamp () }
let new_type_var () = Tvar (mk_type_var ())

let mk_size_var () = { value = Unknown; stamp=new_stamp () }
let new_size_var () = SzVar (mk_size_var ())

(* Canonical representation of a type +  path compression *)

let rec type_repr = function
  | Tvar ({value = Known ty1} as var) ->
      let ty = type_repr ty1 in
      var.value <- Known ty;
      ty
  | ty -> ty

let rec size_repr = function
  | SzVar ({value = Known sz1} as var) ->
      let sz = size_repr sz1 in
      var.value <- Known sz;
      sz
  | sz -> sz

(* Real type : path compression + unabbreviation *)

let rec real_type ty = 
  match type_repr ty with
    Tconstr ({tc_abbr=Tabbrev ty'}, _, _) -> real_type ty'
  | Tarrow (ty1,ty2) -> Tarrow (real_type ty1, real_type ty2)
  | Tproduct ts  -> Tproduct (List.map real_type ts)        
  | Tvar { value=Known ty'} -> ty'
  | ty -> ty

let rec real_size sz = 
  match size_repr sz with
  | SzVar { value=Known sz'} -> sz'
  | sz -> sz

(* Occur check *)

let occur_check var ty =
  let rec test t =
    match type_repr t with
    | Tvar var' ->
        if var == var' then raise(TypeCircularity(Tvar var, ty))
    | Tarrow (ty1,ty2) ->
        test ty1;
        test ty2
    | Tproduct ts ->
        List.iter test ts
    | Tconstr(constr, args, _) ->
        List.iter test args
  in test ty

let occur_check_size (ty1,ty2) var sz =
  let rec test s =
    match size_repr s with
    | SzVar var' ->
        if var == var' then raise(TypeCircularity(ty1,ty2))
    | _ ->
        ()
  in test sz

(* Unification *)

let rec unify ty1 ty2 =
  let val1 = real_type ty1
  and val2 = real_type ty2 in
  if val1 == val2 then () else
  match (val1, val2) with
  | Tvar v1, Tvar v2 when v1==v2 -> 
      ()
  | Tvar var, ty ->
      occur_check var ty;
      var.value <- Known ty
  | ty, Tvar var ->
      occur_check var ty;
      var.value <- Known ty
  | Tarrow(ty1, ty2), Tarrow(ty1', ty2') ->
      unify ty1 ty1';
      unify ty2 ty2'
  | Tproduct ts1, Tproduct ts2 when List.length ts1 = List.length ts2 ->
      List.iter2 unify ts1 ts2
  | Tconstr({tc_name="bundle"}, [ty1'], [sz1]), Tproduct ts2
  | Tproduct ts2, Tconstr({tc_name="bundle"}, [ty1'], [sz1]) ->
      (* Note 2014-05-14, JS. Special case for unifying bundles and tuples *)
      unify_size (val1,val2) sz1 (SzConst (List.length ts2));
      List.iter (unify ty1') ts2
  | Tconstr({tc_name="int"}, [], [lo1;hi1]), Tconstr({tc_name="int"}, [], [lo2;hi2]) ->
      (* Note 2017-07-11, JS. If both integers have explicit ranges, these ranges must be equal *)
      unify_size (val1,val2) lo1 lo2;
      unify_size (val1,val2) hi1 hi2
  | Tconstr({tc_name="int"}, [], [lo1;hi1]), Tconstr({tc_name="int"}, sg2, sz2) ->
      (* Note 2017-07-11, JS. A ranged integer has no explicit sign nor size. *)
      ()
  | Tconstr({tc_name="int"}, sg2, sz2), Tconstr({tc_name="int"}, [], [lo1;hi1]) ->
      (* Note 2017-07-11, JS. A ranged integer has no explicit sign nor size. *)
      ()
  | Tconstr(constr1, arguments1, sizes1), Tconstr(constr2, arguments2, sizes2)
    when constr1=constr2
        && List.length arguments1 = List.length arguments2
        && List.length sizes1 = List.length sizes2 ->
      List.iter2 unify arguments1 arguments2;
      List.iter2 (unify_size (val1,val2)) sizes1 sizes2
  | _, _ ->
      raise (TypeConflict(val1, val2))

and unify_size (ty1,ty2) sz1 sz2 =
  let val1 = real_size sz1
  and val2 = real_size sz2 in
  if val1 == val2 then
    ()
  else
  match (val1, val2) with
    | SzVar var1, SzVar var2 when var1 == var2 ->  (* This is hack *)
        ()
    | SzVar var, sz ->
        occur_check_size (ty1,ty2) var sz;
        var.value <- Known sz
    | sz, SzVar var ->
        occur_check_size (ty1,ty2) var sz;
        var.value <- Known sz
    | SzConst n1, SzConst n2 when n1 = n2 ->
        ()
    | SzRef k1, SzRef k2 when k1=k2 ->
        ()
    | _, _ ->
        raise (TypeConflict(ty1, ty2))

let generalize env ty =
  (* Note : we use here a naive version in which generic variables are detected by
     simply checking whether they do not occur free in the englobing typing environment.
     A more efficient version would use binding levels, of course *)
  let vars_of (tvars',svars') ty = 
     (* Returns the list of type (resp. size) variables occuring in [t] but not in [tvars'] (resp. [svars']) *)
    let tvars = ref [] in
    let svars = ref [] in
    let rec scan_ty t =
      match type_repr t with
      | Tvar var ->
          if not (List.memq var !tvars) && not (List.memq var tvars') 
          then tvars := var :: !tvars
      | Tarrow (t1, t2) ->
          scan_ty t1;
          scan_ty t2
      | Tproduct ts ->
          List.iter scan_ty ts
      | Tconstr (constr, arguments, sizes) ->
          List.iter scan_ty arguments;
          List.iter scan_sz sizes
    and scan_sz sz =
      match size_repr sz with
      | SzVar var ->
          if not (List.memq var !svars) && not (List.memq var svars') 
          then svars := var :: !svars
      | SzConst _ ->
          ()
      | SzRef _ ->
          ()
    in
    scan_ty ty;
    !tvars,!svars in
  let free_tvars, free_svars = 
    List.fold_left
      (fun (tvs,svs) (_,ts) -> 
        let tvs',svs' = vars_of (ts.ts_tparams,ts.ts_sparams) ts.ts_body in
        tvs @ tvs', svs @ svs')
      ([],[])
      env in
  let gen_tvars, gen_svars = vars_of (free_tvars,free_svars) ty in
  {ts_tparams = List.rev gen_tvars; ts_sparams = List.rev gen_svars; ts_body = ty}

let trivial_scheme ty = { ts_tparams=[]; ts_sparams=[]; ts_body=ty }

(* Type instanciation *)

let rec copy_type tvbs svbs ty =
  let rec copy ty = 
    match type_repr ty with
    | Tvar var as ty ->
        begin try
          List.assq var tvbs
        with Not_found ->
            ty
        end
    | Tarrow (ty1, ty2) ->
        Tarrow (copy ty1, copy ty2)
    | Tproduct ts ->
        Tproduct (List.map copy ts)
    | Tconstr (constr, arguments, sizes) as ty ->
        Tconstr (constr, List.map copy arguments, List.map (copy_size ty svbs) sizes) in
  copy ty

and copy_size ty svbs sz =
  match size_repr sz with
  | SzVar var as sz ->
      begin try
        List.assq var svbs 
      with Not_found ->
        sz
      end
  | SzRef _ as sz ->
      sz
  | SzConst _ as sz ->
      sz

let full_type_instance ty_sch =
  match ty_sch.ts_tparams, ty_sch.ts_sparams with
  | [], [] -> ty_sch.ts_body, [], []
  | tparams, sparams ->
      let unknown_ts = List.map (fun var -> (var, new_type_var())) tparams in
      let unknown_ss = List.map (fun var -> (var, new_size_var())) sparams in
      copy_type unknown_ts unknown_ss ty_sch.ts_body,
      unknown_ts,
      unknown_ss

let type_instance ty_sch = Misc.fst3 (full_type_instance ty_sch)

let type_copy t = type_instance (generalize [] t)  (* tofix ? *)

(* Type and size var bindings *)

let rec type_equal t1 t2 =
  match real_type t1, real_type t2 with
  | Tvar { stamp=s1; value=Unknown }, Tvar { stamp=s2; value=Unknown } ->
      s1 = s2
  | Tarrow (ty1, ty1'), Tarrow (ty2, ty2') ->
      type_equal ty1 ty2 && type_equal ty1' ty2'
  | Tproduct ts, Tproduct ts' when List.length ts = List.length ts'->
      List.for_all2 type_equal ts ts'
  | Tconstr ({tc_name="int"}, [sg1], [sz1]), Tconstr({tc_name="int"}, [sg2], [sz2]) -> (* Special case ..*)
      begin match real_type sg1, size_repr sz1, real_type sg2, size_repr sz2 with
      | Tconstr ({tc_name=s1},[],[]), SzConst n1, Tconstr({tc_name=s2},[],[]), SzConst n2 -> s1=s2 && n1=n2 (* [un]signed<n> = [un]signed<n> *)
      | Tconstr ({tc_name=s1},[],[]), SzRef n1, Tconstr({tc_name=s2},[],[]), SzRef n2 -> s1=s2 && n1=n2 (* [un]signed<@n> = [un]signed<@n> *)
      | Tconstr ({tc_name=s1},[],[]), SzVar _, Tconstr({tc_name=s2},[],[]), SzVar _ -> s1=s2   (* [un]signed<?> = [un]signed<?> *)
      | Tconstr ({tc_name="_signed"},[],[]), SzVar _, Tvar _, SzVar _ -> true  (* signed<?> = int *)  (* Added v2.6.2 *)
      | Tvar _, SzVar _, Tconstr ({tc_name="_signed"},[],[]), SzVar _ -> true  (* int = signed<?> *)  (* Added v2.6.2 *)
      | Tvar _, SzConst n1, Tvar _, SzConst n2 -> n1=n2  (* int<n> = int<n> *)  (* Added v2.8.4 *)
      | Tvar _, SzVar _, Tvar _, SzVar _ -> true (* int<?> = int<?> *)
      | _, _, _, _ -> false 
      end
  | Tconstr (cstr1, args1, sizes1), Tconstr(cstr2, args2, sizes2)
      when List.length args1 = List.length args2 && List.length sizes1 = List.length sizes2 ->
      cstr1 = cstr2 && List.for_all2 type_equal args1 args2 && List.for_all2 size_equal sizes1 sizes2
  | _, _ ->  (* TO FIX ? *)
      false

and size_equal t1 t2 =
  match size_repr t1, size_repr t2 with
  | SzVar { stamp=s1; value=Unknown }, SzVar { stamp=s2; value=Unknown } ->
      s1 = s2
  | SzConst n1, SzConst n2 -> 
      n1 = n2
  | SzRef n1, SzRef n2 -> 
      n1 = n2
  | _, _ ->
      false

type 'a var_bind = 'a var * 'a

(* A specialized module for keeping track of instanciation *)

module TypeInsts =
  Itbl.Make
    (struct
      type t = typ
      let equal t1 t2 = type_equal t1 t2
    end)

(* Type size references *)

type szref_env = (int * int) list

let refs_of ty =
  let refs = ref [] in 
  let rec find t = match type_repr t with
  | Tvar _ ->
      ()
  | Tarrow (ty1, ty2) ->
      find ty1;
      find ty2
  | Tproduct ts ->
      List.iter find ts
  | Tconstr (cstr, args, sizes) ->
      List.iter find_sz sizes;
      List.iter find args
  and find_sz sz = match size_repr sz with
    SzRef m -> refs := m :: !refs
  | _ -> () 
  in find ty;
  !refs

let deref renv ty =
  (* Replace, in [ty], all occurences of [@k] by [s] whenever [k] is bound to [s] in [renv] *)
  let rec subst_ty t = match type_repr t with
    | Tconstr (cstr, args, sizes) ->
        Tconstr (cstr, List.map subst_ty args, List.map subst_sz sizes)
    | Tproduct ts ->
        Tproduct (List.map  subst_ty ts)
    | Tarrow (ty1,ty2) ->
        Tarrow (subst_ty ty1, subst_ty ty2)
     | ty ->
         ty
  and subst_sz sz = match size_repr sz with
  | SzRef k when List.mem_assoc k renv ->
      SzConst (List.assoc k renv)
  | sz -> sz 
  in subst_ty ty

(* Type refinement (for ints) *)

type refine_rel = 
    RR_Less   (* less "precise" *)
  | RR_Diff   (* same "precision" but different *) 
  | RR_Equal  (* fully equal *)
  | RR_Greater (* more "precise" *)

let compare_int_signness sg1 sg2 =
  match real_type sg1, real_type sg2 with
    Tconstr({tc_name=s1},[],[]), Tconstr({tc_name=s2},[],[]) -> if s1=s2 then RR_Equal else RR_Diff
  | Tconstr({tc_name=s1},[],[]), Tvar _ -> RR_Greater
  | Tvar v1 , Tvar v2 -> if v1==v2 then RR_Equal else RR_Diff
  | Tvar _, _ -> RR_Less
  | _, _ -> failwith "Types.compare_int_signness" (* should not happen *)

let compare_int_size sz1 sz2 =
  match size_repr sz1, size_repr sz2 with
    SzConst n1, SzConst n2 -> if n1=n2 then RR_Equal else RR_Diff
  | SzConst n1, SzRef r2 -> RR_Diff (* We cannot tell at the typing level.. Let's assume the corresp values are diff. *)
  | SzConst n1, SzVar _ -> RR_Greater
  | SzRef r1, SzConst n2 -> RR_Diff
  | SzRef r1, SzRef r2 -> RR_Diff
  | SzRef r1, SzVar r2 -> RR_Greater
  | SzVar v1, SzVar v2 -> if v1==v2 then RR_Equal else RR_Diff
  | SzVar v1, _ -> RR_Less

(* Inspectors  *)

let arrow_types t = match real_type t with
  Tarrow (t1,t2) -> t1, t2
| _ -> raise (Invalid_argument "arrow_types")

let prod_types t = match real_type t with
  Tproduct ts -> ts
| _ -> raise (Invalid_argument "prod_types")

let is_constr_type p t = match real_type t with
  | Tconstr ({tc_name=name},_,_) -> p name 
  | _ -> false

let is_int_type = is_constr_type (function n -> n="int")
let is_bool_type = is_constr_type (function n -> n="bool") 
let is_float_type = is_constr_type (function n -> n="float")
let is_enum_type  = is_constr_type (function n -> String.length n >= 5 && String.sub n 0 5 = "_enum") 
let is_dc_type = is_constr_type (function n -> n="dc")
let is_array_type = is_constr_type (function n -> n="array")

let is_simple_type ty = is_int_type ty || is_bool_type ty || is_float_type ty || is_enum_type ty

let is_array_of_simple_type t = match real_type t with
  | Tconstr ({tc_name="array"},[t'],_) -> is_simple_type t'
  | _ -> false

let is_array_of_array_of_simple_type t = match real_type t with
  | Tconstr ({tc_name="array"},[t'],_) -> is_array_of_simple_type t'
  | _ -> false

let rec array_dims t = match real_type t with
  | Tconstr ({tc_name="array"},[t1],[s1]) -> real_size s1 :: array_dims t1
  | _ -> []

let is_range_type t = match real_type t with
  | Tconstr ({tc_name="int"},[],[lo;hi]) -> true
  | _ -> false

let type_range_bounds t = match real_type t with
  | Tconstr ({tc_name="int"},[],[lo;hi]) -> real_size lo, real_size hi
  | _ -> Misc.fatal_error "Types.type_range_bounds"

let ty_constr_name t = match real_type t with
  | Tconstr ({tc_name=name},_,_) -> name 
  | _ -> Misc.fatal_error "Types.ty_constr_name"

let rec is_ground_type ?(strict=false) ty = 
    match real_type ty with
      | Tvar _ -> false
      | Tarrow (t1, t2) -> is_ground_type t1 && is_ground_type t2
      | Tproduct ts -> List.for_all is_ground_type ts
      | Tconstr ({tc_name="int"}, [sg], [sz]) -> 
          if strict
          then is_ground_type sg && is_ground_size sz
         else true
      | Tconstr (constr, arguments, sizes) -> List.for_all is_ground_type arguments || List.for_all is_ground_size sizes 

and is_ground_size sz =
    match size_repr sz with
    | SzConst _ -> true
    | SzVar _ -> false
    | SzRef _ -> false

let list_of_types ty = match real_type ty with
  Tproduct ts -> List.map real_type ts
| Tconstr({tc_name="unit"},_,_) -> []
| _ -> [real_type ty]

let type_width ty = 
  match real_type ty with
      | Tconstr ({tc_name="bool"}, [], []) -> Some (SzConst 1)
      | Tconstr ({tc_name="int"}, [sg], [sz]) -> Some (real_size sz)
      | Tconstr ({tc_name="array"},[t],[sz]) -> Some (real_size sz)
      | Tproduct ts -> Some (SzConst (List.length ts))
      | _ -> None

let array_dim t =
  (* Returns (t,[]) for a non-array type [t],
             (t,[s1]) for a type [t array[s1]],
             (t,[s1;s2]) for [t array[s1][s2]], ... *) 
  match type_repr t with 
  | Tconstr({tc_name="array"}, [ty1], [sz1]) ->
      begin match type_repr ty1, size_repr sz1 with
      | Tconstr({tc_name="array"}, [ty2], [sz2]), s1 ->
          begin match type_repr ty2, size_repr sz2 with
          | Tconstr({tc_name="array"}, [ty3], [sz3]), s2  ->  
              type_repr ty3, [s1;s2;size_repr sz3]    
          | t2, s2 ->                                      
              t2, [s1; s2]
          end
      | t1, s1 ->
          t1, [s1]
      end
  | t0 -> t0, []
