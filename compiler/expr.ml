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

(* Expression level language *)

open Syntax
open Error
open Misc
open Types
open Arrays

let abbrev_dc_ctors = ref false

(* Expression-level semantic values [v] *)

type int_precision = 
    Unsigned of int
  | Signed of int

type e_val =
   | Val_int of int * int_precision option
   | Val_bool of bool
   | Val_float of float
   | Val_con of string * e_val list (* '<, '> and Data x will be there *)
   | Val_unknown
   | Val_tuple of e_val list
   | Val_array1 of int * e_val Array1.t
   | Val_array2 of (int * int) * e_val Array2.t
   | Val_array3 of (int * int * int) * e_val Array3.t
   | Val_prim of (e_val -> e_val)
   | Val_fun of string * Syntax.ident list * Syntax.expr (* no env here; core expr funs are closed *)
   | Val_extern of ext_fn_desc 
   | Val_hprim of string * int  (* Higher-order primitives (map, fold,...) - handled specially *)
   | Val_ignored

and ext_fn_desc = {
    ef_c_name: string;
    ef_vhd_name: string;
    ef_ml_name: string;
    ef_type: Types.typ
    }   

type static_env = {
    se_vals: (string * e_val) list;
    se_ctors: (string * int) list;  (* Name, arity *)
    }

(* Misc *)

let listify_tuple_value = function
    Val_tuple vs -> vs
  | v -> [v]

let tuplify_value_list = function
    [] -> Misc.fatal_error "Expr.tuplify_value_list: empty list"
  | [v] -> v
  | vs -> Val_tuple vs

let array_get loc ar ixs =
  try
    match ar, ixs with
      Val_array1 (sz1, a), [Val_int (i1, _)] -> a.(i1)
    | Val_array2 ((sz1, sz2), a), [Val_int (i1, _)] -> Val_array1 (sz2, a.(i1))
    | Val_array2 ((sz1, sz2), a), [Val_int (i1, _); Val_int (i2,_)] -> a.(i1).(i2)
    | Val_array3 ((sz1, sz2, sz3), a), [Val_int (i1, _)] -> Val_array2 ((sz2, sz3), a.(i1))
    | Val_array3 ((sz1, sz2, sz3), a), [Val_int (i1, _); Val_int (i2,_)] -> Val_array1 (sz3, a.(i1).(i2))
    | Val_array3 ((sz1, sz2, sz3), a), [Val_int (i1, _); Val_int (i2, _); Val_int (i3, _)] -> a.(i1).(i2).(i3)
    | _ -> failwith "Expr.array_get" (* should not happen thx to type checking *)
  with Array1.Invalid_index _ | Array2.Invalid_index _ | Array3.Invalid_index _ | Invalid_argument _ ->
    (* TOFIX : all subarray access should go through ArrayN calls, so raw Invalid_argument should not occur *)
    array_index_out_of_range loc

let array_set loc ar ixs v =
  try
    match ar, ixs, v with
      Val_array1 (sz1, a), [Val_int (i1, _)], _ -> a.(i1) <- v
    | Val_array2 ((sz1, sz2), a), [Val_int (i1, _)], Val_array1 (_,v') -> a.(i1) <- v'
    | Val_array2 ((sz1, sz2), a), [Val_int (i1, _); Val_int (i2,_)], _ -> a.(i1).(i2) <- v
    | Val_array3 ((sz1, sz2, sz3), a), [Val_int (i1, _)], Val_array2 (_,v') -> a.(i1) <- v'
    | Val_array3 ((sz1, sz2, sz3), a), [Val_int (i1, _); Val_int (i2,_)], Val_array1 (_,v') -> a.(i1).(i2) <- v'
    | Val_array3 ((sz1, sz2, sz3), a), [Val_int (i1, _); Val_int (i2, _); Val_int (i3, _)], _ -> a.(i1).(i2).(i3) <- v
    | _ -> failwith "Expr.array_set" (* should not happen thx to type checking *)
  with Array1.Invalid_index _ | Array2.Invalid_index _ | Array3.Invalid_index _ ->
    array_index_out_of_range loc

let array_replace loc ar ixs v =
  let ar' = match ar with
      Val_array1 (sz1, a) -> Val_array1 (sz1, Array1.copy a)
    | Val_array2 ((sz1,sz2), a) -> Val_array2((sz1,sz2), Array2.copy a)
    | Val_array3 ((sz1,sz2,sz3), a) -> Val_array3((sz1,sz2,sz3), Array2.copy a)
    | _ -> failwith "Expr.array_replace" (* should not happen thx to type checking *) in
  array_set loc ar' ixs v;
  ar'

(* Rule TE, EE |- Expr => v *)

let rec eval_expression tenv env expr =
(* Note 2015-12-11, JS.
  [tenv] here only contains the "extra" bindings needed to resolve attributes. Other types (such as those
   needed to evaluate casts) can be retrieved from the [e_typ] and [te_typ] annotations which have been
   computed during the typing phase *)
  let lookup_env id =
    try List.assoc id env
    with Not_found -> unbound_value_err id expr.e_loc in
  match expr.e_desc with
  | EConst c -> eval_const expr.e_loc c
  | EVar id -> 
      begin
        match lookup_env id with
          Val_unknown -> uninitialized_var expr.e_loc id
        | v -> v
      end
  | EAttr (id,attr) -> 
      eval_attribute expr.e_loc tenv env attr id
  | ECon (c,es) -> Val_con (c,List.map (eval_expression tenv env) es)
  | EApp(fn, _, args) ->
      let val_fn = eval_expression tenv env fn in
      let val_args = List.map (eval_expression tenv env) args in
      begin match val_fn with
      | Val_prim primitive ->
          let r = primitive (tuplify_value_list val_args)
          in r
      | Val_fun (_, fargs, exp) ->
          let env' = List.combine fargs val_args in
          eval_expression tenv (env' @ env) exp
      | Val_extern { ef_ml_name = f } ->
          let v = Foreign.call f (List.map (cast_to_int expr.e_loc) val_args) in
          Val_int (v, None)
      | _ ->
          application_of_non_function_err expr.e_loc
      end
  | ELet([id,exp], body) ->
      let v = eval_expression tenv env exp in
      eval_expression tenv ((id,v)::env) body
  | ELet(_, _) ->
      failwith "should not happen" (* Multiple let-defs are only introduced in the interm repr *)
  | ECond(e1, e2, e3) ->
      begin match eval_expression tenv env e1 with
        Val_bool true -> eval_expression tenv env e2
      | Val_bool false -> eval_expression tenv env e3
      | _ -> fatal_error "Expr.eval_expression : first arg of ECond is not a boolean value" end
  | EIgnored -> Val_ignored
  | EArrayExt1 es -> 
      let a = Array1.of_list (List.map (eval_expression tenv env) es) in
      Val_array1 (Array1.size a, a)
  | EArrayExt2 ess ->
      begin try
        let a = Array2.of_list (List.map (List.map (eval_expression tenv env)) ess) in
        Val_array2 (Array2.size a, a)
       with Array2.Invalid_size ->
         illegal_array_extension expr.e_loc
      end
  | EArrayExt3 esss ->
      begin try
        let a = Array3.of_list (List.map (List.map (List.map (eval_expression tenv env))) esss) in
        Val_array3 (Array3.size a, a)
       with Array2.Invalid_size ->
         illegal_array_extension expr.e_loc
      end
  | EArrayCompr ([id1,lo1,hi1],e) ->
      begin match eval_expression tenv env lo1, eval_expression tenv env hi1 with
        Val_int (0,_), Val_int (n1,_) ->
          let sz = n1+1 in
          Val_array1 (
            sz,
            Array1.init
              sz 
              (function i1 ->
                let env'  = (id1,Val_int(i1,None))::env in 
                eval_expression tenv env' e))
      | _, _ ->
        illegal_array_range expr.e_loc
      end
  | EArrayCompr ([id1,lo1,hi1;id2,lo2,hi2],e) ->
      begin match eval_expression tenv env lo1, eval_expression tenv env hi1,
                  eval_expression tenv env lo2, eval_expression tenv env hi2 with
        Val_int (0,_), Val_int (n1,_), Val_int (0,_), Val_int (n2,_) ->
          let sz1 = n1+1 in
          let sz2 = n2+1 in
          Val_array2 (
            (sz1, sz2),
            Array2.init 
              (sz1,sz2)
              (fun i1 i2 ->
                let env' = (id1,Val_int(i1,None))::(id2,Val_int(i2,None))::env in
                eval_expression tenv env' e))
      | _, _, _, _ ->
        illegal_array_range expr.e_loc
     end
  | EArrayCompr ([id1,lo1,hi1;id2,lo2,hi2;id3,lo3,hi3],e) ->
      begin match eval_expression tenv env lo1, eval_expression tenv env hi1,
                  eval_expression tenv env lo2, eval_expression tenv env hi2,
                  eval_expression tenv env lo3, eval_expression tenv env hi3 with
        Val_int (0,_), Val_int (n1,_), Val_int (0,_), Val_int (n2,_), Val_int (0,_), Val_int (n3,_) ->
          let sz1 = n1+1 in
          let sz2 = n2+1 in
          let sz3 = n3+1 in
          Val_array3 (
            (sz1, sz2, sz3),
            Array3.init 
              (sz1,sz2,sz3)
              (fun i1 i2 i3 ->
                let env' = (id1,Val_int(i1,None))::(id2,Val_int(i2,None))::(id3,Val_int(i3,None))::env in
                eval_expression tenv env' e))
      | _, _, _, _, _, _ ->
        illegal_array_range expr.e_loc
     end
  | EArrayCompr (_,_) -> failwith "Expr.eval_expression(EArrayCompr)" (* should not happen *)
  | EArrRead (id, idx) -> array_get expr.e_loc (List.assoc id env) (List.map (eval_expression tenv env) idx)
  | ECast (e,t) -> eval_cast e.e_loc (e.e_typ) (t.te_typ) (eval_expression tenv env e)
  | ETuple es -> Val_tuple (List.map (eval_expression tenv env) es)

and eval_const loc = function
    Const.CInt (v,_,_) -> Val_int (v,None)
  | Const.CBool v -> Val_bool v
  | Const.CFloat v -> Val_float v
        
and eval_cast loc t1 t2 v =
    begin match Types.real_type t1, Types.real_type t2, v with
      (* INT -> INT with different signness and/or precision *)
      (* Allowed cases have been specified (and checked) by [Typing.type_cast] *)
    | Tconstr({tc_name="int"}, [sg1], [sz1]), Tconstr({tc_name="int"}, [sg2], [sz2]), Val_int(n,p) ->
         begin match real_type sg2, size_repr sz2 with
            Tconstr({tc_name="_unsigned"}, _, _),  SzConst s2 -> Val_int (to_unsigned loc s2 n, Some (Unsigned s2))
          | Tconstr({tc_name="_signed"}, _, _),  SzConst s2 -> Val_int (to_signed loc s2 n, Some (Signed s2))
          | _, _ -> Val_int (n, None)
        end
      (* INT -> BOOL *)
      (*  ([un]signed<m>:bool) is ok (the resulting value is true if the original value is != 0, false otherwise *)
    | Tconstr({tc_name="int"}, [sg], [sz]), Tconstr({tc_name="bool"}, [], []), Val_int (n,p) ->
        Val_bool (n <> 0)
      (* BOOL -> INT *)
      (* (bool:[un]signed<m>) is ok (the resulting value is 1 for true, 0 for false *)
    | Tconstr({tc_name="bool"}, [], []), Tconstr({tc_name="int"}, [sg], [sz]), Val_bool b  ->
         begin match real_type sg, size_repr sz with
            Tconstr({tc_name="_unsigned"}, _, _),  SzConst s2 -> Val_int (int_of_bool b, Some (Unsigned s2))
          | Tconstr({tc_name="_signed"}, _, _),  SzConst s2 -> Val_int (int_of_bool b, Some (Signed s2))
          | _, _ -> illegal_cast loc t1 t2
         end
      (* INT -> FLOAT *)
    | Tconstr({tc_name="int"}, [sg], [sz]), Tconstr({tc_name="float"}, [], []), Val_int (n,p) ->
        Val_float (float_of_int n)
      (* BOOL -> FLOAT *)
    | Tconstr({tc_name="bool"}, [], []), Tconstr({tc_name="float"}, [], []), Val_bool b ->
        Val_float (float_of_int (int_of_bool b))
      (* FLOAT -> INT *)
    | Tconstr({tc_name="float"}, [], []), Tconstr({tc_name="int"}, [sg], [sz]), Val_float f  ->
         begin match real_type sg, size_repr sz with
            Tconstr({tc_name="_unsigned"}, _, _),  SzConst s2 -> Val_int (int_of_float f, Some (Unsigned s2))
          | Tconstr({tc_name="_signed"}, _, _),  SzConst s2 -> Val_int (int_of_float f, Some (Signed s2))
        | _, _ -> illegal_cast loc t1 t2 end
      (* FLOAT -> BOOL *)
      (*  (float:bool) is ok (the resulting value is true if the original value is != 0, false otherwise *)
    | Tconstr({tc_name="float"}, [], []), Tconstr({tc_name="bool"}, [], []), Val_float f ->
        Val_bool (f <> 0.0)
      (* FLOAT -> FLOAT *)
    | Tconstr({tc_name="float"}, [], []), Tconstr({tc_name="float"}, [], []), Val_float f  ->
        Val_float f
      (* TUPLES *)
    | Tproduct ts1, Tproduct ts2, Val_tuple vs when List.length ts1 = List.length ts2 ->
        Val_tuple (Misc.map3 (fun ty1 ty2 v -> eval_cast loc ty1 ty2 v) ts1 ts2 vs)
      (* ARRAYS *)
    | Tconstr ({tc_name="array"}, _, _), Tconstr({tc_name="array"}, _, _), v ->
        begin
          match array_dim t1, array_dim t2, v with
          | (tt1, [_]), (tt2, [_]), Val_array1 (sz, vs) ->
              Val_array1 (sz, Array1.map (eval_cast loc tt1 tt2) vs)
          | (tt1, [_; _]), (tt2, [_; _]), Val_array2 (sz, vs) ->
              Val_array2 (sz, Array2.map (eval_cast loc tt1 tt2) vs)
          | (tt1, [_; _; _]), (tt2, [_; _; _]), Val_array3 (sz, vs) ->
              Val_array3 (sz, Array3.map (eval_cast loc tt1 tt2) vs)
          | _, _, _ ->
              illegal_cast loc t1 t2
        end
      (* TODO ? other CONSTRUCTED TYPES *)
    | _, _, _ ->
      illegal_cast loc t1 t2 end

and int_of_bool = function false -> 0 | true -> 1

and to_signed loc sz n =
  let b = 2. ** (float (sz-1)) in
  let m = float n in
  let n' =
    if m >= -.b && m <= b-.1. then n
    else begin
      warning_truncated_cast loc (string_of_int n) ("signed<" ^ string_of_int sz ^ ">");
      int_of_float(mod_float m b) end in
  n'

and to_unsigned loc sz n =
  let b = 2. ** (float sz) in
  let m = float n in
  let n' =
    if m >= 0. && m <= b-.1. then n
    else begin
      warning_truncated_cast loc (string_of_int n) ("unsigned<" ^ string_of_int sz ^ ">");
      int_of_float(mod_float m b) end in
  n'
    
and cast_to_int loc v = match v with
     Val_int (v,_) -> v
   | _ -> cast_error "int" "expression (for calling external function)" loc

and eval_attribute loc tenv env attr id = match attr with
  "size" -> 
    let ty = 
      try List.assoc id tenv 
      with Not_found -> failwith "Expr.eval_attribute" in (* should not happen *)
    begin match Types.type_width ty with
      Some (SzConst n) -> Val_int (n,None)
    | _ -> invalid_size_attribute loc
    end
| _ ->
    illegal_attribute loc attr
   
let rec is_static_constant v = match v with
   | Val_int (_,_) -> true
   | Val_bool _ -> true
   | Val_float _ -> true
   | Val_array1 (_,vs) -> List.for_all is_static_constant (Array1.to_list vs)
   | Val_array2 (_,vs) ->  List.for_all is_static_constant (List.flatten (Array2.to_list vs))
   | Val_array3 (_,vs) -> List.for_all is_static_constant (List.flatten (List.flatten ((Array3.to_list vs))))
   | Val_con (_, vs) -> List.for_all is_static_constant vs
   | _ -> false

(* Printing *)

let flat_variants = ref false

let rec string_of_val v = match v with
    Val_int (i,_) -> string_of_int i
  | Val_bool b -> string_of_bool b
  | Val_float v -> string_of_float v
  | Val_tuple vs ->
      if !flat_variants
      then  Misc.string_of_list string_of_val " " vs
      else "(" ^ Misc.string_of_list string_of_val "," vs ^ ")"
  | Val_array1 (_,vs) -> Array1.to_string ~ld:"{" ~rd:"}" string_of_val vs
  | Val_array2 (_,vs) -> Array2.to_string ~ld:"{" ~rd:"}" string_of_val vs
  | Val_array3 (_,vs) -> Array3.to_string ~ld:"{" ~rd:"}" string_of_val vs
  | Val_extern _ -> "extern"
  | Val_fun _ -> "<fun>"
  | Val_prim _ -> "<prim>"
  | Val_hprim (p,n) -> "<" ^ p ^ "," ^ string_of_int n ^ ">"
  | Val_con ("SoS",[]) -> if !abbrev_dc_ctors then "<" else "SoS"
  | Val_con ("EoS",[]) -> if !abbrev_dc_ctors then ">" else "EoS"
  | Val_con ("Data",[v]) -> if !abbrev_dc_ctors then string_of_simple_val v else "Data " ^ string_of_simple_val v
  | Val_con (c, []) -> c
  | Val_con (c, [v]) -> c ^ " " ^ string_of_simple_val v
  | Val_con (c, vs) ->
      if !flat_variants
      then  c ^ " " ^ Misc.string_of_list string_of_val " " vs 
      else  c ^ " (" ^ Misc.string_of_list string_of_val "," vs ^ ")"
  | Val_unknown -> "<unknown>"
  | Val_ignored -> "_"

and string_of_simple_val v = if is_simple_val v then string_of_val v else "(" ^ string_of_val v ^ ")"

and is_simple_val = function
  | Val_int _ 
  | Val_bool _
  | Val_float _ -> true
  | Val_tuple _ -> true
  | _ -> false

let rec is_simple_expr_val = function
  | Val_int _ 
  | Val_bool _
  | Val_float _ -> true
  | Val_con (_, vs) -> List.for_all is_simple_expr_val vs
  | _ -> false
