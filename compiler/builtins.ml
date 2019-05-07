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

open Types
open Typing
open Expr
open Ssval
open Dsval
open Misc

(* Builtin type constructors *)

let constr_type_bundle = { tc_name="bundle"; tc_abbr=Tnotabbrev }

(* Aux fns for type descr *)

let type_unop t t' = type_arrow t t'
let type_binop t t' = type_arrow (type_pair t t) t'

let type_arithm () = 
  let sg = Tvar (mk_type_var ()) in
  let sz = SzVar (mk_size_var ()) in
  type_arrow (type_product [type_int sg sz; type_int sg sz]) (type_int sg sz)

let type_arithm1 () = 
  let sg = Tvar (mk_type_var ()) in
  let sz = SzVar (mk_size_var ()) in
  type_arrow (type_int sg sz) (type_int sg sz)

let type_compar () = 
  let sg = Tvar (mk_type_var ()) in
  let sz = SzVar (mk_size_var ()) in
  type_arrow (type_product [type_int sg sz; type_int sg sz]) (type_bool)

let type_shift () = 
  let sg = Tvar (mk_type_var ()) in
  let sz = SzVar (mk_size_var ()) in
  let sz' = SzVar (mk_size_var ()) in
  type_arrow (type_product [type_int sg sz; type_unsigned sz']) (type_int sg sz)

let higher_order_types = [
  ("map", function () ->
   let t1 = Tvar (mk_type_var ()) in
   let t2 = Tvar (mk_type_var ()) in
   let sz = SzVar (mk_size_var ()) in
   type_arrow2 (type_arrow t1 t2) (type_bundle t1 sz) (type_bundle t2 sz));
  ("map2", function () ->
   let t1 = Tvar (mk_type_var ()) in
   let t2 = Tvar (mk_type_var ()) in
   let t3 = Tvar (mk_type_var ()) in
   let sz = SzVar (mk_size_var ()) in
   type_arrow2 (type_arrow (type_product [t1;t2]) t3) (type_product [type_bundle t1 sz; type_bundle t2 sz]) (type_bundle t3 sz));
  ("mapi", function () ->
   let t1 = Tvar (mk_type_var ()) in
   let t2 = Tvar (mk_type_var ()) in
   let sz = SzVar (mk_size_var ()) in
   let sz' = SzVar (mk_size_var ()) in
   type_arrow2 (type_arrow2 (type_unsigned sz') t1 t2) (type_bundle t1 sz) (type_bundle t2 sz));
  ("map2i", function () ->
   let t1 = Tvar (mk_type_var ()) in
   let t2 = Tvar (mk_type_var ()) in
   let t3 = Tvar (mk_type_var ()) in
   let sz = SzVar (mk_size_var ()) in
   let sz' = SzVar (mk_size_var ()) in
   type_arrow2
    (type_arrow2 (type_unsigned sz') (type_product [t1;t2]) t3)
    (type_product [type_bundle t1 sz; type_bundle t2 sz]) (type_bundle t3 sz));
  ("foldl", function () ->
   let t1 = Tvar (mk_type_var ()) in
   let sz = SzVar (mk_size_var ()) in
   type_arrow2 (type_arrow (type_product [t1;t1]) t1) (type_bundle t1 sz)  t1);
  ("foldli", function () ->
   let t1 = Tvar (mk_type_var ()) in
   let sz = SzVar (mk_size_var ()) in
   let sz' = SzVar (mk_size_var ()) in
   type_arrow2 (type_arrow2 (type_unsigned sz') (type_product [t1;t1]) t1) (type_bundle t1 sz)  t1);
  ("foldt", function () ->
   let t1 = Tvar (mk_type_var ()) in
   let sz = SzVar (mk_size_var ()) in
   type_arrow2 (type_arrow (type_product [t1;t1]) t1) (type_bundle t1 sz)  t1);
  ("pipe", function () ->
    let sg = Tvar (mk_type_var ()) in
    let sz = SzVar (mk_size_var ()) in
    let t1 = Tvar (mk_type_var ()) in
    type_arrow3 (type_int sg sz) (type_arrow t1 t1) t1 t1);
  ("chain", function () ->
    let sg1 = Tvar (mk_type_var ()) in
    let sz1 = SzVar (mk_size_var ()) in
    let t2 = Tvar (mk_type_var ()) in
    let t3 = Tvar (mk_type_var ()) in
    type_arrow3 (type_int sg1 sz1) (type_arrow t2 t3) t2 (type_bundle t3 (SzRef 1)));
  ("repln", function () ->
   let sg1 = Tvar (mk_type_var ()) in
   let sz1 = SzVar (mk_size_var ()) in
   let t2 = Tvar (mk_type_var ()) in
   type_arrow2 (type_int sg1 sz1) t2 (type_bundle t2 (SzRef 1)));
  ("napp", function () ->
   let sg1 = Tvar (mk_type_var ()) in
   let sz1 = SzVar (mk_size_var ()) in
   let t2 = Tvar (mk_type_var ()) in
   let t3 = Tvar (mk_type_var ()) in
   type_arrow3 (type_int sg1 sz1) (type_arrow t2 t3) t2 (type_bundle t3 (SzRef 1)));
  ("nappi", function () ->
   let sg1 = Tvar (mk_type_var ()) in
   let sz1 = SzVar (mk_size_var ()) in
   let t2 = Tvar (mk_type_var ()) in
   let sz2 = SzVar (mk_size_var ()) in
   let t3 = Tvar (mk_type_var ()) in
   type_arrow3 (type_int sg1 sz1) (type_arrow2 (type_unsigned sz2) t2 t3) t2 (type_bundle t3 (SzRef 1)))
  ]

let lookup_type pname types = 
  let f = try List.assoc pname types with Not_found -> fatal_error "Builtins.lookup_type" in
  f ()
  
(* Aux fns for building the expression-level builtin environment *)

let encode_int n =
    Val_int (n,None)
let rec decode_int = function
  | Val_int (n,_) -> n
  | _ -> fatal_error "Builtins.decode_int" (* should not happen *)
let encode_bool b =
    Val_bool b
let rec decode_bool = function
  | Val_bool b -> b
  | _ -> fatal_error "Builtins.decode bool" (* should not happen *)
let encode_float v =
    Val_float v
let rec decode_float = function
  | Val_float v -> v
  | _ -> fatal_error "Builtins.decode float" (* should not happen *)

let prim1 encode op decode =
  Val_prim(function v -> encode (op (decode v)))
and prim2 encode op decode1 decode2 =
  Val_prim(function
   | Val_tuple [v1;v2] ->
       encode (op (decode1 v1) (decode2 v2))
   | _ -> fatal_error "Builtins.prim2")
let tprim2 op =
  let decode v = match v with
    Val_int (n,_) -> Val_int (n,None) 
      (* Note 2011-05-27, JS :
         We must ignore prec annotation to apply polymorphic builtin primitives; otherwise, 
         [Val_int(2, None)] may be different than [Val_int(2, Some p)] ! *)
  | _ -> v in
  Val_prim(function
   | Val_tuple [v1;v2] ->
       let v1'= decode v1
       and v2' = decode v2 in
       encode_bool (op v1' v2')
   | _ -> fatal_error "Builtins.tprim2")

(* Primitives *)

let builtin_primitives = [
  (* Id, type, expr-level value *)
    "+",  (type_arithm (), prim2 encode_int  ( + ) decode_int decode_int);
    "-",  (type_arithm (), prim2 encode_int  ( - ) decode_int decode_int);
    "*",  (type_arithm (), prim2 encode_int  ( * ) decode_int decode_int);
    "/",  (type_arithm (), prim2 encode_int  ( / ) decode_int decode_int);
    "mod", (type_arithm (), prim2 encode_int  ( mod ) decode_int decode_int);
    "+.", (type_binop type_float type_float, prim2 encode_float  ( +. ) decode_float decode_float);
    "-.", (type_binop type_float type_float, prim2 encode_float  ( -. ) decode_float decode_float);
    "*.", (type_binop type_float type_float, prim2 encode_float  ( *. ) decode_float decode_float);
    "/.", (type_binop type_float type_float, prim2 encode_float  ( /. ) decode_float decode_float);
    "~-",  (type_arithm1 (), prim1 encode_int ( ~- ) decode_int);
    "~-.",  (type_unop type_float type_float, prim1 encode_float ( ~-. ) decode_float);
    "=.", (type_binop type_float type_bool, prim2 encode_bool  ( = ) decode_float decode_float);
    "!=.", (type_binop type_float type_bool, prim2 encode_bool  ( != ) decode_float decode_float);
    ">.", (type_binop type_float type_bool, prim2 encode_bool  ( > ) decode_float decode_float);
    "<.", (type_binop type_float type_bool, prim2 encode_bool  ( < ) decode_float decode_float);
    ">=.", (type_binop type_float type_bool, prim2 encode_bool  ( >= ) decode_float decode_float);
    "<=.", (type_binop type_float type_bool, prim2 encode_bool  ( <= ) decode_float decode_float);
    "||", (type_binop type_bool type_bool, prim2 encode_bool  ( || ) decode_bool decode_bool);
    "&&", (type_binop type_bool type_bool, prim2 encode_bool  ( && ) decode_bool decode_bool);
    "=",  (type_compar () , tprim2 ( = ));
    "!=", (type_compar (), tprim2 ( <> ));
    "<",  (type_compar (), tprim2 ( < ));
    ">",  (type_compar (), tprim2 ( > ));
    "<=", (type_compar (), tprim2 ( <= ));
    ">=", (type_compar (), tprim2 ( >= ));
    "not",  (type_unop type_bool type_bool, prim1 encode_bool ( not ) decode_bool);
    "lnot",  (type_arithm1 (), prim1 encode_int ( lnot ) decode_int);
    "land",  (type_arithm (), prim2 encode_int  ( land ) decode_int decode_int);
    "lor",  (type_arithm (), prim2 encode_int  ( lor ) decode_int decode_int);
    "lxor",  (type_arithm (), prim2 encode_int  ( lor ) decode_int decode_int);
    ">>",  (type_shift (), prim2 encode_int  ( asr ) decode_int decode_int);
    "<<",  (type_shift (), prim2 encode_int  ( lsl ) decode_int decode_int);
    "map",  (lookup_type "map" higher_order_types, Val_hprim ("map",1));
    "map2",  (lookup_type "map2" higher_order_types, Val_hprim ("map2",1));
    "mapi",  (lookup_type "mapi" higher_order_types, Val_hprim ("mapi",1));
    "map2i",  (lookup_type "map2i" higher_order_types, Val_hprim ("map2i",1));
    "foldl",  (lookup_type "foldl" higher_order_types, Val_hprim ("foldl",1));
    "foldli",  (lookup_type "foldli" higher_order_types, Val_hprim ("foldli",1));
    "foldt",  (lookup_type "foldt" higher_order_types, Val_hprim ("foldt",1));
    "pipe",  (lookup_type "pipe" higher_order_types, Val_hprim ("pipe",2));
    "chain",  (lookup_type "chain" higher_order_types, Val_hprim ("chain",2));
    "repln",  (lookup_type "repln" higher_order_types, Val_hprim ("repln",1));
    "napp",  (lookup_type "napp" higher_order_types, Val_hprim ("napp",2));
    "nappi",  (lookup_type "nappi" higher_order_types, Val_hprim ("nappi",2))
]

let builtin_typing_env = {
  te_types = [
    "int", {tc_defn={ty_constr={tc_name="int";tc_abbr=Tnotabbrev}; ty_arity=1,1; ty_desc=Abstract_type}; tc_insts=TyconInsts.empty ()};
    "bool", {tc_defn={ty_constr={tc_name="bool";tc_abbr=Tnotabbrev}; ty_arity=0,0; ty_desc=Abstract_type}; tc_insts=TyconInsts.empty ()};
    "unit", {tc_defn={ty_constr={tc_name="unit";tc_abbr=Tnotabbrev}; ty_arity=0,0; ty_desc=Abstract_type}; tc_insts=TyconInsts.empty ()};
    "float", {tc_defn={ty_constr={tc_name="float";tc_abbr=Tnotabbrev}; ty_arity=0,0; ty_desc=Abstract_type}; tc_insts=TyconInsts.empty ()};
    "array", {tc_defn={ty_constr=constr_type_array; ty_arity=1,1; ty_desc=Abstract_type};tc_insts=TyconInsts.empty ()};
    "bundle", {tc_defn={ty_constr=constr_type_bundle; ty_arity=1,1; ty_desc=Abstract_type};tc_insts=TyconInsts.empty ()} ];
  te_values = List.map (function (n,(t,v)) -> n, generalize [] t) builtin_primitives;
  te_ctors = []; (* Note v2.6.2 : empty since the [dc] ctor is now longer builtin *)
  te_consts = []
}

let builtin_static_env = {
  se_vals = List.map (function (n,(t,v)) -> n,v) builtin_primitives;
  se_ctors  = [ (* Name, arity *)
      ]  (* Note v2.6.2 : empty since the [dc] ctor is now longer builtin *)
}

