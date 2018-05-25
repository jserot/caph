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

(* VHDL backend *)

open Printf
open Types
open Static
open Interm
open Arrays

type vhdl_config = {
  mutable vhdl_core_libs: string list;
  mutable vhdl_core_fp_libs: string list;
  mutable vhdl_num_lib: string;
  mutable vhdl_fp_libs: string list;
  mutable vhdl_extfns_package: string;
  mutable vhdl_global_suffix: string;
  mutable vhdl_splitters_suffix: string;
  mutable vhdl_globals_package_name: string;
  mutable vhdl_clock: string;
  mutable vhdl_reset: string;
  mutable vhdl_type_prefix: string;
  mutable vhdl_sig_ena_prefix: string;
  mutable vhdl_sig_nxt_prefix: string;
  mutable vhdl_default_int_size: int;
  mutable vhdl_default_int_signness: string;
  mutable vhdl_arch_tag: string;
  mutable vhdl_annot_file: string;
  mutable vhdl_fifo_offset: int;
  mutable vhdl_buffer_model: string;
  mutable vhdl_default_fifo_capacity: int;
  mutable vhdl_big_fifo_model: string;
  mutable vhdl_small_fifo_model: string;
  mutable vhdl_fifo_model_threshold: int;
  mutable vhdl_reset_duration_ns: int;
  mutable vhdl_clock_period_ns: int;
  mutable vhdl_seq_delay_ns: int;
  mutable vhdl_tb_stream_in_name: string;
  mutable vhdl_tb_stream_mult_in_name: string;
  mutable vhdl_stream_in_period: int;
  mutable vhdl_stream_in_blanking: bool;
  mutable vhdl_stream_in_skew: int;
  mutable vhdl_tb_stream_out_name: string;
  mutable vhdl_tb_stream_mult_out_name: string;
  mutable vhdl_tb_port_in_name: string;
  mutable vhdl_tb_port_out_name: string;
  mutable vhdl_trace: bool;
  mutable vhdl_io_file_suffix: string;
  mutable vhdl_init_array_at_decl: bool;
  mutable vhdl_use_native_mult: bool;
  mutable vhdl_default_float_size: int;
  mutable vhdl_float_support: bool;
  mutable vhdl_write_type_converters: bool;
  mutable vhdl_tyconv_lib: string;
  mutable vhdl_mem_ctlr_name: string;
  mutable vhdl_mem_ctlr_base_addr: int;
  mutable vhdl_rename_io_wires: bool;
  mutable vhdl_generate_qip: bool;
  mutable vhdl_warn_on_unsized_consts: bool;
  }

let cfg = {
  vhdl_core_libs = [ "ieee.std_logic_1164.all"; "caph.core.all"; "caph.data_types.all" ];
  vhdl_core_fp_libs = [ "ieee.std_logic_1164.all"; "caph.core.all"; "caph.data_types.all"; "caph_fp.core_fp.all" ];
  vhdl_num_lib = "ieee.numeric_std.all";
  vhdl_fp_libs = [
    "ieee.math_real.all";
    "ieee_proposed.fixed_float_types.all";
    "ieee_proposed.fixed_pkg.all";
    "ieee_proposed.float_pkg.all" ];
  vhdl_extfns_package = "work.extfns";
  vhdl_global_suffix = "globals";
  vhdl_splitters_suffix = "_splitters";
  vhdl_globals_package_name = "globals";
  vhdl_clock = "clock";
  vhdl_reset = "reset";
  vhdl_type_prefix = "";
  vhdl_sig_ena_prefix = "en_";
  vhdl_sig_nxt_prefix = "n_";
  vhdl_default_int_size = 8;
  vhdl_default_int_signness = "_unsigned";
  vhdl_arch_tag = "arch";
  vhdl_annot_file = "fifo_caps.dat";
  vhdl_fifo_offset = 2;
  vhdl_buffer_model = "port_buffer";
  vhdl_default_fifo_capacity = 4;
  vhdl_small_fifo_model = "fifo";
  vhdl_big_fifo_model = "fifo";
  vhdl_fifo_model_threshold = 32;
  vhdl_reset_duration_ns = 1;
  vhdl_clock_period_ns = 10;
  vhdl_seq_delay_ns = 1;  (* TO FIX : this should be 0 !! *)
  vhdl_tb_stream_in_name = "stream_in";
  vhdl_tb_stream_mult_in_name = "stream_in_mult";
  vhdl_stream_in_period = 1;
  vhdl_stream_in_blanking = false;
  vhdl_stream_in_skew = 0;
  vhdl_tb_stream_out_name = "stream_out";
  vhdl_tb_stream_mult_out_name = "stream_out_mult";
  vhdl_tb_port_in_name = "port_in";
  vhdl_tb_port_out_name = "port_out";
  vhdl_trace = false;
  vhdl_io_file_suffix = "bin";
  vhdl_init_array_at_decl = false;
  vhdl_use_native_mult = false;
  vhdl_default_float_size = 32;
  vhdl_float_support = false;
  vhdl_write_type_converters = false;
  vhdl_tyconv_lib = "tyconv";
  vhdl_mem_ctlr_name = "mem_controller";
  vhdl_mem_ctlr_base_addr = 0;
  vhdl_rename_io_wires = false;
  vhdl_generate_qip = false;
  vhdl_warn_on_unsized_consts = false;
}

let dump_banner oc = Misc.dump_banner "--" oc

let default_wire_name wid = "w" ^ (string_of_int wid)

let rename_wire renamed (id, ty) =
  if List.mem_assoc id renamed then (List.assoc id renamed, ty) else (default_wire_name id, ty)

(* Types *)

type vhdl_type =
  | Signed_vec of vec_size
  | Unsigned_vec of vec_size
  | Integer of range option
  | Float
  | Boolean
  | Enum of string   (* ty ctor *)
  | Variant of variant_desc 
  | Array of array_desc
  | Size of int
  | TVar of Types.typ var

and range = int * int (* low, high *)
           
and vec_size = 
    VS_Const of int
  | VS_Var of Types.siz var

and array_desc = vhdl_type * int (* subtype, size *)

and variant_desc = 
   { vd_name: string;            (* full type name *)
     vd_tparams: typ list;       (* actual type params for polymorphic variants (empty for monomorphic variants) *)
     vd_sparams: siz list;       (* actual size params for polymorphic variants (empty for monomorphic variants) *)
     vd_repr: vd_bits;           (* Bit level repr *)
     vd_ctors: vc_desc list }    (* value ctors *)
  
and vd_bits = 
   { vr_size: int;               (* Total size in bits *)
     vr_tag_size: int;           (* Size of the tag part [ceil(log2(nb_of_ctors))] *)
     vr_data_size: int;          (* Size of the data part [max_i(size(ctor_i))] *)
     vr_tag: bit_range; 
     vr_data: bit_range }
 
and bit_range = { hi: int; lo: int }

and vc_desc =
  { vc_name: string;
    vc_tag: int;
    vc_size: int;                        (* Total size for the associated data (0 for nullary ctors) *)
    vc_arity: int;                       (* 0 for constant ctors *)
    vc_arg: (vhdl_type * int) list; }    (* Argument type(s), with their size ([] for constant ctors) *)

exception Incomplete_type of Types.typ

type splitter_desc = int (* fanout *)

type profil = {
    has_externs: bool;
    has_globals: bool;
    use_floats: bool;
    array_types: (string * array_desc) list;   (* Item type, dims *)
    variant_types: (string * variant_desc) list;
    input_variant_types: (string * variant_desc) list;          (* For dumping IO converters *)
    output_variant_types: (string * variant_desc) list;
    splitters: splitter_desc list }

type pseudo_const_desc = {
    cst_type: vhdl_type;
    cst_mem_offset: int;
    cst_mem_size: int
  }

module TypeVarNames =
  Varname.Make
    (struct
      type t = typ var
      let init = 1
      let to_string cnt = "_t" ^ string_of_int cnt
    end)

module SizeVarNames =
  Varname.Make
    (struct
      type t = siz var
      let init = 1
      let to_string cnt = "_s" ^ string_of_int cnt
    end)

exception Type_size

let rec false_val ty =
  match real_type ty with
  | Tconstr({tc_name="array"}, [ty'], _) -> "(others => " ^ false_val ty' ^ ")"
  | _ -> "false"

let rec init_dim_array v n = 
  if n = 0 then v else  "(others => " ^ init_dim_array v (n-1) ^ ")"

let get_type_size sz =
  match size_repr sz with
    SzConst n -> string_of_int n
  | SzVar v -> SizeVarNames.name_of v
  | _ -> raise Type_size

let get_num_size sz =
  match size_repr sz with
    SzConst n -> n
  | _ -> raise Type_size

(* Name mangling  *)

let rec mangle_type ty = match real_type ty with
  (* Note v2.6.2.
     We use a slightly specialized schema here, due to both VHDL syntax limitations and backward compatibility issues *)
  | Tconstr({tc_name="array"}, [ty'], [sz])
        when is_simple_type ty' || is_array_of_simple_type ty' || is_array_of_array_of_simple_type ty' ->
      "array" ^ (Mangling.string_of_size sz) ^ "_" ^ mangle_type ty'
  | Tconstr({tc_name=name}, ts, ss)  when Typing.is_variant_type name ->
      Mangling.string_of_name name ts ss []
  | t -> Mangling.string_of_type t
  
let rec string_of_type ty = string_of_vhdl_type (vhdl_type_of ty)

and string_of_return_type ty = string_of_vhdl_return_type (vhdl_type_of ty)

and string_of_farg_type ty = string_of_vhdl_farg_type (vhdl_type_of ty)

and string_of_io_type ty = string_of_vhdl_io_type (vhdl_type_of ty)

and vhdl_type_of ty = match real_type ty with
  | Tconstr({tc_name="bool"}, _, _) -> Boolean
  | Tconstr({tc_name="float"}, _, _) when cfg.vhdl_float_support -> Float
  | Tconstr({tc_name="int"}, [], [lo;hi]) -> (* integer range, since 2.8.4 *)
      begin
        match size_repr lo, size_repr hi with
          SzConst l, SzConst h -> Integer (Some (l,h))
        | _, _ -> Integer None
      end
  | Tconstr({tc_name="int"}, [sg], [sz]) -> 
      begin
        match real_type sg, size_repr sz with
          Tconstr({tc_name="_unsigned"}, _, _), SzConst n ->  Unsigned_vec (VS_Const n)
        | Tconstr({tc_name="_unsigned"}, _, _), SzVar v -> Unsigned_vec (VS_Var v)
        | Tconstr({tc_name="_signed"}, _, _), SzConst n ->  Signed_vec (VS_Const n)
        | Tconstr({tc_name="_signed"}, _, _), SzVar v -> Signed_vec (VS_Var v)
        | _, _ -> Integer None
      end
  | Tconstr({tc_name=name}, ts, ss) when Typing.is_variant_type name ->
      Variant (mk_variant_desc name ts ss)
  | Tconstr({tc_name="array"}, [ty'], [sz])
        when is_simple_type ty' || is_array_of_simple_type ty' || is_array_of_array_of_simple_type ty' ->
      begin 
        try
          let s = get_num_size sz in
          Array (vhdl_type_of ty', s)
        with
          Type_size -> failwith "Vhdl.vhdl_type_of: cannot retrieve array size"
      end
  | Tconstr({tc_name=cstr}, [], []) when Misc.string_is_prefix "_enum" cstr  -> (* This is a hack ! *)
      Enum cstr
  | Tvar tv -> TVar tv
  | _ -> cannot_translate_type ty

and mk_variant_desc name ts ss = 
  match Typing.find_type_constr name with
    { Typing.tc_defn={ty_desc=Variant_type (tvs,svs,cdescs)} } ->
      let cds = Misc.list_map_index (variant_ctor_desc (List.combine tvs ts) (List.combine svs ss)) cdescs in
      let tag_sz = Misc.bits_from_card (List.length cds)
      and data_sz = Misc.list_max (List.map (function c -> c.vc_size) cds) in
      let sz = tag_sz + data_sz in
      { vd_name = Mangling.string_of_name name ts ss [];
        vd_tparams = ts;
        vd_sparams = ss;
        vd_repr= {
          vr_size = sz;
          vr_tag_size = tag_sz;
          vr_data_size = data_sz;
          vr_tag = {hi=sz-1; lo=sz-tag_sz};
          vr_data = {hi=data_sz-1; lo=0} };
        vd_ctors = cds }
  | _ -> failwith "Vhdl.mk_variant_dec"  (* should not happen *)

and variant_ctor_desc tvbs svbs i c =
   let ty_arg = copy_type tvbs svbs (real_type c.cs_arg) in
   { vc_name = c.cs_name;
     vc_arity = c.cs_arity;
     vc_tag = begin match c.cs_tag with Some tag -> tag | None -> i end;
     vc_size = num_size_of_type ty_arg;
     vc_arg = 
       match c.cs_arity, ty_arg with
     | 0, _  -> []
     | 1, t -> [vhdl_type_of t, num_size_of_type t]
     | n, Tproduct ts when List.length ts = n -> List.map (function t -> vhdl_type_of t, num_size_of_type t) ts
     | _, _ -> Error.invalid_ctor_arity "by the VHDL backend" c.cs_name }

and mk_array_desc ty sz =
  try vhdl_type_of ty, get_num_size sz
  with Type_size -> failwith "Vhdl.mk_array_desc: cannot retrieve array size"

and size_of_type ty = match real_type ty with 
  | Tconstr ({tc_name="int"}, [sg], [sz]) ->
      begin match size_repr sz with
        SzConst n -> VS_Const n  
      | SzVar v -> VS_Var v
      | _ -> failwith "Vhdl.size_of_type: cannot compute int size"
      end
  | Tconstr ({tc_name="unit"}, _, _) -> VS_Const 0  (* for nullary value ctors *)
  | Tconstr ({tc_name="bool"}, _, _) -> VS_Const 1
  | Tconstr ({tc_name="float"}, _, _) -> VS_Const cfg.vhdl_default_float_size
  | Tconstr ({tc_name=name}, ts, ss) when Typing.is_variant_type name ->
      let vd = mk_variant_desc name ts ss in
      VS_Const vd.vd_repr.vr_size
  | Tproduct ts -> VS_Const (List.fold_left (fun s t -> s + num_size_of_type t) 0 ts)
  | _ -> Misc.not_implemented ("Vhdl.size_of_type: cannot compute size (in bits) for type " ^ Pr_type.string_of_type ty)

and num_size_of_type ty =
  match size_of_type ty with
    VS_Const sz -> sz
  | _ -> Misc.not_implemented ("Vhdl.num_size_of_type: cannot actual size (in bits) for type " ^ Pr_type.string_of_type ty)


and string_of_vhdl_type = function
  | Signed_vec (VS_Const sz) -> sprintf "signed(%d downto 0)" (sz-1)
  | Unsigned_vec (VS_Const sz) -> sprintf "unsigned(%d downto 0)" (sz-1)
  | Signed_vec (VS_Var sz) 
  | Unsigned_vec (VS_Var sz) ->
      Error.unsized_int_warning "vhdl [un]signed" "integer";
      sprintf "integer"
(*   | Signed_vec (VS_Var sz) -> sprintf "signed(%s-1 downto 0)" (SizeVarNames.name_of sz) *)
(*   | Unsigned_vec (VS_Var sz) -> sprintf "unsigned(%s-1 downto 0)" (SizeVarNames.name_of sz) *)
  | Integer None -> "integer"
  | Integer (Some (lo,hi)) -> "integer range " ^ string_of_int lo ^ " to " ^ string_of_int hi
  | Float -> "float" ^ string_of_int cfg.vhdl_default_float_size
  | Boolean -> "boolean"
  | Variant vd -> vd.vd_name
  | Enum cstr -> "t" ^ cstr
  | Array (ty,sz) -> string_of_array_type (ty,sz)
  | Size s -> string_of_int s
  | TVar tv -> TypeVarNames.name_of tv

and string_of_vhdl_io_type = function
  | Signed_vec (VS_Const sz) -> sprintf "std_logic_vector(%d downto 0)" (sz-1)
  | Signed_vec (VS_Var sz) -> sprintf "std_logic_vector(%s-1 downto 0)"  (SizeVarNames.name_of sz)
  | Unsigned_vec (VS_Const sz) -> sprintf "std_logic_vector(%d downto 0)" (sz-1)
  | Unsigned_vec (VS_Var sz) -> sprintf "std_logic_vector(%s-1 downto 0)"  (SizeVarNames.name_of sz)
  | Boolean -> sprintf "std_logic_vector(0 downto 0)"
  | Float when cfg.vhdl_float_support -> sprintf "std_logic_vector(31 downto 0)"
  | Variant { vd_repr={vr_size=n} } -> sprintf "std_logic_vector(%d downto 0)" (n-1)
  | _ -> failwith "Vhdl.string_of_vhdl_io_type: illegal io type"

and string_of_vhdl_farg_type = function
    (* This variant is required for the arg types of functions *)
  | Signed_vec (VS_Const sz) -> sprintf "signed(%d downto 0)" (sz-1)
  | Signed_vec (VS_Var sz) -> sprintf "signed"
  | Unsigned_vec (VS_Const sz) -> sprintf "unsigned(%d downto 0)" (sz-1)
  | Unsigned_vec (VS_Var sz) -> sprintf "unsigned" 
  | Integer None -> "integer"
  | Integer (Some (lo,hi)) -> "integer range " ^ string_of_int lo ^ " to " ^ string_of_int hi
  | Boolean -> "boolean"
  | Float when cfg.vhdl_float_support -> "float" ^ string_of_int cfg.vhdl_default_float_size
  | Enum cstr -> "t" ^ cstr
  | Array (ty,sz) -> string_of_array_type (ty,sz)
  | _ -> failwith "Vhdl.string_of_farg_type: illegal type for function argument"

and string_of_vhdl_return_type = function 
    (* This variant is required for the return type of functions *)
  | Signed_vec  _ -> sprintf "signed"
  | Unsigned_vec  _ -> sprintf "unsigned"
  | t -> string_of_vhdl_type t 

and string_of_vhdl_array_type t = match t with
    (* This variant is required for the encoding array types *)
  | Signed_vec (VS_Const sz) -> sprintf "s%d" sz
  | Signed_vec _ -> sprintf "s"
  | Unsigned_vec (VS_Const sz) -> sprintf "u%d" sz
  | Unsigned_vec _ -> sprintf "u"
  | Enum cstr -> "t" ^ cstr
  | Integer _ -> "i"
  | Boolean -> "b"
  | Array (t',sz') -> "array" ^ string_of_int sz' ^ "_" ^ (string_of_vhdl_array_type t')
  | _ -> failwith "Vhdl.string_of_vhdl_array_type" (* should not happen *)

and string_of_array_type (ty,sz) =
  "array" ^ string_of_int sz ^ "_" ^ (string_of_vhdl_array_type ty)

and size_of_vhdl_type = function
  | Signed_vec (VS_Const sz) -> sz
  | Unsigned_vec (VS_Const sz) -> sz
  | Float when cfg.vhdl_float_support -> cfg.vhdl_default_float_size
  | Boolean -> 1
  | Variant vd -> vd.vd_repr.vr_size
  | t -> Misc.not_implemented ("Vhdl.size_of_vhdl_type: cannot compute size (in bits) for type " ^ string_of_vhdl_type t)

and cannot_translate_type ty = Misc.not_implemented ("VHDL translation of type " ^ Pr_type.string_of_type ty)

(* Constants *)

let rec string_of_const ?(loc=Location.no_location) ty c = match c, real_type ty with
  | Const.CInt (v,_,_), Tconstr({tc_name="int"}, [sg], [sz]) ->
      begin match real_type sg, size_repr sz, cfg.vhdl_default_int_signness with
        Tconstr({tc_name="_unsigned"}, _, _), SzConst n, _ -> string_of_int_const "unsigned" (string_of_int n) v   
      | Tconstr({tc_name="_signed"}, _, _), SzConst n, _ -> string_of_int_const "signed" (string_of_int n) v   
      | Tconstr({tc_name="_unsigned"}, _, _), _, _ 
      | Tconstr({tc_name="_signed"}, _, _), _, _ ->
          if cfg.vhdl_warn_on_unsized_consts then Error.unsized_vhdl_int_const loc;
          string_of_int v   
      | _,  SzConst n, "_unsigned" -> string_of_int_const "unsigned" (string_of_int n) v 
      | _,  SzConst n, "_signed" -> string_of_int_const "signed" (string_of_int n) v 
      | _, _, _ -> 
          if cfg.vhdl_warn_on_unsized_consts then Error.unsized_vhdl_int_const loc;
          string_of_int v   
      end
  | Const.CBool true, _ -> "true"
  | Const.CBool false, _ -> "false"
  | Const.CFloat v, _ -> "to_float(" ^  Misc.string_of_float' v ^ ")"
  | _, _ -> Misc.not_implemented (
           "VHDL translation of constant " ^ Syntax.string_of_const c ^ " (type " ^ Pr_type.string_of_type ty ^ ")")

and string_of_int_const sgn sz v = Printf.sprintf "to_%s(%d,%s)" sgn v sz

and bitstring_of_int n v = 
  let r = String.create n in 
  let rec h i v = if i < 0 then r else begin r.[i] <- if v mod 2 = 1 then '1' else '0'; h (i-1) (v/2) end in
  h (n-1) v

and bitvec_of_int sg sz v = 
  if v < 0 && sg = "_signed"
  then "\"" ^ bitstring_of_int sz (Misc.cpl2 sz (-v)) ^ "\""
  else "\"" ^ bitstring_of_int sz v ^ "\""

(* Expressions *)

let full_fn_name f ann = match ann with
  | None -> f
  | Some (fd,ty) ->
      if TypeInsts.card fd.Syntax.gf_insts > 1 then 
          Mangling.string_of_name f [ty] [] []
      else
          f

let illegal_cast string_of e ty =
    Misc.not_implemented
      ("VHDL backend: cannot cast expression " ^ string_of e ^ " to type " ^ Pr_type.string_of_type ty)

let rec string_of_expr e = string_of_exp ~loc:e.Syntax.e_loc e.Syntax.e_typ e.Syntax.e_desc

and string_of_exp ?(loc=Location.no_location) ty e = match e, real_type ty with
  (* TODO : add parens to reflect nesting and priorities *)
    Syntax.EConst c, ty -> string_of_const ~loc:loc ty c
  | Syntax.EVar v, _ -> v
  (* Unary ops *)
  | Syntax.EApp ({Syntax.e_desc=Syntax.EVar op},_,[e']), _ when Syntax.is_unop op -> string_of_unop op ^ "(" ^ string_of_expr e' ^ ")"
  (* Binary ops *)
  | Syntax.EApp ({Syntax.e_desc=Syntax.EVar ">>"},_,[e1;{Syntax.e_desc=Syntax.EConst(Const.CInt (n,_,_))}]), _ ->
      "SHIFT_RIGHT(" ^ string_of_expr e1 ^ ", " ^ string_of_int n ^ ")"
  | Syntax.EApp ({Syntax.e_desc=Syntax.EVar ">>"},_,[e1;e2]), _ ->
      "SHIFT_RIGHT(" ^ string_of_expr e1 ^ ", to_integer(" ^ string_of_expr e2 ^ "))"
  | Syntax.EApp ({Syntax.e_desc=Syntax.EVar "<<"},_,[e1;{Syntax.e_desc=Syntax.EConst(Const.CInt (n,_,_))}]), _ ->
      "SHIFT_LEFT(" ^ string_of_expr e1 ^ ", " ^ string_of_int n ^ ")"
  | Syntax.EApp ({Syntax.e_desc=Syntax.EVar "<<"},_,[e1;e2]), _ ->
      "SHIFT_LEFT(" ^ string_of_expr e1 ^ ", to_integer(" ^ string_of_expr e2 ^ "))"
  (* Others *)
  | Syntax.EApp ({Syntax.e_desc=Syntax.EVar "*"},_,[e1;e2]), _ when not cfg.vhdl_use_native_mult ->
      "mul(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"
  | Syntax.EApp ({Syntax.e_desc=Syntax.EVar op},_,[e1;e2]), _ when Syntax.is_binop op ->
      "(" ^ string_of_expr e1 ^ ") " ^ string_of_binop op ^ " (" ^ string_of_expr e2 ^ ")"
  | Syntax.EApp ({Syntax.e_desc=Syntax.EVar f},ann,es), _ ->
      full_fn_name f ann ^ "(" ^ Misc.string_of_list string_of_expr "," es ^ ")"
  | Syntax.ECon (cid,[]), ty when is_enum_type ty -> cid  (* special case for locally defined enums *)
  | Syntax.ECon(cid,[]), ty -> sprintf "%s.mk_%s" (mangle_type ty) (String.uncapitalize cid)
  | Syntax.ECon(cid, es), ty ->
      sprintf "%s.mk_%s(%s)"
        (mangle_type ty)
        (String.uncapitalize cid)
        (Misc.string_of_list string_of_expr ", " es)
  (* Nested if-exprs (top-level ones are handled specifically) *)
  (* Since conditionnal is a statement and not an expression in VHDL, we have to use a function *)
  | Syntax.ECond (e1,e2,e3), _ -> sprintf "cond(%s,%s,%s)" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
(* Note 2012-06-12, JS : ELet exprs are handled at the rule level *)
(*   | Syntax.ELet (i,e1,e2) -> Misc.not_implemented "Systemc.string_of_rexp: let expression" *)
  | Syntax.EArrRead (ar,idxs), _ -> ar ^ Misc.string_of_list string_of_index_exp "" idxs
  | Syntax.ECast ({Syntax.e_desc=Syntax.EConst (Const.CInt (c,_,_))}, te), ty ->
      string_of_const ~loc:loc ty (refine_int_constant ty c)
  | Syntax.ECast (e, te), ty ->
      string_of_cast_expr e ty
  | e, _ -> Misc.not_implemented ("VHDL translation of expression " ^ Syntax.string_of_exp e)

and string_of_index_exp e = "(" ^ string_of_index_expr e ^ ")"

and refine_int_constant ty c = match real_type ty with
  (* Note 2016-05-26, JS. We do not check if refinement is loss-less..
     For example, we silently accept (100:unsigned<4>).. *)
  | Tconstr({tc_name="int"}, [sg], [sz]) ->
      begin match real_type sg, size_repr sz with
      | Tconstr({tc_name="_signed"}, _, _), SzConst sz -> Const.CInt (c, Some Const.Signed, Some sz)
      | Tconstr({tc_name="_unsigned"}, _, _), SzConst sz -> Const.CInt (c, Some Const.Unsigned, Some sz)
      | Tconstr({tc_name="_signed"}, _, _), _ -> Const.CInt (c, Some Const.Signed, None)
      | Tconstr({tc_name="_unsigned"}, _, _), _ -> Const.CInt (c, Some Const.Unsigned, None)
      | _, _ -> Const.CInt (c, None, None)
      end
  | _ -> failwith "Vhdl.refine_int_constant" (* should nit happen *)

and string_of_cast_expr e ty =
  (* Note 2013-05-21, JS
     Illegal casts have has normally been caught by the type-checker.
     Effective operations are provided by overloaded VHDL fns *)
  let conv_prefix = match e.Syntax.e_desc with
    Syntax.EConst _ -> "to_"
  | _ -> "conv_" in
  match real_type ty with
  | Tconstr({tc_name="int"}, [sg], [sz]) ->
      begin match real_type sg, size_repr sz with
      | Tconstr({tc_name="_signed"}, _, _), SzConst n -> 
          conv_prefix ^ "signed(" ^ string_of_expr e ^ "," ^ string_of_int n ^ ")"
      | Tconstr({tc_name="_unsigned"}, _, _), SzConst n -> 
          conv_prefix ^ "unsigned(" ^ string_of_expr e ^ "," ^ string_of_int n ^ ")"
      | Tconstr({tc_name="_signed"}, _, _), _ -> 
          "signed(" ^ string_of_expr e ^ ")"
      | Tconstr({tc_name="_unsigned"}, _, _), _ -> 
          "unsigned(" ^ string_of_expr e ^ ")"
      | _, _ -> illegal_cast Syntax.string_of_expr e ty
      end
  | Tconstr({tc_name="float"}, _, _) ->
     "to_float(" ^ string_of_expr e ^ ")"
  | Tconstr({tc_name="bool"}, _, _) ->
     "to_bool(" ^ string_of_expr e ^ ")"
  | _ -> 
      illegal_cast Syntax.string_of_expr e ty 

and string_of_cast_guard_expr e ty =
  (* Note 2013-05-21, JS
     Illegal casts have has normally been caught by the type-checker.
     Effective operations are provided by overloaded VHDL fns *)
  let conv_prefix = match e.Interm.ge_desc with
    Interm.GEConst _ -> "to_"
  | _ -> "conv_" in
  match real_type ty with
  | Tconstr({tc_name="int"}, [sg], [sz]) ->
      begin match real_type sg, size_repr sz with
      | Tconstr({tc_name="_signed"}, _, _), SzConst n -> 
          conv_prefix ^ "signed(" ^ string_of_guard_expr e ^ "," ^ string_of_int n ^ ")"
      | Tconstr({tc_name="_unsigned"}, _, _), SzConst n -> 
          conv_prefix ^ "unsigned(" ^ string_of_guard_expr e ^ "," ^ string_of_int n ^ ")"
      | _, _ -> illegal_cast Interm.string_of_guard_expr e ty
      end
  | Tconstr({tc_name="float"}, _, _) ->
     "to_float(" ^ string_of_guard_expr e ^ ")"
  | Tconstr({tc_name="bool"}, _, _) ->
     "to_bool(" ^ string_of_guard_expr e ^ ")"
  | _ -> 
      illegal_cast Interm.string_of_guard_expr e ty 

  
and string_of_binop = function
    "&&" -> "AND"
  | "||" -> "OR"
  | "land" -> "AND";
  | "lor" -> "OR";
  | "lnand" -> "NAND";
  | "lnor" -> "NOR";
  | "lxor" -> "XOR";
  | "lxnor" -> "XNOR"
  | "!=" -> "/="
  | "%" -> "MOD"
  | "+." -> "+"
  | "-." -> "-"
  | "*." -> "*"
  | "/." -> "/"
  | "=." -> "="
  | "!=." -> "/="
  | ">." -> ">"
  | "<." -> "<"
  | ">=." -> ">="
  | "<=." -> "<="
  | op -> op   (* TO FIX : use a global symbol table in builtins for this *)

and string_of_unop = function
    "lnot" -> "NOT"
  | "not" -> "NOT"
  | "~-" -> "-"
  | "~-." -> "-"
  | op -> op  

and string_of_index_expr e = match e.Syntax.e_desc, real_type e.Syntax.e_typ with 
  Syntax.EConst (Const.CInt (i,_,_)), Tconstr({tc_name="int"}, _, _) -> string_of_int i
| _, Tconstr({tc_name="int"}, _, _) -> "to_integer(" ^ string_of_expr e ^ ")"
| _ -> Misc.fatal_error "Vhdl.string_of_index"  (* should not happen *)

and string_of_guard_expr e = match e.Interm.ge_desc with 
    Interm.GEConst c -> string_of_const e.Interm.ge_typ c 
  | Interm.GEVar v -> v
  (* Unary ops *)
  | Interm.GEApp (op,_,[e']) when Syntax.is_unop op -> string_of_unop op ^ "(" ^ string_of_guard_expr e' ^ ")"
  (* Binary ops *)
  | Interm.GEApp (">>",_,[e1;e2]) ->
      "SHIFT_RIGHT(" ^ string_of_guard_expr e1 ^ ", to_integer(" ^ string_of_guard_expr e2 ^ "))"
  | Interm.GEApp ("<<",_,[e1;e2]) ->
      "SHIFT_LEFT(" ^ string_of_guard_expr e1 ^ ", to_integer(" ^ string_of_guard_expr e2 ^ "))"
  | Interm.GEApp (op,_,[e1;e2]) when Syntax.is_binop op ->
      "(" ^ string_of_guard_expr e1 ^ ") " ^ string_of_binop op ^ " (" ^ string_of_guard_expr e2 ^ ")"
  (* Others *)
  | Interm.GEApp (op,_,es) -> op ^ "(" ^ Misc.string_of_list string_of_guard_expr "," es ^ ")"
  | Interm.GEArrRead (ar,idxs) -> ar ^ Misc.string_of_list string_of_index_guard_exp "" idxs
  | Interm.GECast (e, te) -> string_of_cast_guard_expr e e.Interm.ge_typ
  | Interm.GEBoundInp (i,Syntax.RPatVar v,ty) ->  sprintf "from_std_logic_vector(%s,%d)" i (num_size_of_type ty)
  | Interm.GEBoundInp (i,Syntax.RPatCon(cid,[{Syntax.rp_desc=Syntax.RPatVar _}]),ty) ->
      sprintf "%s.get_%s(%s)" (mangle_type ty) (String.uncapitalize cid) i
  | Interm.GEBoundVar (v,Syntax.RPatVar v',_) -> string_of_local_var v
  | _ -> Misc.not_implemented ("VHDL translation of guard expression " ^ Interm.string_of_guard_expr e)

and string_of_index_guard_exp e = "(" ^ string_of_index_guard_expr e ^ ")"

and string_of_index_guard_expr e = match e.Interm.ge_desc with 
  Interm.GEConst (Const.CInt (i,_,_)) -> string_of_int i
| _ -> "to_integer(" ^ string_of_guard_expr e ^ ")"

and string_of_local_var = function
  VSimple v -> v
| VArrLoc (v,ks) -> v ^ Misc.string_of_list string_of_array_loc "" ks

and string_of_array_loc k =  "(to_integer(" ^ string_of_expr k ^ "))"

(* Values *)

let rec string_of_val ty v = match v, real_type ty with
  | Expr.Val_int (v',_), Tconstr({tc_name="int"},[sg],[sz]) ->
      begin match real_type sg, size_repr sz with
      | Tconstr({tc_name=sg}, _, _), SzConst sz -> bitvec_of_int sg sz v'
      | _, _ -> string_of_int v'  (* TO FIX ?? *)
     end
  | Expr.Val_int (v',Some (Expr.Signed sz)), Tproduct [] ->  (* Recovery case.. *)
      bitvec_of_int "_signed" sz v'
  | Expr.Val_int (v',Some (Expr.Unsigned sz)), Tproduct [] ->  (* Recovery case.. *)
      bitvec_of_int "_unsigned" sz v'
  | Expr.Val_int (v',_), Tconstr({tc_name="int"},_,_) -> string_of_int v' (* unused *)
  | Expr.Val_bool v', _ -> string_of_bool v'
  | Expr.Val_con(cid,[]), ty ->
      if is_enum_type ty then
        cid 
      else
        sprintf "%s.mk_%s" (mangle_type ty) (String.uncapitalize cid)
  | Expr.Val_con(cid, vs), ty when List.for_all Expr.is_simple_expr_val vs ->
      sprintf "%s.mk_%s(%s)"
        (mangle_type ty)
        (String.uncapitalize cid)
        (Misc.string_of_list (string_of_val Types.no_type) ", " vs)
  | Expr.Val_array1 (sz,vs), t' ->
      let tt, _ = array_dim t' in
      if Misc.list_same (function x -> x) (Array1.to_list vs) then
        "( others => " ^ string_of_val tt vs.(0) ^ ")"
      else
        Array1.to_string ~ld:"(" ~rd:")"  (string_of_val tt) vs
  | Expr.Val_array2 (sz,vs), t' ->
      let tt, _ = array_dim t' in
      Array2.to_string ~ld:"(" ~rd:")"  (string_of_val tt) vs
  | Expr.Val_array3 (sz,vs), t' ->
      let tt, _ = array_dim t' in
      Array3.to_string ~ld:"(" ~rd:")"  (string_of_val tt) vs
  | v, _ -> Misc.not_implemented ("VHDL translation of value " ^ Expr.string_of_val v)

(* Actors *)

let rec dump_actor_interface kw oc (id,b) =
    let name = match b.ib_impl.Syntax.ai_vhdl with "" -> id | s -> s in
    fprintf oc "\n";
    fprintf oc "%s %s is\n " kw name;
    TypeVarNames.reset ();
    SizeVarNames.reset ();
    if b.ib_params <> [] then begin
      fprintf oc "  generic (\n";
      Misc.print_semic_nl oc (fun oc (id,(_,ty)) -> fprintf oc "    %s: %s" id (string_of_type ty)) b.ib_params;
      fprintf oc "\n    );\n"
    end;
    fprintf oc "  port (\n";
    List.iter (function (id,(_,ty)) ->
      fprintf oc "    %s_empty: in std_logic;\n" id;
      fprintf oc "    %s: in %s;\n" id (string_of_io_type ty);
      fprintf oc "    %s_rd: out std_logic;\n" id) b.ib_ins;
    List.iter (function (id,(_,ty)) ->
      fprintf oc "    %s_full: in std_logic;\n" id;
      fprintf oc "    %s: out %s;\n" id (string_of_io_type ty);
      fprintf oc "    %s_wr: out std_logic;\n" id) b.ib_outs;
    fprintf oc "    %s: in std_logic;\n" cfg.vhdl_clock;
    fprintf oc "    %s: in std_logic\n" cfg.vhdl_reset;
    fprintf oc "    );\n";
    fprintf oc "end %s;\n\n" (if kw = "component" then kw else name)

let comb_signals b =
    Misc.flatmap (function (i,_) -> [i; i^"_empty"]) b.ib_ins
  @ List.map (function (o,_) -> o^"_full") b.ib_outs
  @ List.fold_left 
      (fun vs (id,(v,ty,kind,qual)) ->
        if kind = Interm.IV_Regular then id::vs else vs) 
      []
      b.ib_vars

let rec dump_actor_architecture oc (name,b) =
  fprintf oc "architecture FSM of %s is\n" name;
  List.iter (dump_local_type_decl oc) b.ib_types;
(*   List.iter (dump_mem_type_decl oc) b.ib_vars; *)
    (* Note 2015-11-27, JS : all array types are now declared in [xxx_globals] package *)
  let reg_vars, other_vars =
    List.partition
      (function (_,(_,_,Interm.IV_Regular,_)) -> true | _ -> false)
      b.ib_vars in
  List.iter (dump_var_decl oc "signal") reg_vars;
  fprintf oc "begin\n";
  fprintf oc "  comb: process(%s)\n" (Misc.string_of_list (function i -> i) ", " (comb_signals b));
  List.iter (dump_var_decl oc "variable") other_vars;
  List.iter (dump_extra_var_decl oc "variable") reg_vars;
  fprintf oc "  begin\n";
  Misc.list_iter_index (dump_transition oc b) b.ib_transitions;
  fprintf oc "    else\n";
  List.iter (function (i,_) -> fprintf oc "      %s_rd <= '0';\n" i) b.ib_ins;
  List.iter (function (o,_) -> fprintf oc "      %s_wr <= '0';\n" o) b.ib_outs;
  List.iter (function (o,_) -> fprintf oc "      %s <= (others => 'X');\n" o) b.ib_outs;
  List.iter (function (v,(_,ty,_,_)) -> fprintf oc "      %s%s <= %s;\n" cfg.vhdl_sig_ena_prefix v (false_val ty)) reg_vars;
  List.iter (function (v,_) -> fprintf oc "      %s%s <= %s;\n" cfg.vhdl_sig_nxt_prefix v v) reg_vars;
  fprintf oc "    end if;\n";
  fprintf oc "  end process;\n";
  fprintf oc "  seq: process(%s, %s)\n" cfg.vhdl_clock cfg.vhdl_reset;
  fprintf oc "  begin\n";
  fprintf oc "    if (%s='0') then\n" cfg.vhdl_reset;
  List.iter (dump_var_init oc) reg_vars;
(*   List.iter (function (id,ty) -> fprintf oc "      %s_rd <= '0';\n" id) b.ib_ins; *)
(*   List.iter (function (id,ty) -> fprintf oc "      %s_wr <= '0';\n" id) b.ib_outs; *)
  fprintf oc "    elsif rising_edge(%s) then\n" cfg.vhdl_clock;
  List.iter (dump_var_update oc) reg_vars;
  fprintf oc "    end if;\n";
  fprintf oc "  end process;\n";
  fprintf oc "end FSM;\n"
 
and dump_local_type_decl oc (id, Ssval.SVEnumDefn cts) =
  fprintf oc "    type t%s is (%s);\n"  id (Misc.string_of_list (function c->c) "," cts)

and dump_var_decl oc repr (id,(v,ty,k,qual)) =
  let t', t'', v' = match real_type ty with
  | Tconstr({tc_name="array"}, [ty'],[sz]) as t ->
      let s = string_of_type t in
      s,
      Misc.replace_suffix '_' "b" s,
      begin match v, cfg.vhdl_init_array_at_decl with
        Some iv, true -> " := " ^ string_of_val t iv
      | _, _ -> ""
      end
  | Tconstr({tc_name=name},_,_) when Typing.is_variant_type name ->
      string_of_io_type ty,
      "boolean",
      ""
  | t ->
      string_of_type ty,
      "boolean",
      "" in
  fprintf oc "    %s %s : %s;\n" repr id t';
  if repr = "signal" then begin
    fprintf oc "    %s %s%s : %s%s;\n" repr cfg.vhdl_sig_nxt_prefix id t' v';
    fprintf oc "    %s %s%s : %s;\n" repr cfg.vhdl_sig_ena_prefix id t'';
    end

and dump_extra_var_decl oc repr (id,(v,ty,k,qual)) =
  match real_type ty with
  | Tconstr({tc_name="array"}, [_],[_]) as t ->
      let t' = string_of_type t in
      let t'' = Misc.replace_suffix '_' "b" t' in
      fprintf oc "    %s %s%s_tmp : %s;\n"
        repr
        cfg.vhdl_sig_nxt_prefix 
        id
        t';
      fprintf oc "    %s %s%s_tmp : %s;\n"
        repr
        cfg.vhdl_sig_ena_prefix 
        id
        t''
  | _ ->
      ()

and dump_array_var_decl oc id repr ty iv =
  match cfg.vhdl_init_array_at_decl, iv with
  | true, Some v -> fprintf oc "    %s %s : t_%s := %s;\n" repr id id (string_of_val ty v)
  | _, _ -> fprintf oc "    %s %s : t_%s;\n" repr id id

and dump_var_init oc (id,(v,ty,kind,qual)) = match v, real_type ty with
  None, _ -> ()
| Some (Expr.Val_array1 (sz,vs)), t' ->
    let tt, _ = array_dim t' in
    if not cfg.vhdl_init_array_at_decl then
      Array1.iteri 
        (fun i v -> fprintf oc "      %s(%d) %s %s;\n" id i (assign_of_kind kind) (string_of_val tt v))
        vs
| Some (Expr.Val_array2 (sz,vs)), t' ->
    let tt, _ = array_dim t' in
    if not cfg.vhdl_init_array_at_decl then
      Array2.iteri 
        (fun i1 i2 v -> fprintf oc "      %s(%d)(%d) %s %s;\n" id i1 i2 (assign_of_kind kind) (string_of_val tt v))
        vs
| Some (Expr.Val_array3 (sz,vs)), t' ->
    let tt, _ = array_dim t' in
    if not cfg.vhdl_init_array_at_decl then
      Array3.iteri 
        (fun i1 i2 i3 v -> fprintf oc "      %s(%d)(%d)(%d) %s %s;\n" id i1 i2 i3 (assign_of_kind kind) (string_of_val tt v))
        vs
| Some v, ty -> fprintf oc "      %s %s %s;\n" id (assign_of_kind kind) (string_of_val ty v)

and dump_var_update oc (id,(v,ty,kind,qual)) = 
  let delay = match cfg.vhdl_seq_delay_ns with
  | 0 -> "" 
  | n -> sprintf " after %d ns" n in
  match real_type ty with
  | Tconstr({tc_name="array"}, [_],[_]) as t ->
      let i_v = "i" in (* TO FIX ? *)  
      let en_v = cfg.vhdl_sig_ena_prefix ^ id in
      let dims = List.map get_num_size (Types.array_dims t) in
      let ndims = List.length dims in
      let sp n = String.make (6+n*2) ' ' in
      Misc.list_iter_index 
        (fun i d ->
          fprintf oc "%sfor %s%d in 0 to %d loop\n" (sp i) i_v (i+1) (d-1))
        dims;
      let sps = sp ndims in
      let indexes =
        Misc.string_of_list
          (function i -> i)
          ""
          (Misc.list_make 1 ndims (function i ->"(" ^ i_v ^ string_of_int i ^ ")")) in
      fprintf oc "%sif ( %s%s ) then\n" sps en_v indexes;
      fprintf oc "  %s%s%s <= %s%s%s%s;\n" sps id indexes cfg.vhdl_sig_nxt_prefix id indexes delay;
      fprintf oc "%send if;\n" sps;
      for i=ndims-1 downto 0 do
        fprintf oc "%send loop;\n" (sp i)
      done
  | _ ->
      fprintf oc "      if ( %s%s ) then\n" cfg.vhdl_sig_ena_prefix id;
      fprintf oc "        %s <= %s%s%s;\n" id cfg.vhdl_sig_nxt_prefix id delay;
      fprintf oc "      end if;\n"

and string_of_range_expr e = match e.Syntax.e_desc with
    Syntax.EConst (Const.CInt (v,_,_)) -> string_of_int v
  | Syntax.EVar v -> v
  | _ -> Error.illegal_range_expression e.Syntax.e_loc

and assign_of_kind = function
    Interm.IV_Regular -> "<="
  | Interm.IV_Pattern -> ":="
  | Interm.IV_Let -> ":="

and dump_transition oc b j ((conds,actions) as t) =
  fprintf oc "    -- %s\n" (Interm.string_of_transition t);
  fprintf oc "    %s %s then\n" (if j=0 then "if" else "elsif") (Misc.string_of_list string_of_cond " and " conds);
  let touched_outputs, touched_inputs, touched_vars, touched_array_locs =
    List.fold_left (dump_action oc) ([],[],[],[]) actions in
  List.iter
    (function (o,_) ->
      if not (List.mem o touched_outputs) then begin
        fprintf oc "      %s <= (others => 'X');\n" o;
        fprintf oc "      %s_wr <= '0';\n" o
        end)
    b.ib_outs;
  List.iter
    (function (i,_) ->
      if not (List.mem i touched_inputs) then begin
        fprintf oc "      %s_rd <= '0';\n" i
        end)
    b.ib_ins;
  List.iter
    (fun (v,(_,ty,kind,_)) ->
      if kind = Interm.IV_Regular && not (List.mem v touched_vars) then begin
        fprintf oc "      %s%s <= %s;\n" cfg.vhdl_sig_nxt_prefix v v;
        fprintf oc "      %s%s <= %s;\n" cfg.vhdl_sig_ena_prefix v (false_val ty)
        end)
    b.ib_vars;
  if touched_array_locs <> [] then
    List.iter
      (function
        (v,(((ks,_,_)::_) as kss)) ->
          let n_v = cfg.vhdl_sig_nxt_prefix ^ v in
          let en_v = cfg.vhdl_sig_ena_prefix ^ v in
          let ndims = List.length ks in (* Note 2015-11-27, JS: should be derived from the type of [v] .. *)
          fprintf oc "      %s_tmp := %s;\n" n_v v;
          fprintf oc "      %s_tmp := %s;\n" en_v (init_dim_array "false" ndims);
          List.iter
            (function
                ks,exp,ty ->
                    fprintf oc "      %s_tmp%s := %s;\n" n_v (string_of_indexes ks) (string_of_exp ty exp);
                    fprintf oc "      %s_tmp%s := true;\n" en_v (string_of_indexes ks))
            kss;
          fprintf oc "      %s <= %s_tmp;\n" n_v n_v;
          fprintf oc "      %s <= %s_tmp;\n" en_v en_v
       | (_, []) ->
          failwith "Vhdl.dump_transition" (* should not happen *))
      (Misc.assoc_from_list touched_array_locs)

and string_of_indexes ks = Misc.string_of_list (function k -> "(to_integer(" ^ string_of_expr k ^ "))") "" ks

and string_of_cond c = match c with
    Interm.CInpRdy (i, ty) ->
      sprintf "%s_empty='0'" i
  | Interm.COutRdy (o, ty) ->
      sprintf "%s_full='0'" o
  | Interm.CInpMatch (i, Syntax.RPatConst c, ty) ->
      sprintf "from_std_logic_vector(%s,%d)=%s" i (num_size_of_type ty) (string_of_const ty c)
  | Interm.CInpMatch (i, Syntax.RPatCon(cid,[]), ty) ->
      sprintf "%s.is_%s(%s)" (mangle_type ty) (String.uncapitalize cid) i
  | Interm.CInpMatch (i, Syntax.RPatCon(cid,[{Syntax.rp_desc=Syntax.RPatConst c}]), ty) ->
      let ty' = begin match real_type ty with
        Tconstr(_, [ty'],_) -> real_type ty'
      | _ -> failwith ("Vhdl.string_of_cond: cannot handle multi-parameter type ctors") end in
      sprintf "%s.is_%s(%s) and %s.get_%s(%s)=%s"
        (mangle_type ty) (String.uncapitalize cid) i
        (mangle_type ty) (String.uncapitalize cid) i (string_of_const ty' c)
  | Interm.CInpMatch (i, Syntax.RPatCon(cid,ps), ty) when List.for_all Syntax.is_simple_rule_pattern ps ->
      begin match Misc.string_of_indexed_list (string_of_pattern_cond i cid ty) " and " ps with
        "" -> sprintf "%s.is_%s(%s)" (mangle_type ty) (String.uncapitalize cid) i
      | s  -> sprintf "%s.is_%s(%s) and %s" (mangle_type ty) (String.uncapitalize cid) i s
      end
  | Interm.CVarMatch (v, Syntax.RPatConst c, ty) ->  sprintf "%s=%s" (string_of_local_var v) (string_of_const ty c)
  | Interm.CVarMatch (v, Syntax.RPatCon(c,[]), ty) ->  sprintf "%s=%s" (string_of_local_var v) c (* enums *)
  | Interm.CVarMatch (VSimple v, Syntax.RPatCon(cid,ps), ty) when List.for_all Syntax.is_simple_rule_pattern ps ->
      begin match Misc.string_of_indexed_list (string_of_pattern_cond v cid ty) " and " ps with
        "" -> sprintf "%s.is_%s(%s)" (mangle_type ty) (String.uncapitalize cid) v
      | s  -> sprintf "%s.is_%s(%s) and %s" (mangle_type ty) (String.uncapitalize cid) v s
      end
  | Interm.CGuardExp e -> sprintf "(%s)" (string_of_guard_expr e)
  | _ -> Misc.not_implemented
        ("Vhdl.string_of_cond: cannot translate rule condition [" ^ Interm.string_of_cond c ^ "] to VHDL")

and string_of_pattern_cond name cid ty j p = match p.Syntax.rp_desc with
  Syntax.RPatConst c -> 
      sprintf "%s.get_%s_field_%d(%s)=%s"
        (mangle_type ty) (String.uncapitalize cid) j name (string_of_const ~loc:p.Syntax.rp_loc p.Syntax.rp_typ c)
| Syntax.RPatWild -> ""
| Syntax.RPatVar _ -> ""
| Syntax.RPatCon _ -> failwith "Vhdl.string_of_pattern_cond" (* should not happen *)


and dump_action oc (outps,inps,vars,valocs) a = match a with
  | Interm.AReadInp (i,ty) ->
      fprintf oc "      %s_rd <= '1';\n" i;
      (outps,i::inps,vars,valocs)
  | Interm.ABindInp (i,Syntax.RPatVar v,ty) ->
      fprintf oc "      %s := from_std_logic_vector(%s,%d);\n" v i (num_size_of_type ty);
      fprintf oc "      %s_rd <= '1';\n" i;
      (outps,i::inps,vars,valocs)
  | Interm.ABindInp (i,Syntax.RPatCon(cid,[{Syntax.rp_desc=Syntax.RPatVar v}]),ty) ->
      fprintf oc "      %s := %s.get_%s(%s);\n" v (mangle_type ty) (String.uncapitalize cid) i;
      fprintf oc "      %s_rd <= '1';\n" i;
      (outps,i::inps,vars,valocs)
  | Interm.ABindInp (i,Syntax.RPatCon(cid,ps),ty) when List.for_all Syntax.is_simple_rule_pattern ps ->
      Misc.list_iter_index
        (fun j p ->
          match p.Syntax.rp_desc with
            | Syntax.RPatConst _ ->
                ()
            | Syntax.RPatWild ->
                ()
            | Syntax.RPatVar v ->
                fprintf oc "      %s := %s.get_%s_field_%d(%s);\n" v (mangle_type ty) (String.uncapitalize cid) j i
            | Syntax.RPatCon (_,_) ->
                failwith "Vhdl.dump_action: illegal pattern binding" (* should not happen *))
        ps;
      fprintf oc "      %s_rd <= '1';\n" i;
      (outps,i::inps,vars,valocs)
  | Interm.ABindVar (v, Syntax.RPatVar v',ty) ->
      fprintf oc "      %s := %s;\n" v' (string_of_local_var v);
      (outps,inps,vars,valocs)
  | Interm.ABindVar (v, Syntax.RPatCon(cid,[{Syntax.rp_desc=Syntax.RPatVar v'}]),ty) ->
      fprintf oc "      %s <= %s.get_%s(%s);\n" v' (mangle_type ty) (String.uncapitalize cid) (string_of_local_var v);
      (outps,inps,vars,valocs)
  | Interm.ABindVar (VSimple v,Syntax.RPatCon(cid,ps),ty) when List.for_all Syntax.is_simple_rule_pattern ps ->
      Misc.list_iter_index
        (fun j p ->
          match p.Syntax.rp_desc with
            | Syntax.RPatConst _ ->
                ()
            | Syntax.RPatWild ->
                ()
            | Syntax.RPatVar v' ->
                fprintf oc "      %s := %s.get_%s_field_%d(%s);\n" v' (mangle_type ty) (String.uncapitalize cid) j v
            | Syntax.RPatCon (_,_) ->
                failwith "Vhdl.dump_action: illegal pattern binding" (* should not happen *))
        ps;
      (outps,inps,vars,valocs)
  (* Special handling of top-level conditionals *)
  | Interm.AWriteOut (o,Syntax.ECond(e1,e2,e3),ty) ->
      fprintf oc "      if ( %s ) then %s <= %s; else %s <= %s; end if;\n"
        (string_of_expr e1) o (string_of_output_exp ty e2.Syntax.e_desc) o (string_of_output_exp ty e3.Syntax.e_desc);
      fprintf oc "      %s_wr <= '1';\n" o;
      (o::outps,inps,vars,valocs)
  | Interm.AWriteVar (Interm.VSimple _ as v, Syntax.ECond(e1,e2,e3),ty) ->   (* TO FIX when ty(v) is array ! *)
      let v' = string_of_local_var v in
      let v'' = cfg.vhdl_sig_nxt_prefix ^ v' in
      fprintf oc "      if ( %s ) then %s <= %s; else %s <= %s; end if;\n"
        (string_of_expr e1) v'' (string_of_expr e2) v'' (string_of_expr e3);
      fprintf oc "      %s%s <= true;\n" cfg.vhdl_sig_ena_prefix v';
      (outps,inps,v'::vars,valocs)  
  (* Special handling of let exprs - they have been lifted up in the interm repr *)
  | Interm.AWriteOut (o, Syntax.ELet(bs, e2), ty) ->
      List.iter (function (v,e1) -> fprintf oc "      %s := %s;\n" v (string_of_expr e1)) bs;
      fprintf oc "      %s <= %s;\n" o (string_of_output_exp ty e2.Syntax.e_desc);
      fprintf oc "      %s_wr <= '1';\n" o;
      (o::outps,inps,vars,valocs)
  | Interm.AWriteVar (Interm.VSimple _ as v, Syntax.ELet(bs, e2), ty) ->   (* TO FIX when ty(v) is array ! *)
      let v' = string_of_local_var v in
      List.iter (function (v,e1) -> fprintf oc "    %s:=%s;\n" v (string_of_expr e1)) bs;
      fprintf oc "      %s%s <= %s;\n" cfg.vhdl_sig_nxt_prefix v' (string_of_expr e2);
      fprintf oc "      %s%s <= true;\n" cfg.vhdl_sig_ena_prefix v';
      (outps,inps,v'::vars,valocs)
  (* Other cases *)
  | Interm.AWriteOut (o,exp,ty) ->
      fprintf oc "      %s <= %s;\n" o (string_of_output_exp ty exp);
      fprintf oc "      %s_wr <= '1';\n" o;
      (o::outps,inps,vars,valocs)
  | Interm.AWriteVar (VSimple v, exp,ty) ->
      fprintf oc "      %s%s <= %s;\n" cfg.vhdl_sig_nxt_prefix v (string_of_exp ty exp);
      fprintf oc "      %s%s <= true;\n" cfg.vhdl_sig_ena_prefix v;
      (outps,inps,v::vars,valocs)
  | Interm.AWriteVar (Interm.VArrLoc (v,ks), exp,ty) ->
      (outps,inps,v::vars,(v,(ks,exp,ty))::valocs)
  | _ -> Misc.not_implemented
        ("Vhdl.dump_action: cannot translate rule action [" ^ Interm.string_of_action a ^ "] to VHDL")

and string_of_output_exp ty exp =
  let e = string_of_exp ty exp in
  match real_type ty with
  | Tconstr({tc_name="int"}, _, _) -> sprintf "std_logic_vector(%s)" e
  | Tconstr({tc_name="bool"}, _, _) -> sprintf "to_std_logic_vector(%s,1)" e
  | Tconstr({tc_name="float"}, _, _) -> sprintf "to_slv(%s)" e
  | Tconstr({tc_name=name}, _, _) when Typing.is_variant_type  name -> sprintf "%s" e
  | _ -> failwith "Vhdl.string_of_output_exp: illegal io type"

and dump_array_update oc a (idx,exp) =
  fprintf oc "            %s(%s) <= %s;\n" a (Misc.string_of_list string_of_index_expr "," idx) (string_of_expr exp)

let dump_libraries oc libs =
  let libnames = List.fold_left
      (fun ps l ->
        let p = Misc.prefix '.' Error.invalid_lib_name l in
        if List.mem p ps then ps else ps@[p])
      []
      libs in
  fprintf oc "library %s;\n" (Misc.string_of_list (function l -> l) "," libnames);
  List.iter (fprintf oc "use %s;\n") libs

let dump_actor prefix profil ir (id,a) =
  if a.Interm.ia_impl.Syntax.ai_vhdl <> "" then begin
    let f = Misc.prefix_dir Genmake.target.Genmake.dir a.Interm.ia_impl.Syntax.ai_vhdl ^ ".vhd" in
    Misc.check_file f;
    Genmake.add_target ~extra:true f
    end
  else
    let dump_inst mono ((ty,fns), (b, bids)) =
      let name =
        if mono then b.ib_name ^ "_act" 
        else Mangling.string_of_name b.ib_name [ty] [] fns ^ "_act" in
      let fname = name ^ ".vhd" in
      let fname' = Misc.prefix_dir Genmake.target.Genmake.dir fname in
      let oc = Misc.open_out fname' in
      dump_banner oc;
      dump_libraries oc
        ( (if profil.use_floats then cfg.vhdl_core_fp_libs else cfg.vhdl_core_libs)
          @ [cfg.vhdl_num_lib]
          @ (if profil.use_floats then cfg.vhdl_fp_libs else [])
          @ (if ((profil.has_globals || profil.variant_types <> []) && not cfg.vhdl_generate_qip) then ["work.all"] else []) );
          (* Adding "use work.all" seems to cause problems when compiling under Quartus 13.1.. *)
      if profil.has_externs then fprintf oc "use %s.all;\n" cfg.vhdl_extfns_package;
      if profil.has_globals then fprintf oc "use work.%s_%s.all;\n" prefix cfg.vhdl_global_suffix;
      dump_actor_interface "entity" oc (name,b);
      dump_actor_architecture oc (name,b);
      Logfile.write fname';
      close_out oc in
    begin match ActInsts.to_list a.ia_insts with
      [] ->
        ()
    | [inst] -> (* only one (box) instance, no need to generate unique names *)
        dump_inst true inst
    | insts ->  (* several (box) instances *)
        List.iter (dump_inst false) insts
    end

(* Split actors *)

let dump_split_actor_interface kw oc fanout  =
  let name = "split" ^ string_of_int fanout in
  fprintf oc "%s %s is\n" kw name;
  fprintf oc "  generic ( size: integer := 10);\n";
  fprintf oc "  port (  \n";
  fprintf oc "       d_f:    out std_logic;\n";
  fprintf oc "       d :     in std_logic_vector (size-1 downto 0);\n";
  fprintf oc "       d_wr :  in std_logic;\n";
  for i=1 to fanout do
    fprintf oc "       d%d_f :  in std_logic;\n" i; 
    fprintf oc "       d%d :    out std_logic_vector(size-1 downto 0);\n" i; 
    fprintf oc "       d%d_wr : out std_logic%s\n" i (if i<fanout then ";" else "");
    done;
  fprintf oc "       );\n";
  fprintf oc "end %s;\n\n" (if kw = "component" then kw else name)

let dump_split_actor_architecture oc fanout  =
  fprintf oc "architecture arch of split%d is\n" fanout;
  fprintf oc "begin\n";
  for i=1 to fanout do
    fprintf oc "  d%d <= d;\n" i
  done;
  for i=1 to fanout do
    fprintf oc "  d%d_wr <= d_wr;\n" i
  done;
  fprintf oc "  d_f <= %s;\n"
    (Misc.string_of_list
       (function i -> "d" ^ i ^ "_f")
       " or "
       (Misc.list_make 1 fanout string_of_int));
  fprintf oc "end arch;\n";
  fprintf oc "\n"

let dump_split_actor oc fanout =
  fprintf oc "library ieee;\n";
  fprintf oc "use ieee.std_logic_1164.all;\n\n";
  dump_split_actor_interface "entity" oc fanout;
  fprintf oc "\n";
  dump_split_actor_architecture oc fanout;
  fprintf oc "\n"

let dump_split_actors fname fanouts =
  let fname' = Misc.prefix_dir Genmake.target.Genmake.dir fname in
  let oc = Misc.open_out fname' in
  Misc.dump_banner "--" oc;
  List.iter (dump_split_actor oc) fanouts;
  close_out oc;
  Logfile.write fname'

(* Dumping of network structure *)

let rec dump_network prefix tp ir =
  let name = prefix ^ "_net" in
  let fname = name ^ ".vhd"  in
  let fname' = Misc.prefix_dir Genmake.target.Genmake.dir fname in
  let anns =
    if Sys.file_exists cfg.vhdl_annot_file then begin
      Printf.printf "Reading annotation file %s\n" cfg.vhdl_annot_file;
      Vhdl_annot.parse cfg.vhdl_annot_file end
    else begin
      eprintf "warning: VHDL annotation file %s does not exist.\n" cfg.vhdl_annot_file;
      [] end in
  let has_externs = ir.Interm.ir_extfns != [] in
  let has_globals = ir.Interm.ir_consts != [] || ir.Interm.ir_globfns != [] in
  let array_types, variant_types = List.fold_left extract_user_defined_types ([],[]) ir.Interm.ir_boxes in
  let input_variant_types, output_variant_types = List.fold_left extract_io_variant_types ([],[]) ir.Interm.ir_boxes in
  let actor_instances =
    Misc.flatmap
      (function (id,a) ->
        match ActInsts.to_list a.ia_insts with
          [] -> []
        | [(ty,fns),(b,_)] -> [b.ib_name ^ "_act", b]
        | bs -> List.map (function | (ty,fns), (b,_) -> Mangling.string_of_name b.ib_name [ty] [] fns ^ "_act", b) bs)
      ir.ir_actors in
  let splitters = List.fold_left Interm.extract_split_boxes [] ir.Interm.ir_boxes in
  let use_floats = List.fold_left check_floats false actor_instances in
  let oc = Misc.open_out fname' in
  let wire_ins, wire_outs, renamed_wires = extract_io_wires ir in
  dump_banner oc;
  dump_libraries oc
    ( (if use_floats then cfg.vhdl_core_fp_libs else cfg.vhdl_core_libs)
    @ [cfg.vhdl_num_lib]
    @ (if use_floats then cfg.vhdl_fp_libs else [])
    @ (if has_globals || array_types <> [] || variant_types <> [] then ["work.all"] else []) );
  if array_types != [] then fprintf oc "use work.%s_%s.all;\n" prefix cfg.vhdl_global_suffix;
  fprintf oc "\n";
  dump_network_intf "entity" oc name wire_ins wire_outs;
  fprintf oc "\n";
  fprintf oc "architecture %s of %s is\n" cfg.vhdl_arch_tag name;
  List.iter (dump_actor_interface "component" oc) actor_instances;
  fprintf oc "\n";
  List.iter (dump_split_actor_interface "component" oc) splitters;
  fprintf oc "\n";
  List.iter (dump_wire ir.Interm.ir_boxes oc) ir.Interm.ir_wires;
  fprintf oc "\n";
  fprintf oc "begin\n";
  let wire_name = mk_wire_name_fn renamed_wires in
  List.iter (dump_box oc ir.Interm.ir_actors anns wire_name) ir.Interm.ir_boxes;
  fprintf oc "end %s;\n" cfg.vhdl_arch_tag;
  Logfile.write fname';
  close_out oc;
  { has_externs = has_externs;
    has_globals =  has_globals || array_types <> [];   (* Global consts (including arrays) and fns *)
    use_floats = use_floats;
    array_types = array_types;
    variant_types = variant_types;
    input_variant_types = input_variant_types;
    output_variant_types = output_variant_types;
    splitters = splitters },
  actor_instances

and mk_wire_name_fn renamed_wires =
  if cfg.vhdl_rename_io_wires
  then
      function wid ->
        if List.mem_assoc wid renamed_wires
        then List.assoc wid renamed_wires
        else default_wire_name wid
  else
    default_wire_name

and extract_io_wires ir = List.fold_left (extract_io_wire ir.Interm.ir_boxes) ([],[],[]) ir.Interm.ir_wires

and extract_io_wire boxes (wins,wouts,renamed) (wid,(((src,_),(dst,_)),ty)) =
  if is_inp_box boxes src then
    if cfg.vhdl_rename_io_wires then
      let name' = (Interm.find_box boxes src).ib_name in
      (name',ty)::wins, wouts, (wid,name')::renamed
    else
      (default_wire_name wid,ty)::wins, wouts, renamed
  else if is_out_box boxes dst then
    if cfg.vhdl_rename_io_wires then
      let name' = (Interm.find_box boxes dst).ib_name in
      wins, (name',ty)::wouts, (wid,name')::renamed
    else
      wins, (default_wire_name wid,ty)::wouts, renamed
  else wins, wouts, renamed

and extract_user_defined_types (array_types,variant_types) (id,b) =
  List.fold_left
    add_user_defined_type
    (array_types,variant_types)
    (  List.map (function (_,(_,ty)) -> ty) b.ib_params
     @ List.map (function (_,(_,ty)) -> ty) b.ib_ins
     @ List.map (function (_,(_,ty)) -> ty) b.ib_outs
     @ List.map (function (_,(_,ty,_,_)) -> ty) b.ib_vars)

and extract_io_variant_types (inps,outps) (id,b) =
  match b.ib_tag, Types.real_type b.ib_typ  with
    InpB _, Tconstr({tc_name=name}, ts, ss) when Typing.is_variant_type name ->
      let id = Mangling.string_of_name name ts ss [] in
      let vd = mk_variant_desc name ts ss in
      (id, vd) :: inps, outps
  | OutB _, Tconstr({tc_name=name}, ts, ss) when Typing.is_variant_type name ->
      let id = Mangling.string_of_name name ts ss [] in
      let vd = mk_variant_desc name ts ss in
      inps, (id, vd) :: outps
  | _, _ -> inps, outps

and add_user_defined_type (array_types,variant_types) ty =
  let add ts (n,t) = if List.mem_assoc n ts then ts else (n,t) :: ts in
  match real_type ty with
  | Tconstr({tc_name="array"}, [ty'],[sz]) as ty ->
      add array_types (mangle_type ty, mk_array_desc ty' sz), variant_types
(*   | Tconstr({tc_name="array2"}, [ty';sz]) -> *)
(*       add (vhdl_type_of ty', let s1,s2 = get_array_sizes "add_array_type" sz in [s1;s2])  *)
  | Tconstr({tc_name=name}, ts, ss) when Typing.is_variant_type name -> 
      array_types, add variant_types (Mangling.string_of_name name ts ss [], mk_variant_desc name ts ss)
  | _ ->
      array_types, variant_types

and check_floats r (id,a) = 
     List.exists (function (_,(_,ty)) -> is_float_type ty) a.ib_ins
  || List.exists (function (_,(_,ty)) -> is_float_type ty) a.ib_outs
  || List.exists (function (_,(_,ty)) -> is_float_type ty) a.ib_params
  || List.exists (function (_,(_,ty,_,_)) -> is_float_type ty) a.ib_vars

and dump_network_intf kw oc name wire_ins wire_outs =
  fprintf oc "%s %s is\n" kw name;
(*   fprintf oc "  generic (\n"; *)
(*   Misc.print_semic_nl oc (fun oc (id,ty,v) -> fprintf oc "    %s: %s := %s" id ty v) cfg.vhdl_default_generics; *)
(*   fprintf oc "\n    );\n"; *)
  fprintf oc "  port (\n";
  List.iter (function (id,ty) ->
    fprintf oc "    %s_f: out std_logic;\n" id;
    fprintf oc "    %s: in %s;\n" id (string_of_io_type ty);
    fprintf oc "    %s_wr: in std_logic;\n" id) wire_ins;
  List.iter (function (id,ty) ->
    fprintf oc "    %s_e: out std_logic;\n" id;
    fprintf oc "    %s: out %s;\n" id (string_of_io_type ty);
    fprintf oc "    %s_rd: in std_logic;\n" id) wire_outs;
  fprintf oc "    %s: in std_logic;\n" cfg.vhdl_clock;
  fprintf oc "    %s: in std_logic\n" cfg.vhdl_reset;
  fprintf oc "    );\n";
  fprintf oc "end %s;\n" (if kw = "component" then kw else name)

and dump_wire boxes oc (wid,(((src,_),(dst,_)),ty)) =
  let w = default_wire_name wid in
  if not (is_inp_box boxes src) && not (is_out_box boxes dst) then begin
     (* Do not declare signals for wires connected to stream/port ios *)
    if is_fifo_box boxes src or is_buffer_box boxes src then begin
      fprintf oc "signal %s_e : std_logic;\n" w;
      fprintf oc "signal %s : %s;\n" w (string_of_io_type ty);
      fprintf oc "signal %s_rd : std_logic;\n" w;
      end
    else if is_fifo_box boxes dst || is_split_box boxes dst || is_buffer_box boxes dst then begin
      fprintf oc "signal %s_f : std_logic;\n" w;
      fprintf oc "signal %s : %s;\n" w (string_of_io_type ty);
      fprintf oc "signal %s_wr : std_logic;\n" w
      end
    else  (* should not happen since fifos have been inserted btw actors *)
      fprintf oc "signal %s : %s;\n" w (string_of_io_type ty);
    end

and is_inp_box boxes bid = test_box_kind boxes (function b -> match b.ib_tag with InpB _ -> true | _ -> false) bid
and is_out_box boxes bid = test_box_kind boxes (function b -> match b.ib_tag with OutB _ -> true | _ -> false) bid
and is_fifo_box boxes bid = test_box_kind boxes (function b -> b.ib_tag = RegularB && b.ib_name = "fifo") bid
and is_buffer_box boxes bid = test_box_kind boxes (function b -> b.ib_tag = RegularB && b.ib_name = "buffer") bid
and is_split_box boxes bid = test_box_kind boxes (function b -> b.ib_tag = RegularB && b.ib_name = "_split") bid

and test_box_kind boxes f bid = 
  try 
    let b = List.assoc bid boxes in
    f b
  with Not_found -> Misc.fatal_error "Vhdl.test_box_kind: cannot retrieve box from id"

and dump_trace_signal oc (bid,box) =
  match box.ib_tag, box.ib_name with
    RegularB,  "fifo" -> fprintf oc "signal %s : integer;\n" (fifo_cnt_name bid);
  | RegularB,  "_split" -> ()
  | _, _ -> ()

and dump_box oc actors annots wire_name (bid,box) =
  match box.ib_tag with
    RegularB when box.ib_name = "fifo" ->
      let fid = "F" ^ string_of_int bid in
      let sz = begin try
        match List.assoc box.ib_wsub annots with 
          (* Note 2011-06-10, JS : in the annotation file, FIFOs are refered to by name of the original wire *)
          (* Note 2017-06-27, JS : the annotation tag "fifo_max_occ" is now deprecated *)
        | "fifo_size", n -> n + cfg.vhdl_fifo_offset
        | p, _ -> Error.unknown_annotation cfg.vhdl_annot_file fid p; cfg.vhdl_default_fifo_capacity
      with Not_found -> Printf.printf "Reverting to default size for fifo %s\n" fid; cfg.vhdl_default_fifo_capacity end in
      let model = 
        if sz >= cfg.vhdl_fifo_model_threshold
        then cfg.vhdl_big_fifo_model
        else cfg.vhdl_small_fifo_model in
      fprintf oc "  %s: %s generic map (%d,%d) port map(%s);\n"
          fid
          model
          sz
          (box_io_width box)
          (string_of_fifo_ios wire_name (bid,box))
  | RegularB when box.ib_name = "buffer" ->
      let fid = "G" ^ string_of_int bid in
      fprintf oc "  %s: %s generic map (%d) port map(%s);\n"
            fid
            cfg.vhdl_buffer_model
            (box_io_width box)
            (string_of_fifo_ios wire_name (bid,box))
  | RegularB when box.ib_name = "_split" ->
      let fanout = List.length box.ib_outs in
      fprintf oc "  S%d: split%d generic map (%d) port map(%s);\n"
        bid
        fanout
        (box_io_width box)
        (string_of_split_ios wire_name (bid,box))
  | RegularB ->
      let name =
        begin match box.ib_impl.Syntax.ai_vhdl, box.ib_ninsts with
          "", n when n<2 -> box.ib_name ^ "_act" 
        | "", n -> Mangling.string_of_name box.ib_name [box.ib_tysig] [] (List.map snd box.ib_fpbs) ^ "_act"
        | s, _ -> s
        end in
      fprintf oc "  B%d: %s %s port map(%s,%s%s,%s);\n"    (* TO FIX : size param ! *)
        bid
        name
        (string_of_params box)
        (string_of_box_ios wire_name box)
        ""
        cfg.vhdl_clock
        cfg.vhdl_reset
  | InpB _
  | OutB _ -> () (* IO boxes are not included in the generated network. They will be instanciated in the testbench *)
  | DummyB ->  Misc.fatal_error "Vhdl.dump_box: dummy box"

and box_io_width b = match b.ib_ins with
  [_,(_,ty)] -> 
    begin match size_of_type ty with
      VS_Const sz -> sz
    | _ -> Misc.fatal_error "Vhdl.fifo_or_split_size: cannot retrieve width of input channel data" (* should not happen *) end
| _ -> Misc.fatal_error "Vhdl.fifo_or_split_size: cannot retrieve width of input channel data" (* should not happen *)

and string_of_params b = match b.ib_params with
  [] -> ""
| params -> sprintf "generic map (%s)" (Misc.string_of_list string_of_param_value "," params)

and string_of_param_value (name, (v,ty)) = match (v,real_type ty) with
  | Expr.Val_int (n,_), Tconstr({tc_name="int"}, [sg],[sz]) ->
      begin match real_type sg, size_repr sz with
      | Tconstr({tc_name=sg}, _, _), SzConst sz -> bitvec_of_int sg sz n
      | _, _ -> string_of_int n  (* TO FIX ?? *)
      end
  | Expr.Val_bool b, Tconstr({tc_name="bool"}, _, _) -> string_of_bool b
  | Expr.Val_float f, Tconstr({tc_name="float"}, _, _) -> "\"" ^  Misc.bits_of_float f ^ "\""
  | Expr.Val_array1 (sz,vs), t' ->
      let tt, _ = array_dim t' in
      string_of_array_param_value vs tt
  | Expr.Val_array2 (sz,vs), t' ->
      let tt, _ = array_dim t' in
      let vs' = Array1.mapi (fun i v -> (i,v)) vs in
      Array1.to_string ~ld:"(" ~rd:")" (fun (i,v) -> string_of_int i ^ "=>" ^ string_of_array_param_value v tt) vs'
  | Expr.Val_array3 (sz,vs), t' ->
      Misc.not_implemented "VHDL : 3D arrays as parameters"
  | _ -> Misc.fatal_error "Vhdl.string_of_param_value"

and string_of_array_param_value vs t =
  let vs' = Array1.mapi (fun i v -> (i,v)) vs in
  Array1.to_string ~ld:"(" ~rd:")" (fun (i,v) -> string_of_int i ^ "=>" ^ string_of_param_value ("",(v,t))) vs'

and string_of_box_ios wire_name box =
    Misc.string_of_list (string_of_box_inp wire_name)  "," box.ib_ins
  ^ ","
  ^ Misc.string_of_list (string_of_box_outp wire_name) "," box.ib_outs

(* Each wire at the box level gives 3 wires at the VHDL level.
 *
 * For regular (actor) boxes :
 *        +--------+                   w_e  +--------+ w'_f  
 *        |        |                 ------>|        |<------
 *     w  |        | w'                w    |        | w'
 *   ---->|   A    |---->    ==>     ------>|   A    |------>
 *        |        |                   w_rd |        | w'_wr 
 *        |        |                 <------|        |------>
 *        +--------+                        +--------+      
 *
 * For FIFOs :
 *        +--------+                   w_f  +--------+ w'_e  
 *        |        |                 <------|        |------>
 *     w  |        | w'                w    |        | w'
 *   ---->|  fifo  |---->    ==>     ------>|  fifo  |------>
 *        |        |                   w_wr |        | w'_rd 
 *        |        |                 ------>|        |<------
 *        +--------+                        +--------+      
*)
   

and string_of_box_inp wire_name (id,(wid,ty)) = 
  let w = wire_name wid in
  sprintf "%s_e,%s,%s_rd" w w w

and string_of_box_outp wire_name = function
  | (id,([wid],ty)) -> 
      let w = wire_name wid in
      sprintf "%s_f,%s,%s_wr" w w w
  | _ -> Misc.fatal_error "Vhdl.string_of_box_outp: box output is connected to more than a wire"
        (* should not happen after splitters insertion *)

and string_of_fifo_ios wire_name (bid,box) =
  match box.ib_ins, box.ib_outs with
    [id,(wid,ty)], [id',([wid'],ty')] -> 
      let w = wire_name wid in
      let w' = wire_name wid' in
      if cfg.vhdl_trace then
        sprintf "%s_f,%s,%s_wr,%s_e,%s,%s_rd,%s,%s,%s" w w w w' w' w' cfg.vhdl_clock cfg.vhdl_reset (fifo_cnt_name bid)
      else
        sprintf "%s_f,%s,%s_wr,%s_e,%s,%s_rd,%s,%s" w w w w' w' w' cfg.vhdl_clock cfg.vhdl_reset;
  | _ -> Misc.fatal_error "Vhdl.string_of_fifo_ios: invalid ios for fifo" (* should not happen *)

and fifo_cnt_name bid = "fcnt" ^ (string_of_int bid)

and string_of_split_ios wire_name (bid,box) =
  match box.ib_ins, box.ib_outs with
    [id,(wid,ty)], bouts -> 
      let w = wire_name wid in
      sprintf "%s_f,%s,%s_wr,%s" w w w (string_of_split_outputs wire_name bouts);
  | _ -> Misc.fatal_error "Vhdl.string_of_split_ios: invalid ios for split" (* should not happen *)

and string_of_split_outputs wire_name bouts =
  let string_of_split_output = function 
      id,([wid],ty) -> let w = wire_name wid in sprintf "%s_f,%s,%s_wr" w w w
    | _ -> Misc.fatal_error "Vhdl.string_of_split_outputs: invalid output for split" in (* should not happen *) 
  Misc.string_of_list string_of_split_output "," bouts

(* Dumping global constants and functions - and also array type decls *)

let rec dump_globals prefix ir profil =
  let modname = prefix ^ "_globals" in
  let fname = modname ^ ".vhd" in
  let fname' = Misc.prefix_dir Genmake.target.Genmake.dir fname in
  let oc = Misc.open_out fname' in
  dump_banner oc;
  dump_libraries oc
    ( (if profil.use_floats then cfg.vhdl_core_fp_libs else cfg.vhdl_core_libs)
        @ [cfg.vhdl_num_lib]
        @ (if profil.use_floats then cfg.vhdl_fp_libs else []) );
  fprintf oc "\n";
  dump_globals_intf oc modname ir profil.array_types ir.Interm.ir_consts;
  fprintf oc "\n";
  dump_globals_impl oc modname ir;
  Logfile.write fname';
  close_out oc;

and dump_globals_intf oc package_name ir array_types true_consts =
  let array_types1 = List.fold_left add_const_array_type array_types ir.Interm.ir_consts in
     (* Add non-explicitely declared array types *)
  let array_types2 = List.fold_left expand_array_type array_types1 array_types1 in
     (* Explicit sub-array types for 2D and 3D arrays *)
  let array_types3 = List.fold_left (add_companion_array Boolean) array_types2 array_types2 in
     (* Add corresponding arrays of std_logic for update enable signals *)
  let array_types4 = Misc.insert_sort (fun (n,_) (n',_) -> Misc.string_is_suffix n n') array_types3 in
     (* Dependency sort *)
  fprintf oc "package %s is\n" package_name;
  List.iter (dump_global_array_type_decl oc) array_types4;
  List.iter (dump_global_const oc) true_consts;
  List.iter (dump_global_fn_sig oc) ir.Interm.ir_globfns; 
  fprintf oc "end %s;\n" package_name

and add_const_array_type types (id,c) =
  match real_type c.gc_typ with
  | Tconstr({tc_name="array"}, [ty'], [sz]) as ty ->
      Misc.assoc_add types (mangle_type ty, mk_array_desc ty' sz)
  | _ -> types

and expand_array_type types (id,arr_desc) =
  match arr_desc with
  | Array (t', sz') as t, sz ->
      let id' = string_of_vhdl_type t in
      let types' = Misc.assoc_add types (id', (t',sz')) in
      expand_array_type types' (id',(t',sz')) 
| _ -> 
    types

and add_companion_array ty' types (id,(ty,sz)) =
  let change_name id = Misc.replace_suffix '_' (string_of_vhdl_array_type ty') id in
  let rec change_type = function
  | Array (t', sz') -> Array (change_type t', sz')
  | t -> ty' in
  Misc.assoc_add types (change_name id, (change_type ty, sz))

and dump_global_const oc (id,c) =
  fprintf oc "  constant %s: %s := %s;\n" id (string_of_type c.gc_typ) (string_of_val c.gc_typ c.gc_val)

and dump_global_fn_sig oc (id,f) =
  let dump_gfun_inst mono (ty, (body, _)) =
    let ty_arg, ty_args, ty_res = Typing.function_types "vhdl backend" id ty in
    fprintf oc "function %s(%s) return %s;\n"
      (if mono then id else id ^ "_" ^ Mangling.string_of_type ty_arg)
      (Misc.string_of_list2 string_of_fn_arg "; " (f.Syntax.gf_def.Syntax.gf_args,ty_args))
      (string_of_return_type ty_res) in
  begin match TypeInsts.to_list f.Syntax.gf_insts with
    [] ->
      dump_gfun_inst true (Types.type_instance f.Syntax.gf_def.Syntax.gf_typ, (f.Syntax.gf_def.Syntax.gf_body, ()))
  | [inst] -> (* only one instance, no need to generate unique names *)
      dump_gfun_inst true inst
  | insts ->  (* several instances *)
      List.iter (dump_gfun_inst false) insts
  end

and string_of_fn_arg id ty = id ^ ":" ^ (string_of_farg_type ty)

and dump_global_array_type_decl oc (name,(ty,sz)) = 
  fprintf oc "  type %s is array (0 to %d) of %s;\n" name (sz-1) (string_of_vhdl_type ty);

and dump_globals_impl oc package_name ir =
  match ir.Interm.ir_globfns with
    [] -> ()
  | fs -> 
      fprintf oc "package body %s is\n" package_name;
      List.iter (dump_global_fn_impl oc) fs;
      fprintf oc "end %s;\n" package_name

and dump_global_fn_impl oc (id,f) =
  let dump_gfun_inst mono (ty, (body, _)) =
    let ty_arg, ty_args, ty_res = Typing.function_types "vhdl backend" id ty in
    let name = if mono then id else id ^ "_" ^ Mangling.string_of_type ty_arg in
    fprintf oc "function %s(%s) return %s is\n"
      name
      (Misc.string_of_list2 string_of_fn_arg "; " (f.Syntax.gf_def.Syntax.gf_args,ty_args))
      (string_of_return_type ty_res);
    fprintf oc "  begin\n";
    fprintf oc "    return %s;\n" (string_of_expr body);
    fprintf oc "  end %s;\n" name in
    begin match TypeInsts.to_list f.Syntax.gf_insts with
      [] ->
        dump_gfun_inst true (Types.type_instance f.Syntax.gf_def.Syntax.gf_typ, (f.Syntax.gf_def.Syntax.gf_body, ()))
    | [inst] -> (* only one instance, no need to generate unique names *)
        dump_gfun_inst true inst
    | insts ->  (* several instances *)
        List.iter (dump_gfun_inst false) insts
    end

(* Dumping user-defined type manipulation fns (one package per declared type) *)

let rec dump_types modname ir profil =
  let fname = modname ^ ".vhd" in
  let fname' = Misc.prefix_dir Genmake.target.Genmake.dir fname in
  let oc = Misc.open_out fname' in
  dump_banner oc;
  List.iter (dump_global_type_intf oc profil ir) profil.variant_types;
  fprintf oc "\n";
  List.iter (dump_global_type_impl oc profil ir) profil.variant_types;
  fprintf oc "\n";
  Logfile.write fname';
  close_out oc

and dump_global_type_intf oc profil ir (name,vd) =
  dump_libraries oc
    ( (if profil.use_floats then cfg.vhdl_core_fp_libs else cfg.vhdl_core_libs)
        @ [cfg.vhdl_num_lib]
        @ (if profil.use_floats then cfg.vhdl_fp_libs else []) );
  fprintf oc "\n";
  fprintf oc "package %s is\n" name;
  List.iter (function c -> fprintf oc "  %s;\n" (variant_inspector_sig vd c)) vd.vd_ctors;
  List.iter (function c -> fprintf oc "  %s;\n" (variant_injector_sig vd c)) vd.vd_ctors;
  List.iter (dump_variant_extractor_intf oc vd) vd.vd_ctors;
  fprintf oc "end %s;\n\n" name

and variant_inspector_sig vd vc = 
   sprintf "function is_%s(t: std_logic_vector) return boolean" (String.uncapitalize vc.vc_name)

and variant_injector_sig vd vc = 
   match vc.vc_arity, vc.vc_arg with
   | 0, _ ->
       (* 0, _ -> sprintf "function mk_%s return std_logic_vector" (String.uncapitalize vc.vc_name) *)
       (* NOTE 2014-08-26, JS
          nullary fns (fns with no argument) does not seem to be correctly supported by the Altera synthetizer.. *)
       sprintf "constant mk_%s: std_logic_vector(%d downto 0) := \"%s\" & to_std_logic_vector(to_unsigned(0,%d),%d)"
         (String.uncapitalize vc.vc_name)
         (vd.vd_repr.vr_size-1)
         (bitstring_of_int vd.vd_repr.vr_tag_size vc.vc_tag)
         vd.vd_repr.vr_data_size
         vd.vd_repr.vr_data_size
   | 1, [t,sz] ->
       sprintf "function mk_%s(d: %s) return std_logic_vector"
         (String.uncapitalize vc.vc_name)
         (string_of_vhdl_type t)
   | n, ts when List.length ts = n ->
       sprintf "function mk_%s(%s) return std_logic_vector"
         (String.uncapitalize vc.vc_name)
         (Misc.string_of_indexed_list (fun i (t,sz) -> "d" ^ (string_of_int i) ^ ": " ^ string_of_vhdl_type t) "; " ts)
   | _, _ ->
       failwith "Vhdl.variant_injector_sig"  (* should not happen *)

and dump_variant_extractor_intf oc vd vc = 
   match vc.vc_arity, vc.vc_arg with
   | 0, _ ->
       ()
   | 1, [t,sz] ->
       fprintf oc "  function get_%s(t: std_logic_vector) return %s;\n"
         (String.uncapitalize vc.vc_name)
         (string_of_vhdl_return_type t)
   | n, ts when List.length ts = n ->
       Misc.list_iter_index
         (fun i (t,sz) ->
           fprintf oc "  function get_%s_field_%d(t: std_logic_vector) return %s;\n"
             (String.uncapitalize vc.vc_name)
             i
             (string_of_vhdl_return_type t))
         ts
   | _, _ -> failwith "Vhdl.dump_variant_extractor_intf"  (* should not happen *)

and dump_global_type_impl oc profil ir (name,vd) =
  dump_libraries oc
    ( (if profil.use_floats then cfg.vhdl_core_fp_libs else cfg.vhdl_core_libs)
        @ [cfg.vhdl_num_lib]
        @ (if profil.use_floats then cfg.vhdl_fp_libs else []) );
  fprintf oc "\n";
  fprintf oc "package body %s is\n\n" name;
   List.iter (dump_variant_inspector_impl oc vd) vd.vd_ctors;
   List.iter (dump_variant_injector_impl oc vd) vd.vd_ctors;
   List.iter (dump_variant_extractor_impl oc vd) (List.filter (function vc -> vc.vc_arity > 0) vd.vd_ctors);
  fprintf oc "end package body %s;\n\n" name

and dump_variant_inspector_impl oc vd vc =
  fprintf oc "  %s is\n" (variant_inspector_sig vd vc);
  fprintf oc "  begin\n";
  if vd.vd_repr.vr_tag_size > 0 then 
    fprintf oc "    return t(%d downto %d) = \"%s\";\n"
      vd.vd_repr.vr_tag.hi
      vd.vd_repr.vr_tag.lo
      (bitstring_of_int (vd.vd_repr.vr_tag_size) vc.vc_tag)
  else
    fprintf oc "    return true;\n";
  fprintf oc "  end;\n\n";

and dump_variant_injector_impl oc vd vc =
   match vc.vc_arity with
   | 0 -> ()
(*      NOTE 2014-08-26, JS. Nullary ctors are implemented as constants and defined in the package interface *)
(*       fprintf oc "    return \"%s\" & to_std_logic_vector(to_unsigned(0,%d),%d);\n" *)
(*         (bitstring_of_int vd.vd_repr.vr_tag_size vc.vc_tag) *)
(*         vd.vd_repr.vr_data_size *)
(*         vd.vd_repr.vr_data_size *)
   | 1 ->
       fprintf oc "  %s is\n" (variant_injector_sig vd vc);
       fprintf oc "  begin\n";
       if vd.vd_repr.vr_tag_size > 0 then
         fprintf oc "    return \"%s\" & to_std_logic_vector(d,%d);\n"
           (bitstring_of_int vd.vd_repr.vr_tag_size vc.vc_tag)
           vd.vd_repr.vr_data_size
       else
         fprintf oc "    return to_std_logic_vector(d,%d);\n"
           vd.vd_repr.vr_data_size;
       fprintf oc "  end;\n\n"
   | n ->
       fprintf oc "  %s is\n" (variant_injector_sig vd vc);
       fprintf oc "  begin\n";
       let tag = 
         if vd.vd_repr.vr_tag_size > 0 then
           "\"" ^ bitstring_of_int vd.vd_repr.vr_tag_size vc.vc_tag ^ "\" & "
         else
           "" in
       let args = Misc.string_of_indexed_list
           (fun i (t,sz) -> "to_std_logic_vector(d" ^ string_of_int i ^ "," ^ string_of_int sz ^ ")")
           " & "
           vc.vc_arg in
       fprintf oc "    return %s %s;\n" tag args;
       fprintf oc "  end;\n\n"

and dump_variant_extractor_impl oc vd vc =
   match vc.vc_arity, vc.vc_arg with
   | 0, _ ->
       ()
   | 1, [t,sz] ->
       fprintf oc "  function get_%s(t: std_logic_vector) return %s is\n"
         (String.uncapitalize vc.vc_name)
         (string_of_vhdl_return_type t);
       fprintf oc "  begin\n";
       fprintf oc "    return from_std_logic_vector(t(%d downto 0),%d);\n"
         vd.vd_repr.vr_data.hi
         vc.vc_size;
       fprintf oc "  end;\n\n"
   | n, ts when List.length ts = n ->
       let _ = Misc.foldl_index
           (fun i offset (t,sz) ->
             fprintf oc "  function get_%s_field_%d(t: std_logic_vector) return %s is\n"
               (String.uncapitalize vc.vc_name)
               i
               (string_of_vhdl_return_type t);
             fprintf oc "    variable r: std_logic_vector(%d downto 0);\n" (sz-1);
               (* Note v2.6.2, JS : we need this to reset the range of the extracted slice to (sz-1,0) *)
             fprintf oc "  begin\n";
             fprintf oc "    r := t(%d downto %d);\n" offset (offset-sz+1);
             fprintf oc "    return from_std_logic_vector(r,%d);\n" sz;
             fprintf oc "  end;\n\n";
             offset-sz)
           vd.vd_repr.vr_data.hi
           ts in
       ()
   | _, _ -> failwith "Vhdl.variant_extractor_sig"  (* should not happen *)

(* Dumping the external converter programs for variant types IO *)

let cannot_write_c_converter fname t =
  eprintf "Error while generating file %s: cannot translate type %s to C\n" fname (string_of_vhdl_type t);
 raise Misc.Error

let rec dump_type_io_converters profil =
  List.iter dump_type_encoder profil.input_variant_types;
  List.iter dump_type_decoder profil.output_variant_types

and dump_type_encoder (id,vd) =
  let fname = "encode_" ^ id ^ ".c" in
  let fname' = Misc.prefix_dir Genmake.target.Genmake.dir fname in
  let oc = Misc.open_out fname' in 
  Misc.dump_banner "//" oc;
  fprintf oc "#include <stdio.h>\n";
  fprintf oc "#include <string.h>\n";
  fprintf oc "#include <stdlib.h>\n";
  fprintf oc "#include \"%s.h\"\n" cfg.vhdl_tyconv_lib;
  fprintf oc "\n";
  List.iter (function vc -> fprintf oc "#define TAG_%s %d\n" vc.vc_name vc.vc_tag) vd.vd_ctors;
  fprintf oc "\n";
  fprintf oc "int main(int argc, char **argv)\n";
  fprintf oc "{\n";
  fprintf oc "  FILE *fpr;\n";
  fprintf oc "  char tok[80];\n";
  fprintf oc "  char bin[] = \"%s\";\n" (String.make vd.vd_repr.vr_size '0');
  fprintf oc "  fpr = get_args(argc, argv);\n";
  fprintf oc "  while ( !feof(fpr) ) {\n";
  fprintf oc "    if ( fscanf(fpr, \"%%s\", tok) != 1 ) break;\n";
  List.iter 
    (function vc ->
      match vc.vc_arity, vc.vc_arg with
      | 0, _ ->
          fprintf oc "    if ( !strcmp(tok, \"%s\") ) {\n" vc.vc_name;
          fprintf oc "      int2bin(TAG_%s, %d, 0, bin); \n" vc.vc_name vd.vd_repr.vr_tag_size;
          fprintf oc "      uint2bin(\"0\", %d, bin+%d); \n"
            vd.vd_repr.vr_data_size
            vd.vd_repr.vr_tag_size;
          fprintf oc "      }\n"
      | 1, [t,sz] ->
          fprintf oc "    if ( !strcmp(tok, \"%s\") ) {\n" vc.vc_name;
          if vd.vd_repr.vr_tag_size > 0 then
            fprintf oc "      int2bin(TAG_%s, %d, 0, bin); \n" vc.vc_name vd.vd_repr.vr_tag_size;
          fprintf oc "      if ( fscanf(fpr, \"%%s\", tok) != 1 ) break;\n";
          fprintf oc "      %s(tok, %d, bin+%d); \n"
            (fst (bin_c_converters fname t))
            vd.vd_repr.vr_data_size
            vd.vd_repr.vr_tag_size;
          fprintf oc "      }\n"
      | n, ts ->
          fprintf oc "    if ( !strcmp(tok, \"%s\") ) {\n" vc.vc_name;
          if vd.vd_repr.vr_tag_size > 0 then
            fprintf oc "      int2bin(TAG_%s, %d, 0, bin); \n" vc.vc_name vd.vd_repr.vr_tag_size;
          let _ = Misc.foldl_index
            (fun i offset (t,sz) ->
              fprintf oc "      if ( fscanf(fpr, \"%%s\", tok) != 1 ) break;\n";
              fprintf oc "      %s(tok, %d, bin+%d); \n"
                (fst (bin_c_converters fname t))
                sz
                offset;
              offset+sz)
            vd.vd_repr.vr_tag_size
            ts in
          fprintf oc "      }\n")
    vd.vd_ctors;
  fprintf oc "  fprintf(stdout,\"%%s\\n\",bin);\n";
  fprintf oc "  }\n";
  fprintf oc "  close_file(fpr); \n";
  fprintf oc "}\n";
  fprintf oc "\n";
  Logfile.write fname';
  close_out oc

and dump_type_decoder (id,vd) =
  let fname = "decode_" ^ id ^ ".c" in
  let fname' = Misc.prefix_dir Genmake.target.Genmake.dir fname in
  let oc = Misc.open_out fname' in
  fprintf oc "#include <stdio.h>\n";
  fprintf oc "#include <string.h>\n";
  fprintf oc "#include <stdlib.h>\n";
  fprintf oc "#include \"%s.h\"\n" cfg.vhdl_tyconv_lib;
  fprintf oc "\n";
  List.iter
    (function vc -> fprintf oc "#define TAG_%s \"%s\"\n" vc.vc_name (bitstring_of_int vd.vd_repr.vr_tag_size vc.vc_tag))
    vd.vd_ctors;
  fprintf oc "\n";
  fprintf oc "int main(int argc, char **argv)\n";
  fprintf oc "{\n";
  fprintf oc "  FILE *fpr;\n";
  fprintf oc "  char tok[80];\n";
  fprintf oc "  char bin[80];\n";
  fprintf oc "  fpr = get_args(argc, argv);\n";
  fprintf oc "  while ( !feof(fpr) ) {\n";
  fprintf oc "    if ( fscanf(fpr, \"%%s\", bin) != 1 ) break;\n";
  List.iter 
    (function vc ->
      match vc.vc_arity, vc.vc_arg with
      | 0, _ ->
          fprintf oc "    if ( !strncmp(bin, TAG_%s, %d) ) {\n" vc.vc_name vd.vd_repr.vr_tag_size;
          fprintf oc "      fprintf(stdout,\"%s \");\n" vc.vc_name;
          fprintf oc "      }\n";
      | 1, [t,sz] ->
          if vd.vd_repr.vr_tag_size > 0 then
            fprintf oc "    if ( !strncmp(bin, TAG_%s, %d) ) {\n" vc.vc_name vd.vd_repr.vr_tag_size;
          fprintf oc "      fprintf(stdout,\"%s \");\n" vc.vc_name;
          fprintf oc "      %s(bin+%d,%d,tok);\n"
            (snd (bin_c_converters fname t))
            vd.vd_repr.vr_tag_size
            vd.vd_repr.vr_data_size;
          fprintf oc "      fprintf(stdout,\"%%s \", tok);\n";
          fprintf oc "      }\n"
      | n, ts ->
          if vd.vd_repr.vr_tag_size > 0 then
            fprintf oc "    if ( !strncmp(bin, TAG_%s, %d) ) {\n" vc.vc_name vd.vd_repr.vr_tag_size;
          fprintf oc "      fprintf(stdout,\"%s \");\n" vc.vc_name;
          let _ = Misc.foldl_index
            (fun i offset (t,sz) ->
              fprintf oc "      %s(bin+%d,%d,tok);\n"
                (snd (bin_c_converters fname t))
                offset
                sz;
              fprintf oc "      fprintf(stdout,\"%%s \", tok);\n";
              offset+sz)
            vd.vd_repr.vr_tag_size
            ts in
          if vd.vd_repr.vr_tag_size > 0 then
            fprintf oc "      }\n")
    vd.vd_ctors;
  fprintf oc "    }\n";
  fprintf oc "  fprintf(stdout,\"\\n\");\n";
  fprintf oc "  fclose(fpr); \n";
  fprintf oc "}\n";
  Logfile.write fname';
  close_out oc

and string_of_ctype fname = function
  | Signed_vec (VS_Const _) -> "int"
  | Unsigned_vec (VS_Const _) -> "unsigned int"
  | Float when cfg.vhdl_float_support -> "float"
  | Boolean -> "int"
  | t -> cannot_write_c_converter fname t

and cformat_for fname = function
  | Signed_vec (VS_Const _) -> "%d"
  | Unsigned_vec (VS_Const _) -> "%u"
  | Float when cfg.vhdl_float_support -> "%f"
  | Boolean -> "%s"
  | t -> cannot_write_c_converter fname t

and bin_c_converters fname = function
  | Signed_vec (VS_Const _) -> "sint2bin", "bin2sint"
  | Unsigned_vec (VS_Const _) -> "uint2bin", "bin2uint"
  | Boolean -> "bool2bin", "bin2bool"
  | Float when cfg.vhdl_float_support -> "float2bin", "bin2float"
  | t -> cannot_write_c_converter fname t


(* Dumping the testbench *)

let rec dump_testbench prefix ir =
  let tb_name = prefix ^ "_tb" in
  let net_name = prefix ^ "_net" in
  let fname = tb_name ^ ".vhd" in
  let fname' = Misc.prefix_dir Genmake.target.Genmake.dir fname in
  let oc = Misc.open_out fname' in
  dump_banner oc;
  let wire_ins, wire_outs, renamed_wires = extract_io_wires ir in
  dump_libraries oc (cfg.vhdl_core_libs @ [cfg.vhdl_num_lib]);
  fprintf oc "\n";
  fprintf oc "entity %s is\n" tb_name;
  fprintf oc "end %s;\n" tb_name;
  fprintf oc "\n";
  fprintf oc "architecture %s of %s is\n" cfg.vhdl_arch_tag tb_name;
  fprintf oc "\n";
  dump_network_intf "component" oc net_name wire_ins wire_outs;
  fprintf oc "\n";
  let wire_name = mk_wire_name_fn renamed_wires in
  List.iter (dump_io_wire ir.Interm.ir_boxes oc wire_name) ir.Interm.ir_wires;
  fprintf oc "signal %s: std_logic;\n" cfg.vhdl_clock;
  fprintf oc "signal %s: std_logic;\n" cfg.vhdl_reset;
  fprintf oc "\n";
  fprintf oc "begin\n";
  List.iter (dump_io_box oc wire_name) ir.Interm.ir_boxes;
  fprintf oc "  N: %s port map(%s,%s,%s);\n"
    net_name
    (string_of_net_ios wire_ins wire_outs)
    cfg.vhdl_clock
    cfg.vhdl_reset;
  fprintf oc "\n";
  dump_reset_process oc;
  fprintf oc "\n";
  dump_clock_process oc;
  fprintf oc "\n";
  fprintf oc "end %s;\n" cfg.vhdl_arch_tag;
  Logfile.write fname';
  close_out oc

and dump_io_wire boxes oc wire_name (wid,(((src,_),(dst,_)),ty)) =
  let w = wire_name wid in
  if is_inp_box boxes src then begin
    fprintf oc "signal %s_f : std_logic;\n" w;
    fprintf oc "signal %s : %s;\n" w (string_of_io_type ty);
    fprintf oc "signal %s_wr : std_logic;\n" w
    end
  else if is_out_box boxes dst then begin
    fprintf oc "signal %s_e : std_logic;\n" w;
    fprintf oc "signal %s : %s;\n" w (string_of_io_type ty);
    fprintf oc "signal %s_rd : std_logic;\n" w
    end
  else 
    ()

and string_of_net_ios wins wouts = 
    Misc.string_of_list (function (w,ty) -> sprintf "%s_f,%s,%s_wr" w w w) "," wins
  ^ "," 
  ^ Misc.string_of_list (function (w,ty) -> sprintf "%s_e,%s,%s_rd" w w w) "," wouts

and dump_reset_process oc = 
  fprintf oc "process                     -- Initial reset\n";
  fprintf oc "begin\n";
  fprintf oc "  %s <= '0';\n" cfg.vhdl_reset;
  fprintf oc "  wait for %d ns;\n" cfg.vhdl_reset_duration_ns;
  fprintf oc "  %s <= '1';\n" cfg.vhdl_reset;
  fprintf oc "  wait;\n";
  fprintf oc "end process;\n"

and dump_clock_process oc =
  fprintf oc "process                     -- Clock\n";
  fprintf oc "begin\n";
  fprintf oc "  %s <= '1';\n" cfg.vhdl_clock;
  fprintf oc "  wait for %d ns;\n" (cfg.vhdl_clock_period_ns/2);
  fprintf oc "  %s <= '0';\n" cfg.vhdl_clock;
  fprintf oc "  wait for %d ns;\n" (cfg.vhdl_clock_period_ns/2);
  fprintf oc "end process;\n"

and dump_io_box oc wire_name (bid,box) =
  let mk_bin_file f = "str_pad(\"" ^ Misc.change_extension "bin" f ^ "\")" in
  match box.ib_tag, box.ib_ins, box.ib_outs with
    RegularB, _, _ -> ()
  | InpB Syntax.StreamIO, _, [_,([wid],ty)] ->
      let w = wire_name wid in
      begin match Filepat.expand box.ib_device with
        [] ->
          () (* should not happen *)
      | [f] -> 
          fprintf oc "  B%d: %s generic map (\"%s\",%d,%d,%s,%d ns) port map(%s_f,%s,%s_wr,%s,%s);\n"
            bid
            cfg.vhdl_tb_stream_in_name
            (Misc.change_extension "bin" f)
            (num_size_of_type ty)
            cfg.vhdl_stream_in_period
            (if cfg.vhdl_stream_in_blanking then "true" else "false")
            cfg.vhdl_stream_in_skew
            w w w
            cfg.vhdl_clock
            cfg.vhdl_reset
      | fs ->
        fprintf oc "  B%d: %s generic map ((%s),%d,%d,%d,%s,%d ns) port map(%s_f,%s,%s_wr,%s,%s);\n"
          bid
          cfg.vhdl_tb_stream_mult_in_name
          (Misc.string_of_list mk_bin_file ", " fs)
          (List.length fs)
          (num_size_of_type ty)
          cfg.vhdl_stream_in_period
          (if cfg.vhdl_stream_in_blanking then "true" else "false")
          cfg.vhdl_stream_in_skew
          w w w
          cfg.vhdl_clock
          cfg.vhdl_reset
      end
  | OutB Syntax.StreamIO, [_,(wid,ty)], _ ->
      let w = wire_name wid in
      begin match Filepat.expand box.ib_device with
        [] ->
          () (* should not happen *)
      | [f] ->
          fprintf oc "  B%d: %s generic map (\"%s\",%d) port map(%s_e,%s,%s_rd,%s,%s);\n"
            bid
            cfg.vhdl_tb_stream_out_name
            (Misc.change_extension "bin" f)
            (num_size_of_type ty)
            w w w
            cfg.vhdl_clock
            cfg.vhdl_reset
      | fs ->
          fprintf oc "  B%d: %s generic map ((%s),%d,%d,%b) port map(%s_e,%s,%s_rd,%s,%s);\n"
            bid
            cfg.vhdl_tb_stream_mult_out_name
            (Misc.string_of_list mk_bin_file ", " fs)
            (List.length fs)
            (num_size_of_type ty)
            !Streams.split_output_frames
            w w w
            cfg.vhdl_clock
            cfg.vhdl_reset
      end
  | InpB Syntax.PortIO, _, [_,([wid],ty)] ->
      let w = wire_name wid in
      fprintf oc "  B%d: %s generic map (\"%s\",%d,%s) port map(%s_f,%s,%s_wr,%s,%s);\n"
        bid
        cfg.vhdl_tb_port_in_name
        (if box.ib_device = "" then "" else Misc.change_extension "bin" box.ib_device)
        (num_size_of_type ty)
        (Misc.string_of_opt (string_of_val ty) box.ib_ival)
        w w w
        cfg.vhdl_clock
        cfg.vhdl_reset
  | OutB Syntax.PortIO, [_,(wid,ty)], _ ->
      let w = wire_name wid in
      fprintf oc "  B%d: %s generic map (\"%s\",%d) port map(%s_e,%s,%s_rd,%s,%s);\n"
        bid
        cfg.vhdl_tb_port_out_name
        (Misc.change_extension "bin" box.ib_device)
        (num_size_of_type ty)
        w w w
        cfg.vhdl_clock
        cfg.vhdl_reset
  | _, _, _ -> Misc.fatal_error "Vhdl.dump_io_box: invalid i/O box" (* should not happen *)
