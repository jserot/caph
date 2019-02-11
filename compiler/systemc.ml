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

(* SystemC backend *)

open Printf
open Types
open Static
open Interm
open Arrays

type sc_config = {
  mutable sc_act_headers: string list;
  mutable sc_top_headers: string list;
  mutable sc_extfn_header: string;
  mutable sc_global_suffix: string;
  mutable sc_splitters_suffix: string;
  mutable sc_trace: bool;
  mutable sc_dump_fifos: bool;
  mutable sc_trace_fifos: bool;
  mutable sc_dump_fifo_stats: bool;
  mutable sc_fifo_stats_file: string;
  mutable sc_mod_suffix: string;
  mutable sc_mod_clock: string;
  mutable sc_clock_period_ns: int;
  mutable sc_fifo_capacity: int;
  mutable sc_stop_time: int;
  mutable sc_stop_idle_time: int;
  mutable sc_io_monitor: bool;
  mutable sc_io_monitor_file: string;
  mutable sc_stream_in_mod_name: string;
  mutable sc_port_in_mod_name: string;
  mutable sc_port_out_mod_name: string;
  mutable sc_stream_in_dc_mod_name: string;
  mutable sc_stream_in_period: int;
  mutable sc_stream_in_hblank: int;
  mutable sc_stream_in_vblank: int;
  mutable sc_stream_out_mod_name: string;
  mutable sc_stream_out_dc_mod_name: string;
  mutable sc_io_monitor_mod_name : string;
  mutable sc_stream_in_out_id: string;
  mutable sc_stream_out_in_id: string;
  mutable sc_use_int: bool;
  mutable sc_use_templates: bool; (* deprecated; kept here for code compatibility in case ... *)
  mutable sc_abbrev_dc_ctors: bool;
  mutable sc_tmp_prefix: string;  (* Prefix for temporary variables in rule actions *)
  mutable sc_type_prefix: string;  (* Prefix for globally defined types *)
  mutable sc_data_field_prefix: string
  }

let cfg = {
  sc_act_headers = [
    "<systemc.h>";
    "<array>";
    "\"fifo.h\"" ];
  sc_top_headers = [
    "<systemc.h>";
    "<array>";
    "<iostream>";
    "\"array_io.h\"";
    "\"fifo.h\"";
    "\"buffer_in.h\"";
    "\"buffer_out.h\"";
    "\"variants.h\"";
    "\"port_in.h\"";
    "\"port_out.h\"";
    "\"stream_in.h\"";
    "\"stream_out.h\"" ;
    "\"stream_dc_in.h\"";
    "\"stream_dc_out.h\"" ];
  sc_global_suffix = "_globals";
  sc_splitters_suffix = "_splitters";
  sc_extfn_header = "\"extfns.h\"";
  sc_trace = false;
  sc_dump_fifos = false;
  sc_trace_fifos = false;
  sc_dump_fifo_stats = false;
  sc_fifo_stats_file = "fifo_stats.dat";
  sc_mod_suffix = "_act";
  sc_mod_clock = "clk";
  sc_clock_period_ns = 10;
  sc_fifo_capacity = 256;
  sc_stop_time = 0;
  sc_stop_idle_time = 0;
  sc_io_monitor = false;
  sc_io_monitor_file = "io_monitor.dat";
  sc_stream_in_mod_name = "stream_in";
  sc_port_in_mod_name = "port_in";
  sc_port_out_mod_name = "port_out";
  sc_stream_in_dc_mod_name = "stream_in_dc";
  sc_io_monitor_mod_name = "io_monitor";
  sc_stream_in_period = 1;
  sc_stream_in_hblank = 0;
  sc_stream_in_vblank = 0;
  sc_stream_out_mod_name = "stream_out";
  sc_stream_out_dc_mod_name = "stream_out_dc";
  sc_stream_in_out_id = "out";
  sc_stream_out_in_id = "inp";
  sc_use_int = false;
  sc_use_templates = false;
  sc_abbrev_dc_ctors = false;
  sc_tmp_prefix = "_";
  sc_type_prefix = "t_";
  sc_data_field_prefix = "d_"
}

let dump_banner oc = Misc.dump_banner "//" oc

let tmp_name v = cfg.sc_tmp_prefix ^ v
let un_tmp_name id = Misc.string_after cfg.sc_tmp_prefix id

let field_name cid = cfg.sc_data_field_prefix ^ String.uncapitalize cid

type profil = {
    has_externs: string option;
    has_globals: string option;   (* Is there some extern / global defns, and if yes name of the file *)
    splitters: splitter_desc list }

and splitter_desc = int (* fanout *)

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

let get_type_size sz =
  match size_repr sz with
    SzConst n -> string_of_int n
  | SzVar v -> SizeVarNames.name_of v
  | _ -> raise Type_size

(* Syntactic constants and values *)

let rec string_of_const = function
  | Const.CInt (v,_,_) -> string_of_int v
  | Const.CBool v -> string_of_bool v
  | Const.CFloat v -> string_of_float v

(* Types *)

let rec string_of_type t  = match real_type t with (* TO REFINE *)
  | Tconstr({tc_name="bool"}, _, _) -> "bool"
  | Tconstr({tc_name="float"}, _, _) -> "float"
  | Tconstr({tc_name="int"}, [], [lo;hi]) -> "int" (* since 2.8.4; no special repr for ranged ints *) 
  | Tconstr({tc_name="int"}, [sg], [sz]) ->
      if cfg.sc_use_int then "int"
          (* Note 2011-11-17, JS
             There seems to a bug in the implementation of the >> operator of the sc_[u]int class
             (end of stream is not handled correctly).
             This option can be a workaround (at the price of simulation accuracy) *)
      else
      begin match real_type sg, size_repr sz with
      | Tconstr({tc_name="_unsigned"},_,_), SzConst s -> "sc_uint<" ^ string_of_int s ^ "> "
      | Tconstr({tc_name="_signed"},_,_), SzConst s -> "sc_int<" ^ string_of_int s ^ "> "
      | Tconstr({tc_name="_unsigned"},_,_), SzVar v ->
          if cfg.sc_use_templates
          then "sc_uint<"  ^ SizeVarNames.name_of v ^ "> "
          else "unsigned int"
      | Tconstr({tc_name="_signed"},_,_), SzVar v ->
          if cfg.sc_use_templates
          then "sc_int<"  ^ SizeVarNames.name_of v ^ "> "
          else "int"
      | _, _ -> "int"
      end
  | Tconstr({tc_name=c}, _, _) when Misc.string_is_prefix "_enum" c -> c  (* This is a hack ! *)
  | Tconstr({tc_name="array"}, [ty], [sz]) -> 
      "std::array" ^ "<" ^ string_of_type ty ^  "," ^ string_of_size sz ^ ">"
  | Tconstr({tc_name=name}, [], []) -> cfg.sc_type_prefix ^ name          (* Globally defined constructed type *)
  | Tconstr({tc_name=name}, ts, ss) ->
      if cfg.sc_use_templates
      then cfg.sc_type_prefix ^ name ^ "<" ^ Misc.string_of_two_lists string_of_type string_of_size "," ts ss ^ ">"
      else cfg.sc_type_prefix ^ Mangling.string_of_name name ts ss []
  | Tvar tv -> TypeVarNames.name_of tv
  | Tproduct ts ->
      "std::tuple<" ^ Misc.string_of_list string_of_type ", " ts ^  " >"
  | ty -> Misc.not_implemented ("Systemc translation of type " ^ Pr_type.string_of_type t)

and string_of_size sz =
  match size_repr sz with
    SzConst n -> string_of_int n
  | SzVar v -> SizeVarNames.name_of v
  | _ -> raise Type_size

let string_of_type_decl = function
    id, Ssval.SVEnumDefn cts -> "typedef enum " ^ "{"  ^ Misc.string_of_list (function c->c) "," cts ^ "} " ^ id 

(* Values *)

let rec string_of_val t v = match v, real_type t with
    Expr.Val_int (i,_), _ -> string_of_int i
  | Expr.Val_bool b, _ -> string_of_bool b
  | Expr.Val_float v, _ -> string_of_float v
  | Expr.Val_con (cid,[]), ty when is_enum_type ty -> cid  (* special case for locally defined enums *)
  | Expr.Val_con (cid,[]), ty -> string_of_type ty ^ "(_tag<" ^ string_of_type ty ^ "::" ^ cid ^ ">())"
  | Expr.Val_con (cid,[v']), ty when Expr.is_simple_expr_val v' ->
      let ty' = Types.no_type in
        (* Note v2.6.2. This is a real hack.
           [ty'] should be retrieved from the typing environment. But we know here that [v'] is a simple value
           and hence that its actual type will be used by the recursive call to [string_of_val]... *)
      string_of_type ty ^ "(_tag<" ^ string_of_type ty ^ "::" ^ cid ^ ">()," ^ string_of_val ty' v' ^ ")"
  | Expr.Val_con (cid,vs), (Tconstr(_,ts,ss) as ty) when List.for_all Expr.is_simple_expr_val vs ->
      let ty' = Types.no_type in
        (* Same note as above.. *)
        string_of_type ty ^ "(_tag<" ^ string_of_type ty ^ "::" ^ cid ^ ">(),"
      ^ "std::make_tuple(" ^ Misc.string_of_list (string_of_val ty') "," vs ^ "))"
  | Expr.Val_array1 (_,vs), t' ->
      let tt, _ = array_dim t' in
      Array1.to_string ~ld:"{{" ~rd:"}}" (string_of_val tt) vs
  | Expr.Val_array2 (_,vs), t' ->
      let tt, _ = array_dim t' in
      Array2.to_string ~ld:"{{" ~rd:"}}" (string_of_val tt) vs
  | Expr.Val_array3 (_,vs), t' ->
      let tt, _ = array_dim t' in
      Array3.to_string ~ld:"{{" ~rd:"}}" (string_of_val tt) vs
  | _ -> 
     failwith ("Systemc.string_of_val: no sensible SystemC representation for value " ^ (Expr.string_of_val v))

(* Expressions *)

let full_fn_name f ann = match ann with
  | None -> f
  | Some (fd,ty) ->
      if cfg.sc_use_templates then
        f ^ "<" ^ string_of_type ty ^ ">"
      else
        if TypeInsts.card fd.Syntax.gf_insts > 1 then 
          Mangling.string_of_name f [ty] [] []
        else
          f

let rec string_of_expr e = string_of_exp e.Syntax.e_desc e.Syntax.e_typ

and string_of_exp e ty =
  match e with
  (* TODO : add parens to reflect nesting and priorities *)
    Syntax.EConst c -> string_of_const c
  | Syntax.EVar v -> v
  | Syntax.ECon (cid,[]) when is_enum_type ty -> cid  (* special case for locally defined enums *)
  | Syntax.ECon(cid,[]) -> string_of_type ty ^ "(_tag<" ^ string_of_type ty ^ "::" ^ cid ^ ">())"
  | Syntax.ECon(cid,[e]) -> string_of_type ty ^ "(_tag<" ^ string_of_type ty ^ "::" ^ cid ^ ">()," ^ string_of_expr e ^ ")"
  | Syntax.ECon(cid,es) -> 
        string_of_type ty ^ "(_tag<" ^ string_of_type ty ^ "::" ^ cid ^ ">(),"
      ^ "std::make_tuple(" ^ Misc.string_of_list string_of_expr' "," es  ^ "))"
  | Syntax.ECond (e1,e2,e3) ->
       "(" ^ string_of_expr e1 ^ ") ? (" ^ string_of_typed_expr ty e2 ^ ") : (" ^ string_of_typed_expr ty e3 ^ ")"
       (* Note 2013-04-30, JS 
          Because of the limitations of the C++ "?" operator, we need an explicit type cast on the two operands.
          See : http://forums.accellera.org/topic/59-bug-on-operator-for-sc-int-class *)
  | Syntax.EApp ({Syntax.e_desc=Syntax.EVar op},_,[e]) when Syntax.is_unop op -> string_of_unop op ^ "(" ^ string_of_expr e ^ ")"
  | Syntax.EApp ({Syntax.e_desc=Syntax.EVar f}, ann, [e]) ->
      full_fn_name f ann ^ "(" ^ string_of_expr e ^ ")"
  | Syntax.EApp ({Syntax.e_desc=Syntax.EVar op},_,[e1;e2]) when Syntax.is_binop op -> 
      string_of_expr' e1 ^ string_of_binop op ^ string_of_expr' e2
  | Syntax.EApp ({Syntax.e_desc=Syntax.EVar f}, ann, es) ->
      full_fn_name f ann ^ "(" ^ Misc.string_of_list string_of_expr "," es ^ ")"
  | Syntax.EArrRead (ar,idxs) -> ar ^ Misc.string_of_list string_of_index_expr "" idxs
  | Syntax.EIgnored -> Error.non_top_ignore_expr "Systemc.string_of_exp"
  | Syntax.ECast (e,t) -> string_of_typed_expr ty e 
(* Note 2012-06-12, JS : ELet exprs are handled at the rule level *)
  | _ -> Misc.not_implemented ("Systemc.string_of_exp: " ^  (Syntax.string_of_exp e))

and string_of_expr' e =
  if is_simple_expr e then string_of_expr e else "(" ^ string_of_expr e ^ ")"

and is_simple_expr e = match e.Syntax.e_desc with
    Syntax.EConst c -> true
  | Syntax.EVar v -> true
  | Syntax.ECon (cid,_) -> true
  | Syntax.EArrRead (ar,idxs) -> true
  | Syntax.ECast (e,t) -> true
  | Syntax.EIgnored -> true
  | _ -> false

and string_of_index_expr e = "[" ^ string_of_expr e ^ "]"

and string_of_typed_expr ty e = "(" ^ string_of_type ty ^ ")(" ^ string_of_expr e ^ ")"

and string_of_guard_expr e = match e.Interm.ge_desc with
    Interm.GEConst c -> string_of_const c
  | Interm.GEVar v -> v
  | Interm.GEApp (op,_,[e]) -> op ^ "(" ^ string_of_guard_expr e ^ ")"
  | Interm.GEApp (">>",_,[e1;e2]) -> string_of_guard_expr' e1 ^ ">>" ^ string_of_guard_expr' e2
  | Interm.GEApp ("<<",_,[e1;e2]) -> string_of_guard_expr' e1 ^ "<<" ^ string_of_guard_expr' e2
  | Interm.GEApp (op,_,[e1;e2]) when Syntax.is_binop op ->
      string_of_guard_expr' e1 ^ string_of_binop op ^ string_of_guard_expr' e2
  | Interm.GEApp (op,_,es) -> op ^ "(" ^ Misc.string_of_list string_of_guard_expr "," es ^ ")"
  | Interm.GEArrRead (ar,idxs) -> ar ^ Misc.string_of_list string_of_guard_index_expr "" idxs
  | Interm.GECast (e,t) -> "(" ^ string_of_type t ^ ")(" ^ string_of_guard_expr e ^ ")"
  | Interm.GEBoundInp (i,Syntax.RPatVar v,_) ->  i ^ "->peek()"
  | Interm.GEBoundInp (i, Syntax.RPatCon(cid,[{Syntax.rp_desc=Syntax.RPatVar _}]), _) ->
      i ^ "->peek().repr.data." ^ field_name cid
  | Interm.GEBoundVar (v,Syntax.RPatVar v',_) -> string_of_local_var v
  | _ -> Misc.not_implemented ("SystemC translation of guard expression " ^ Interm.string_of_guard_expr e)

and string_of_guard_expr' e =
  if is_simple_guard_expr e then string_of_guard_expr e else "(" ^ string_of_guard_expr e ^ ")"

and is_simple_guard_expr e = match e.Interm.ge_desc with
    Interm.GEConst c -> true
  | Interm.GEVar v -> true
  | Interm.GEArrRead (ar,idxs) -> true
  | Interm.GECast (e,t) -> true
  | Interm.GEBoundInp (i,Syntax.RPatVar v,_) -> true
  | Interm.GEBoundInp (i, Syntax.RPatCon(cid,[{Syntax.rp_desc=Syntax.RPatVar _}]), _) -> true
  | _ -> false

and string_of_guard_index_expr e = "[" ^ string_of_guard_expr e ^ "]"

and string_of_local_var = function
  VSimple v -> v
| VArrLoc (v,ks) -> v ^ Misc.string_of_list string_of_array_loc "" ks

and string_of_array_loc k =  "[" ^ string_of_expr k ^ "]"

and string_of_unop = function
    "lnot" -> "~"
  | "not" -> "!"
  | "~-" -> "-"
  | "~-." -> "-"
  | op -> op  

and string_of_binop = function
    "=" -> "=="
  | "land" -> "&"
  | "lor" -> "|"
  | "lxor" -> "^"
  | "+." -> "+"
  | "-." -> "-"
  | "*." -> "*"
  | "/." -> "/"
  | "=." -> "=="
  | "!=." -> "!="
  | ">." -> ">"
  | "<." -> "<"
  | ">=." -> ">="
  | "<=." -> "<="
  | "mod" -> "%"
  | op -> op    (* TO FIX ?  *)

(*** Module implementation (from actor-level desc) ***)

let rec dump_module_impl (profil,tvars,svars) prefix oc name a =
  failwith "Systemc.dump_module_impl: obsolete code" (* should not happen *)

and template_header_string tvars svars = match tvars, svars with
  (* Obsolete *)
  | [],[] -> ""
  | tvs,svs -> "template <" ^ template_params_string true tvs svs ^ ">\n"

and template_params_string with_prefix tvs svs =
  (* Obsolete *)
  Misc.string_of_two_lists (template_class_param with_prefix) (template_int_param with_prefix) ", " tvs svs

and template_class_param with_pfx tv =
  (* Obsolete *)
  let pfx = if with_pfx then "class " else "" in
  pfx ^ TypeVarNames.name_of tv

and template_int_param with_pfx sv =
  (* Obsolete *)
  let pfx = if with_pfx then "int " else "" in
  pfx ^ SizeVarNames.name_of sv

and dump_var_init oc (id,(e,ty,kind,qual)) = match real_type ty, e with
    _, None -> ()
  | Tconstr({tc_name="array"},[ty'], [sz]), _ ->
      fprintf oc "    // Dont worry, %s is initialized in the class constructor...\n" id
  | _, Some e ->
      fprintf oc "    %s = %s;\n" id (string_of_expr e)

and dump_transition oc fvars i ((conds,actions) as t) =
  fprintf oc "        // rule %d: %s\n" i (Interm.string_of_transition t);
  fprintf oc "        %s ( %s ) {\n" (if i=0 then "if" else "else if") (Misc.string_of_list string_of_cond " && " conds);
  (* Note 2015-07-29, JS.
     We now systematically make a copy of (regular) local variables before evaluating a rule RHS to enforce
     a strictly synchronous semantics (all variable names appearing in the RHS of the rule actions refer to 
     the value _before_ the corresponding rule is executed). In the previous versions (<= 2.6.0) this was 
     ensured using a list of "fragile" variables. But the corresponding mechanism could not easily extended 
     to arrays. The current scheme involves a small overhead but is safer (and, more importantly, easier to
     prove correct wrt. the formal semantics and VHDL behavior).
     Update 2016-05-25, JS. 
     Copy also works for arrays as soon as these are implemented with the std::array class. *)
  List.iter (fun (v,v') -> fprintf oc "          %s = %s;\n" v' v) fvars;
  let actions' = List.map (rename_vars_in_action fvars) actions in
  List.iter (dump_action oc) actions';
  fprintf oc "          }\n"

and rename_vars_in_action vars a = match a with
  | Interm.AReadInp (_,_)
  | Interm.ABindInp (_,_,_)
  | Interm.ABindVar (_,_,_) -> a
  | Interm.AWriteOut (o, e, ty) ->
      Interm.AWriteOut(o, Interm.rename_vars_in_expr_desc vars e, ty)
  | Interm.AWriteVar (v, e, ty) ->      (* Bug fix, v2.6.2 *)
      Interm.AWriteVar(Interm.rename_vars_in_local_var vars v, Interm.rename_vars_in_expr_desc vars e, ty)


and string_of_cond c = match c with
    Interm.CInpRdy (i,ty) -> sprintf "(%s->rd_rdy())" i
  | Interm.COutRdy (o,ty) -> sprintf "(%s->wr_rdy())" o
  | Interm.CInpMatch (i, Syntax.RPatConst c, ty) ->  sprintf "(%s->peek()==%s)" i (string_of_const c)
  | Interm.CInpMatch (i, Syntax.RPatCon(cid,[]), ty) -> sprintf "(%s->peek().repr.tag==%s)" i (string_of_type ty ^ "::" ^ cid)
  | Interm.CInpMatch (i, Syntax.RPatCon(cid,[{Syntax.rp_desc=Syntax.RPatConst c}]), ty) ->
      sprintf "(%s->peek().repr.tag==%s) && (%s->peek().repr.data.%s==%s)"
        i (string_of_type ty ^ "::" ^ cid) i (field_name cid) (string_of_const c)
  | Interm.CInpMatch (i, Syntax.RPatCon(cid,[{Syntax.rp_desc=Syntax.RPatVar _}]), ty) ->
      sprintf "(%s->peek().repr.tag==%s)" i (string_of_type ty ^ "::" ^ cid)
  | Interm.CInpMatch (i, Syntax.RPatCon(cid,ps), ty) when List.for_all Syntax.is_simple_rule_pattern ps ->
      begin match Misc.string_of_indexed_list (string_of_pattern_cond i true cid) " && " ps with
        "" -> sprintf "(%s->peek().repr.tag==%s)" i (string_of_type ty ^ "::" ^ cid)
      | s  -> sprintf "(%s->peek().repr.tag==%s) && %s" i (string_of_type ty ^ "::" ^ cid) s
      end
  | Interm.CVarMatch (v, Syntax.RPatConst c, ty) ->  sprintf "(%s==%s)" (string_of_local_var v) (string_of_const c)
  | Interm.CVarMatch (v, Syntax.RPatCon(cid,[]), ty) ->  sprintf "(%s==%s)" (string_of_local_var v) cid 
  | Interm.CVarMatch (VSimple v, Syntax.RPatCon(cid,ps), ty) when List.for_all Syntax.is_simple_rule_pattern ps ->
      (* Note 2015-08-26: patterns like [t[i]:Con(...)] are not allowed (yet) *)
      begin match Misc.string_of_indexed_list (string_of_pattern_cond v false cid) " && " ps with
        "" -> sprintf "(%s.repr.tag==%s)" v (string_of_type ty ^ "::" ^ cid)
      | s  -> sprintf "(%s.repr.tag==%s) && %s" v (string_of_type ty ^ "::" ^ cid) s
      end
  | Interm.CGuardExp e -> sprintf "(%s)" (string_of_guard_expr e)
  | _ -> Misc.not_implemented
        ("Systemc.string_of_cond: cannot translate rule condition [" ^ Interm.string_of_cond c ^ "] to SystemC")

and string_of_pattern_cond src is_inp cid j p = match p.Syntax.rp_desc with
  Syntax.RPatConst c ->
    sprintf "(std::get<%d>(%s%s.repr.data.%s)==%s)"
      j src (if is_inp then "->peek()" else "") (field_name cid) (string_of_const c)
| Syntax.RPatWild -> ""
| Syntax.RPatVar _ -> ""
| Syntax.RPatCon _ -> failwith "SystemC.string_of_pattern_cond" (* should not happen *)

and dump_action oc a = match a with
  | Interm.AReadInp (i,ty) ->
      fprintf oc "          %s->read();\n" i
  | Interm.ABindInp (i,Syntax.RPatVar v,ty) ->
      fprintf oc "          %s = %s->read();\n" v i
  | Interm.ABindInp (i,Syntax.RPatCon(cid,[{Syntax.rp_desc=Syntax.RPatVar v}]),ty) ->
      fprintf oc "          %s = %s->read().repr.data.%s;\n" v i (field_name cid)
  | Interm.ABindInp (i,Syntax.RPatCon(cid,ps),ty) when List.for_all Syntax.is_simple_rule_pattern ps ->
      bind_tuple_pattern oc i true cid ps
  | Interm.ABindVar (v,Syntax.RPatVar v',ty) ->
      fprintf oc "          %s = %s;\n" v' (string_of_local_var v)
  | Interm.ABindVar (v,Syntax.RPatCon(cid,[{Syntax.rp_desc=Syntax.RPatVar v'}]),ty) ->
      fprintf oc "          %s = %s.repr.data;\n" v' (string_of_local_var v) 
  | Interm.ABindVar (VSimple v,Syntax.RPatCon(cid,ps),ty) when List.for_all Syntax.is_simple_rule_pattern ps ->
      bind_tuple_pattern oc v false cid ps
  | Interm.AWriteOut (o, Syntax.ELet(bs, e2), ty) ->
      List.iter (function (v,e1) -> fprintf oc "          %s=%s;\n" v (string_of_expr e1)) bs;
      fprintf oc "          %s->write(%s);\n" o (string_of_expr e2)
  | Interm.AWriteOut (o,exp,ty) ->
      fprintf oc "          %s->write(%s);\n" o (string_of_exp exp ty)
  | Interm.AWriteVar (v, Syntax.ELet(bs, e2), ty) ->
      List.iter (function (v',e1) -> fprintf oc "          %s=%s;\n" v' (string_of_expr e1)) bs;
      fprintf oc "          %s = %s;\n" (string_of_local_var v) (string_of_expr e2)
  | Interm.AWriteVar (v,exp,ty) ->
      fprintf oc "          %s = %s;\n" (string_of_local_var v) (string_of_exp exp ty)
  | _ -> Misc.not_implemented
        ("Systemc.dump_action: cannot translate rule action [" ^ Interm.string_of_action a ^ "] to SystemC")

and bind_tuple_pattern oc src is_inp cid ps =
 let tuple_name =
        "_" ^ src ^ "_" ^ 
        Misc.string_of_list
          (function p -> match p.Syntax.rp_desc with
              Syntax.RPatVar v -> v
            | Syntax.RPatConst _ -> ""
            | Syntax.RPatWild -> ""
            | Syntax.RPatCon (_,_) -> failwith "Systemc.bind_tuple_pattern: illegal pattern binding" (* should not happen *))
          "_"
          ps in
  fprintf oc "          std::tuple<%s> %s = %s%s.repr.data.%s;\n"
        (Misc.string_of_list (function p -> string_of_type p.Syntax.rp_typ) "," ps)
        tuple_name
        src
        (if is_inp then "->read()" else "")
        (field_name cid);
  Misc.list_iter_index 
        (fun i p ->
          match p.Syntax.rp_desc with
            | Syntax.RPatConst _ -> ()
            | Syntax.RPatWild -> ()
            | Syntax.RPatVar v -> fprintf oc "          %s = std::get<%d>(%s);\n" v i tuple_name
            | Syntax.RPatCon (_,_) -> failwith "Systemc.bind_tuple_pattern: illegal pattern binding" (* should not happen *))
        ps

(*** Module implementation (from box-level desc) ***)
(* Note 2013-06-10, JS
   This is largely redundant with the fns above, operating from (potentially polymorphic) actor descriptions. *)

let rec dump_module_impl' profil prefix oc name b =
  let modname = name in 
  fprintf oc "#include \"%s.h\"\n" modname;
  begin match profil.has_externs with Some f -> fprintf oc "#include %s\n" f | None -> () end;
  begin match profil.has_globals with Some f -> fprintf oc "#include \"%s\"\n" f | None -> () end;
  fprintf oc "\n" ;
  fprintf oc "void %s::main(void) {\n" modname;
  List.iter (dump_var_init' oc) b.ib_vars;
  let fragile_vars =
    List.fold_left
      (fun acc (v,(_,ty,kind,_)) ->
        match kind with
        | IV_Regular -> (v, tmp_name v) :: acc
        | _ -> acc)
      []
      b.ib_vars in
  fprintf oc "    while ( 1 ) { \n";
  fprintf oc "      wait(); // %s\n" cfg.sc_mod_clock;
  Misc.list_iter_index (dump_transition oc fragile_vars) b.ib_transitions;
  fprintf oc "    }\n";
  fprintf oc "}\n"

and dump_var_init' oc (id,(v,ty,kind,_)) = match real_type ty, v with
    _, None -> ()
  | Tconstr({tc_name="array"},[ty'], [sz]), _ ->
      fprintf oc "    // %s is initialized in the class constructor...\n" id
  | _, Some v ->
      fprintf oc "    %s = %s;\n" id (string_of_val ty v)

(*** Module interface (from actor-level desc) ***)

let dump_const_decl oc (id,(v,ty,kind,qual)) = match real_type ty, v with
  | Tconstr({tc_name="array"},[_], [_]) as t, Some _ ->
      fprintf oc "  static const %s _%s_init;\n" (string_of_type t) id
  | _, _ -> ()

let actor_var_needs_init = function
    id, (Some _, Types.Tconstr({tc_name="array"},_,_), Interm.IV_Regular, _) ->  true
  | _ -> false

let string_of_actor_var suffix (id,(iv,ty,kind,qual)) = match iv, real_type ty, kind with
| _, _, _ -> sprintf "%s %s%s" (string_of_type ty) id suffix  (* should not happen *)

let rec dump_var_array_init oc suffix (id,(iv,ty,kind,qual)) =
  match id, real_type ty, kind, iv with
  | id, (Types.Tconstr({tc_name="array"},[ty'],[sz]) as t), Interm.IV_Regular, Some _ -> dump_array_init oc suffix id t
  | _ , _, _, _  -> ()

and dump_array_init oc suffix id ty = 
  let size_of sz =
    try get_type_size sz
    with Type_size -> failwith "Systemc.dump_array_init: cannot retrieve array size" in
  match real_type ty with
  | Types.Tconstr({tc_name="array"},[ty'],[sz]) ->
      fprintf oc "    for ( int __i1=0; __i1<%s; __i1++ )\n" (size_of sz);
      begin match real_type ty' with
      | Types.Tconstr({tc_name="array"},[ty''],[sz']) ->
          fprintf oc "      for ( int __i2=0; __i2<%s; __i2++ )\n" (size_of sz');
          begin match real_type ty'' with
          | Types.Tconstr({tc_name="array"},[ty'''],[sz'']) ->
              fprintf oc "        for ( int __i3=0; __i3<%s; __i3++ )\n" (size_of sz'');
              fprintf oc  "         %s[__i1][__i2][__i3] = %s%s[__i1][__i2][__i3];\n" id id suffix
          | _ ->
              fprintf oc  "       %s[__i1][__i2] = %s%s[__i1][__i2];\n" id id suffix
                (* Deeper nesting (nD arrays with n>3 should not happen *)
          end
      | _ ->
          fprintf oc  "     %s[__i1] = %s%s[__i1];\n" id id suffix
      end
  | _ -> () (*should not happen *)

let rec dump_array_var_init bname oc (v,(iv,ty,_,_)) =
  match iv, real_type ty with
  | Some vs, (Tconstr({tc_name="array"},[_],[_]) as t) ->
      fprintf oc "%s %s = %s;\n"
        (string_of_type t)
        (box_var_value_name bname v)
        (string_of_val t vs)
  | _ -> ()

and box_param_value_name bname pname = bname ^ "_" ^ pname ^ "_param"
and box_var_value_name bname vname = bname ^ "_" ^ vname ^ "_var_init"

let dump_var_decl oc (id,(v,ty,kind,qual)) = match real_type ty, kind, qual with
| Tconstr({tc_name="array"}, [ty'],[sz]), Interm.IV_Let, _ ->
    Misc.not_implemented "SystemC translation of let-bound variables with a non-scalar type"
| Tconstr({tc_name="array"}, [ty'],[sz]), Interm.IV_Pattern, Syntax.QVar _ ->
    () (* Note 2011-06-06, JS :
          There's no pattern-binding per se for local variable of array type because the semantics of copying is either
          unclear or too costly *)
| _, Interm.IV_Regular, _ ->
    fprintf oc "    %s %s, %s;\n" (string_of_type ty) id (tmp_name id)
      (* Note 2015-07-29, JS. We now systematically allocate a copy for local variables to implement synchronous behavior *)
| _, _, _ ->
    fprintf oc "    %s %s;\n" (string_of_type ty) id

let rec dump_module_intf (profil,tvars,svars) prefix oc modname a =
  failwith "Systemc.dump_module_intf: obsolete code" (* should not happen *)

let rec dump_module_intf' profil prefix oc modname b =
  TypeVarNames.reset ();
  SizeVarNames.reset ();
  fprintf oc "#ifndef _%s_h\n" modname;
  fprintf oc "#define _%s_h\n" modname;
  fprintf oc "\n";
  List.iter (function h -> fprintf oc "#include %s\n" h) cfg.sc_act_headers;
  begin match profil.has_externs with Some f -> fprintf oc "#include %s\n" f | None -> () end;
  begin match profil.has_globals with Some f -> fprintf oc "#include \"%s\"\n" f | None -> () end;
  fprintf oc "\n";
  fprintf oc "SC_MODULE(%s) {\n" modname;
  fprintf oc "  sc_in<bool> %s;\n" cfg.sc_mod_clock;
  List.iter (function (id,(_,ty)) -> fprintf oc "  sc_port<fifo_in_if<%s > > %s;\n" (string_of_type ty) id) b.ib_ins;
  List.iter (function (id,(_,ty)) -> fprintf oc "  sc_port<fifo_out_if<%s > > %s;\n" (string_of_type ty) id) b.ib_outs;
  fprintf oc "\n";
  List.iter (function td -> fprintf oc "  %s;\n" (string_of_type_decl td)) b.ib_types;
  fprintf oc "\n";
  List.iter (dump_const_decl oc) b.ib_vars;
  fprintf oc "\n";
  fprintf oc "  void main(void);\n";
  fprintf oc "\n";
  fprintf oc "  SC_HAS_PROCESS(%s);\n" modname;
  fprintf oc "\n";
  fprintf oc "  %s(sc_module_name name_" modname;
  List.iter
    (function (id,(v,ty)) -> fprintf oc ", %s" (string_of_actor_param "_" (id,ty)))
    b.ib_params;
  List.iter
    (function v -> fprintf oc ", %s" (string_of_actor_var "_" v))
    (List.filter actor_var_needs_init b.ib_vars);
  fprintf oc ", bool trace_=%b " cfg.sc_trace;
  fprintf oc " ) :\n";
  fprintf oc "    modname(name_), sc_module(name_)";
  List.iter (dump_actor_param_init oc) (List.map (function (id,(v,ty)) -> id,ty) b.ib_params);
  fprintf oc ", trace(trace_) ";
  fprintf oc "\n";
  fprintf oc "  {\n";
  fprintf oc "    SC_THREAD(main);\n";
  fprintf oc "    sensitive << %s.pos();\n" cfg.sc_mod_clock;
  List.iter (dump_array_param_init oc "_") (List.map (function (id,(v,ty)) -> id,ty) b.ib_params);
  List.iter (dump_var_array_init oc "_") b.ib_vars;
  fprintf oc "  }\n";
  fprintf oc "\n";
  fprintf oc "  ~%s() { }\n" modname;
  fprintf oc "\n";
  fprintf oc "  private:\n";
  fprintf oc "    // Service\n";
  fprintf oc "    bool trace;\n";
  fprintf oc "    sc_module_name modname;\n";
  fprintf oc "    // Parameters\n";
  List.iter (function (id,(v,ty)) -> fprintf oc "    %s;\n" (string_of_actor_param "" (id,ty))) b.ib_params;
  fprintf oc "    // Variables\n";
  List.iter (dump_var_decl oc) b.ib_vars;
  fprintf oc "};\n";
  fprintf oc "#endif\n"

and string_of_actor_param suffix (id,ty) = match real_type ty with
  Tarrow(t1,t2) ->
    sprintf "%s (*%s%s)(%s)" (string_of_type t1) id suffix (string_of_type t2)
| _ -> sprintf "%s %s%s" (string_of_type ty) id suffix

and dump_actor_param_init oc (id,ty) = match real_type ty with
  | Tconstr({tc_name="array"},[ty'],[sz]) ->
      ()   (* Array parameters are initialized in the body of the ctor *)
  | _ -> 
      fprintf oc ", %s(%s_)" id id

and dump_array_param_init oc suffix (id,ty) = match id, real_type ty with
  | id, (Types.Tconstr({tc_name="array"},[_],[_]) as t) -> dump_array_init oc suffix id t
  | _ , _  -> ()

(* Module interface (from box-level desc) *)

(* Printing of actor interface and implementation *)

let dump_component f modname fname m =
  let fname' = Misc.prefix_dir Genmake.target.Genmake.dir fname in
  let oc = Misc.open_out fname' in
  dump_banner oc;
  f oc modname m;
  Logfile.write fname';
  close_out oc

let dump_actor profil prefix ir (id,a) =
  match List.assoc_opt "systemc" a.ia_impl with
  | Some [f] ->
    let f1 = f ^ ".h"
    and f2 = f ^ ".cpp" in
    Misc.check_file f1;
    Misc.check_file f2;
    Genmake.add_target ~extra:true f1;
    Genmake.add_target ~extra:true f2
  | _ ->
    if cfg.sc_use_templates then
      (* Obsolete *)
      let modname = id ^ cfg.sc_mod_suffix in
      begin match a.Interm.ia_tvars, a.Interm.ia_svars with
        [], [] -> (* monomorphic actor -> .h + .cpp files *)
          dump_component (dump_module_intf (profil,[],[]) prefix) modname (modname ^ ".h") a;
          dump_component (dump_module_impl (profil,[],[]) prefix) modname (modname ^ ".cpp") a
      | tvars, svars -> (* type and/or size-polymorphic actor -> everything in .h file *)
          dump_component (dump_module_intf (profil,tvars,svars) prefix) modname (modname ^ ".h") a
      end
    else
      let dump_actor_inst mono ((ty,fns), (_, bids)) =
        let b = 
          try List.assoc (List.hd bids) ir.ir_boxes
          with Not_found | Failure "hd" -> Misc.fatal_error "Systemc.dump_actor"  (* should not happen *) in
        let modname =
          if mono then a.ia_name ^ cfg.sc_mod_suffix
          else Mangling.string_of_name a.ia_name [ty] [] fns ^ cfg.sc_mod_suffix in
        dump_component (dump_module_intf' profil prefix) modname (modname ^ ".h") b;
        dump_component (dump_module_impl' profil prefix) modname (modname ^ ".cpp") b in 
      begin match ActInsts.to_list a.ia_insts with
        [] ->
          ()
      | [inst] -> (* only one instance, no need to generate unique names *)
          dump_actor_inst true inst
      | insts ->  (* several instances *)
          List.iter (dump_actor_inst false) insts
      end
          
let rec dump_globals modname ir =
  dump_globals_intf modname ir;
  dump_globals_impl (modname ^ ".h") (modname ^ ".cpp") ir

and dump_globals_intf modname ir =
  let fname = modname ^ ".h" in
  let fname' = Misc.prefix_dir Genmake.target.Genmake.dir fname in
  let oc = Misc.open_out fname' in
  dump_banner oc;
  fprintf oc "#ifndef _%s_h\n" modname;
  fprintf oc "#define _%s_h\n" modname;
  List.iter (function h -> fprintf oc "#include %s\n" h) cfg.sc_top_headers;
  fprintf oc "#ifdef _CPP11\n#include <tuple>\n#endif\n";
  fprintf oc "\n";
  List.iter (dump_global_type_defn oc) ir.Interm.ir_globtys;
  List.iter (dump_global_const_sig oc) ir.Interm.ir_consts;
  List.iter (dump_global_fn_sig oc) ir.Interm.ir_globfns;
  fprintf oc "#endif\n";
  Logfile.write fname';
  close_out oc

and dump_global_type_defn oc (id,t) =
  match cfg.sc_use_templates, t.Typing.tc_defn.ty_arity, t.Typing.tc_defn.ty_desc with
    true, _, Variant_type (_,_,_) ->
      dump_variant_type_defn oc ([],[],t.Typing.tc_defn)
  | false, (0,0), Variant_type (_,_,_) ->   (* Monomorphic variant *)
      dump_variant_type_defn oc ([],[],t.Typing.tc_defn)
  | false, _, Variant_type (_,_,_) ->       (* Polymorphic variant *)
      List.iter
        (function ((tyargs,szargs),(td,_)) -> dump_variant_type_defn oc (tyargs,szargs,td); fprintf oc "\n")
        (Typing.TyconInsts.to_list (t.Typing.tc_insts))
  | _, _, Abstract_type -> Misc.not_implemented "Systemc.dump_global_type_defn_intf: abstract type"  (* should not happen *)
  | _, _, Abbrev_type _ -> Misc.not_implemented "Systemc.dump_global_type_defn_intf: abbrev type" (* should not happen *)

and dump_variant_type_defn oc (tyargs,szargs,td) = match td.ty_desc with
  Variant_type (_,_,cds) -> 
      let base_ty_name = cfg.sc_type_prefix ^ td.ty_constr.tc_name in
      let templ, short_ty_name, full_ty_name =
        if td.ty_arity = (0,0) then
          "",
          base_ty_name,
          base_ty_name
        else
          if cfg.sc_use_templates then 
            let ty_params, sz_params =
              begin try (List.hd cds).cs_params
              with Failure "hd" -> Misc.fatal_error "Systemc.dump_global_type_defn" (* should not happen *) end in
            TypeVarNames.reset ();
            SizeVarNames.reset ();
            template_header_string ty_params sz_params,
            base_ty_name,
            base_ty_name ^ "<" ^ template_params_string false ty_params sz_params ^ " >"
          else
            "",
            Mangling.string_of_name base_ty_name tyargs szargs [],
            Mangling.string_of_name base_ty_name tyargs szargs [] in
      let copy_tuple oc hdr dst arity src = 
        for i=0 to arity-1 do
          Printf.fprintf oc "%sstd::get<%d>(%s) = std::get<%d>(%s);\n" hdr i dst i src
        done in
      if templ <> "" then fprintf oc "%s" templ;
      fprintf oc "class %s {\n" short_ty_name;
      fprintf oc "public:\n";
      fprintf oc "  enum t_tag { %s };\n" (Misc.string_of_list (function c -> c.cs_name) ", " cds);
      fprintf oc "  struct t_data { %s };\n"
        (Misc.string_of_list
           (function c ->
             match c.cs_arity with
               0 -> ""
             | n -> (string_of_type c.cs_arg) ^ " " ^ (field_name c.cs_name) ^ "; ")
           ""
           cds);
      fprintf oc "  struct { t_tag tag; t_data data; } repr;\n";
      fprintf oc "  ~%s() { };\n" short_ty_name;        (* Destructor *)
      fprintf oc "  %s(void) { };\n" short_ty_name;     (* Default ctor *)
      List.iter (function c ->                          (* Ctors *)
        match c.cs_arity with
          0 ->
            fprintf oc "  %s(_tag<%s>) { repr.tag = %s; };\n" short_ty_name c.cs_name c.cs_name
        | 1 ->
            fprintf oc "  %s(_tag<%s>, %s v) { repr.tag = %s; repr.data.%s = v; };\n"
              short_ty_name c.cs_name (string_of_type c.cs_arg) c.cs_name (field_name c.cs_name)
        | n -> 
            fprintf oc "  %s(_tag<%s>, %s v) {\n" short_ty_name c.cs_name (string_of_type c.cs_arg);
            fprintf oc "    repr.tag = %s;\n" c.cs_name;
            copy_tuple oc "    " ("repr.data." ^ field_name c.cs_name) c.cs_arity "v";
            fprintf oc "  }\n")
        cds;
      fprintf oc "  inline %s& operator = (const %s& v) {\n" short_ty_name short_ty_name; (* operator= *)
      fprintf oc "    repr.tag = v.repr.tag;\n";
      fprintf oc "    switch ( v.repr.tag ) {\n";
      List.iter
        (function c -> 
          fprintf oc "      case %s:\n" c.cs_name;
          match c.cs_arity with
            0 -> fprintf oc "      break;\n";
          | 1 -> fprintf oc "      repr.data.%s = v.repr.data.%s;\n      break;\n" (field_name c.cs_name)  (field_name c.cs_name);
          | n ->
              let f = "repr.data." ^ field_name c.cs_name in
              copy_tuple oc "        " f c.cs_arity ("v." ^ f);
              fprintf oc "        break;\n")
        cds;
      fprintf oc "       }\n";
      fprintf oc "    return *this;\n";
      fprintf oc "    }\n";
      fprintf oc "  inline friend ::std::ostream& operator << ( ::std::ostream& os, const %s& v) {\n" full_ty_name;
      fprintf oc "    switch ( v.repr.tag ) {\n";
      List.iter (function c ->
        if td.ty_constr.tc_name = "dc" && cfg.sc_abbrev_dc_ctors then 
          begin match c.cs_name with
            "SoS" -> fprintf oc "      case SoS: os << \"< \"; return os;\n"
          | "EoS" -> fprintf oc "      case EoS: os << \"> \"; return os;\n"
          | "Data" -> fprintf oc "      case Data: os << v.repr.data.%sdata; return os;\n" cfg.sc_data_field_prefix
          |  _ -> Misc.fatal_error "Systemc.dump_variant_type_defn" (* should not happen *) end
        else
          begin match c.cs_arity with 
            0 ->
              fprintf oc "      case %s: os << \"%s \"; return os;\n" c.cs_name c.cs_name
          | 1 -> 
              fprintf oc "      case %s: os << \"%s \" <<  v.repr.data.%s; return os;\n" c.cs_name c.cs_name (field_name c.cs_name)
          | n ->
              fprintf oc "      case %s: os << \"%s \" "  c.cs_name c.cs_name;
              for i=0 to c.cs_arity-1 do
                fprintf oc "<< std::get<%d>(v.repr.data.%s) << \" \"" i (field_name c.cs_name)
              done;
              fprintf oc ";\n   return os;\n"
           end)
        cds;
      fprintf oc "      }\n";
      fprintf oc "      return os;";
      fprintf oc "    }\n";
      fprintf oc "  inline friend ::std::istream& operator >> ( ::std::istream& is, %s& v) {\n" full_ty_name;
      fprintf oc "    char tmp[64];\n";
      fprintf oc "    is >> tmp;\n";
      fprintf oc "    if ( is.good() ) {\n";
      List.iter (function c ->
        if td.ty_constr.tc_name = "dc" && cfg.sc_abbrev_dc_ctors then 
          begin match c.cs_name with
            "SoS" -> fprintf oc "      if ( !strcmp(tmp,\"<\") ) { v.repr.tag=SoS; return is;}\n"
          | "EoS" -> fprintf oc "      else if ( !strcmp(tmp,\">\") ) { v.repr.tag=EoS; return is;}\n"
          | "Data" ->
              fprintf oc "     else { std::istringstream ss(tmp); v.repr.tag=Data; ss >> v.repr.data.%sdata; return is;}\n"
                cfg.sc_data_field_prefix
          |  _ -> Misc.fatal_error "Systemc.dump_variant_type_defn" (* should not happen *) end
        else
          begin match c.cs_arity with 
            0 ->
              fprintf oc "      if ( !strcmp(tmp,\"%s\") ) { v.repr.tag=%s; return is;}\n" c.cs_name c.cs_name
          | 1 ->
              fprintf oc "      if ( !strcmp(tmp,\"%s\") ) { v.repr.tag=%s; is >> v.repr.data.%s; return is;}\n"
                c.cs_name c.cs_name (field_name c.cs_name)
          | n ->
              fprintf oc "      if ( !strcmp(tmp,\"%s\") ) { v.repr.tag=%s; is " c.cs_name c.cs_name;
              for i=0 to c.cs_arity-1 do
                fprintf oc " >> std::get<%d>(v.repr.data.%s) " i (field_name c.cs_name)
              done;
              fprintf oc "; return is; }\n"
           end)
        cds;
      fprintf oc "      }\n";
      fprintf oc "      return is;";
      fprintf oc "    }\n";
      fprintf oc "};\n\n";
      if templ <> "" then fprintf oc "%s" templ;
      fprintf oc "inline void sc_trace(sc_trace_file *tf, const %s& v, const std::string& n) { sc_trace(tf, \"<%s>\", n);}\n"
        full_ty_name short_ty_name;
| _ -> ()

and dump_global_const_sig oc (id,c) = match real_type c.gc_typ with
  | ty -> fprintf oc "extern %s %s;\n" (string_of_type ty) id

and dump_global_fn_sig oc (id,f) =
  if cfg.sc_use_templates then
    let tyvars = f.Syntax.gf_def.Syntax.gf_typ.ts_tparams in
    let szvars = f.Syntax.gf_def.Syntax.gf_typ.ts_sparams in
    let ty_arg, ty_args, ty_res = Typing.function_types "systemc backend" id f.Syntax.gf_def.Syntax.gf_typ.ts_body in
    TypeVarNames.reset ();
    SizeVarNames.reset ();
    fprintf oc "%s%s %s(%s)"
      (template_header_string tyvars szvars)
      (string_of_type ty_res)
      id
      (Misc.string_of_list2 string_of_fn_arg "," (f.Syntax.gf_def.Syntax.gf_args,ty_args));
    match tyvars with
      [] -> fprintf oc ";\n"
    | _  -> fprintf oc " {\n  return %s;\n};\n" (string_of_expr f.Syntax.gf_def.Syntax.gf_body)
  else
    let dump_gfun_inst mono (ty, (body, _)) =
      let ty_arg, ty_args, ty_res = Typing.function_types "systemc backend" id ty in
      fprintf oc "%s %s(%s);\n"
        (string_of_type ty_res)
        (if mono then id else id ^ "_" ^ Mangling.string_of_type ty_arg)
        (Misc.string_of_list2 string_of_fn_arg ", " (f.Syntax.gf_def.Syntax.gf_args,ty_args)) in
    begin match TypeInsts.to_list f.Syntax.gf_insts with
      [] ->
        dump_gfun_inst true (Types.type_instance f.Syntax.gf_def.Syntax.gf_typ, (f.Syntax.gf_def.Syntax.gf_body, ()))
    | [inst] -> (* only one instance, no need to generate unique names *)
        dump_gfun_inst true inst
    | insts ->  (* several instances *)
        List.iter (dump_gfun_inst false) insts
    end

and string_of_fn_arg id ty = (string_of_type ty) ^ " " ^ id

and dump_globals_impl hname fname ir =
  let fname' = Misc.prefix_dir Genmake.target.Genmake.dir fname in
  let oc = Misc.open_out fname' in
  dump_banner oc;
  fprintf oc "#include \"%s\"\n" hname;
  fprintf oc "\n";
  List.iter (dump_global_const_impl oc) ir.Interm.ir_consts;
  List.iter (dump_global_fn_impl oc) ir.Interm.ir_globfns;
  Logfile.write fname';
  close_out oc

and dump_global_const_impl oc (id,c) = match real_type c.gc_typ, c.gc_val with
  | ty, v -> fprintf oc "%s %s = %s;\n" (string_of_type ty) id (string_of_val ty v)

and dump_global_fn_impl oc (id,f) =
  if cfg.sc_use_templates then
    ()  (* The template implementation has been written in the interface file *)
  else
    let dump_gfun_inst mono (ty, (body, _)) =
      let ty_arg, ty_args, ty_res = Typing.function_types "systemc backend" id ty in
      fprintf oc "%s %s(%s) {\n"
        (string_of_type ty_res)
        (if mono then id else id ^ "_" ^ Mangling.string_of_type ty_arg)
        (Misc.string_of_list2 string_of_fn_arg "," (f.Syntax.gf_def.Syntax.gf_args,ty_args));
      fprintf oc "  return (%s);\n" (string_of_expr body);
      fprintf oc "};\n" in
    begin match TypeInsts.to_list f.Syntax.gf_insts with
      [] ->
        dump_gfun_inst true (Types.type_instance f.Syntax.gf_def.Syntax.gf_typ, (f.Syntax.gf_def.Syntax.gf_body, ()))
    | [inst] -> (* only one instance, no need to generate unique names *)
        dump_gfun_inst true inst
    | insts ->  (* several instances *)
        List.iter (dump_gfun_inst false) insts
    end

(* Special handling of arrays used as parameters or to initialize local variable for actors *)

let rec dump_array_constants oc (i,b) =
  let bname = b.Interm.ib_name ^ "_" ^ (string_of_int i) in
  List.iter (dump_array_param bname oc) b.Interm.ib_params;
  List.iter (dump_array_var_init bname oc) b.Interm.ib_vars

and dump_array_param bname oc (p,(v,ty)) =
  match real_type ty with
  | Tconstr({tc_name="array"},[ty'],[sz']) as t ->
      fprintf oc "%s %s = %s;\n"
        (string_of_type t)
        (box_param_value_name bname p)
        (string_of_val t v)
  | _ -> ()

(* Printing splitters interface and implementation *)

let dump_split_actor oc fanout =
  fprintf oc "template <class T>\n";
  fprintf oc "SC_MODULE(split%d) { ;\n" fanout;
  fprintf oc "  sc_in<bool> clk;\n";
  fprintf oc "  sc_port<fifo_in_if<T> > i;\n";
  for i=1 to fanout do
    fprintf oc "  sc_port<fifo_out_if<T> > o_%d;\n" i
  done;
  fprintf oc "\n";
  fprintf oc "  void main(void);\n\n";
  fprintf oc "  SC_HAS_PROCESS(split%d);\n\n" fanout;
  fprintf oc "  split%d(sc_module_name name_, bool trace_=false  ) :\n" fanout;
  fprintf oc "   modname(name_), sc_module(name_), trace(trace_)\n";
  fprintf oc "  {\n";
  fprintf oc "    SC_THREAD(main);\n";
  fprintf oc "    sensitive << clk.pos();\n";
  fprintf oc "  }\n\n";
  fprintf oc "  ~split%d() { };\n\n" fanout;
  fprintf oc "  private:\n";
  fprintf oc "    char *fname;\n";
  fprintf oc "    bool trace;\n";
  fprintf oc "    sc_module_name modname;\n";
  fprintf oc "};\n\n";
  fprintf oc "template <class T>\n";
  fprintf oc "void split%d<T>::main(void) {\n" fanout;
  fprintf oc "    T d;\n";
  fprintf oc "    while(1) {\n";
  fprintf oc "      wait(); // clk\n";
  fprintf oc "      if ( i->rd_rdy() && %s ) {\n"
    (Misc.string_of_list
       (function i -> "o_" ^ i ^ "->wr_rdy()") " && " (Misc.list_make 1 fanout string_of_int));
  fprintf oc "          d = i->read();\n";
  fprintf oc "          if ( trace ) cout << modname << \" read \" << d << \" at \" << sc_time_stamp() << endl;\n";
  for i=1 to fanout do
    fprintf oc "          o_%d->write(d);\n" i
  done;
  fprintf oc "          if ( trace ) cout << modname << \" wrote \" << d << \" at \" << sc_time_stamp() << endl;\n";
  fprintf oc "          };\n";
  fprintf oc "      };\n";
  fprintf oc "};\n\n"

let dump_split_actors fname fanouts =
  let fname' = Misc.prefix_dir Genmake.target.Genmake.dir fname in
  let oc = Misc.open_out fname' in
  Misc.dump_banner "//" oc;
  fprintf oc "#ifndef %s_h\n" cfg.sc_splitters_suffix;
  fprintf oc "#define %s_h\n\n" cfg.sc_splitters_suffix;
  fprintf oc "#include \"fifo.h\"\n";
  fprintf oc "#include <systemc.h>\n\n";
  List.iter (dump_split_actor oc) fanouts;
  fprintf oc "#endif\n";
  close_out oc;
  Logfile.write fname'

(* Printing of IO monitor module (when option [sc_stop_when_idle] or [sc_io_monitor] is on *)

type io_wire = {
    iow_name: string;
    iow_box: string;
    iow_kind: io_mon_kind;
    iow_sig: io_mon_sig;
    }
and io_mon_kind = IOW_Inp | IOW_Outp
and io_mon_sig = IOW_Started | IOW_Stopped
  
let string_of_iow_kind = function IOW_Inp -> "input" | IOW_Outp -> "output"
let string_of_iow_sig = function IOW_Started -> "started" | IOW_Stopped -> "stopped"

let dump_io_monitor_module oc io_mon_wires =
  fprintf oc "SC_MODULE(%s) {\n" cfg.sc_io_monitor_mod_name;
  List.iter (fun w -> fprintf oc "  sc_in<bool> %s;\n" w.iow_name) io_mon_wires;
  fprintf oc "  void react(void) {\n";
  if cfg.sc_io_monitor then
    List.iter
      (fun w ->
        fprintf oc "    if ( ! _%s && %s ) { ofile << \"%s %s at t=\" << sc_time_stamp()%s << \"\\n\"; _%s=true; }\n"
                w.iow_name
                w.iow_name
                w.iow_box
                (string_of_iow_sig w.iow_sig)
                (match w.iow_sig with IOW_Stopped -> "-offset" | _ -> "")
                w.iow_name)
      io_mon_wires;
  if cfg.sc_stop_idle_time > 0 then begin
      let stop_wires = List.filter (function {iow_kind=IOW_Outp;iow_sig=IOW_Stopped} -> true | _ -> false) io_mon_wires in
      fprintf oc "    if ( %s ) {\n" (Misc.string_of_list (function w -> "_" ^ w.iow_name) " && " stop_wires);
      fprintf oc "      cout << \"** Simulation stopped at t=\" << sc_time_stamp() << \" after %d ns of inactivity on stream output(s)\";\n"
              cfg.sc_stop_idle_time;
      fprintf oc "      sc_stop();\n";
      fprintf oc "      }\n"
      end;
  fprintf oc "    }\n";
  fprintf oc "  SC_HAS_PROCESS(%s);\n" cfg.sc_io_monitor_mod_name;
  fprintf oc "  %s(sc_module_name name_) : modname(name_), ofile(\"%s\") {\n"
          cfg.sc_io_monitor_mod_name
          cfg.sc_io_monitor_file;
  fprintf oc "    SC_METHOD(react);\n";
  fprintf oc "    offset=sc_time(%d,SC_NS);\n" cfg.sc_stop_idle_time;
  List.iter (fun w -> fprintf oc "    _%s = false;\n" w.iow_name) io_mon_wires;
  fprintf oc "    sensitive << %s;\n"
    (Misc.string_of_list (function w -> w.iow_name) " << " io_mon_wires);
  fprintf oc "  };\n";
  fprintf oc "  ~%s() {\n" cfg.sc_io_monitor_mod_name;
  fprintf oc "    ofile.close();\n";
  fprintf oc "    cout << \"Wrote file %s\" << endl;\n" cfg.sc_io_monitor_file;
  fprintf oc "  }\n";
  fprintf oc "  private:\n";
  fprintf oc "    const char *modname;\n";
  List.iter (fun w -> fprintf oc "    bool _%s;\n" w.iow_name) io_mon_wires;
  fprintf oc "    sc_time offset;\n";
  fprintf oc "    ofstream ofile;\n";
  fprintf oc "};\n\n"
  
(* Printing of top level .cpp file *)

let rec dump_top_module prefix fname ir =
  let has_externs = ir.Interm.ir_extfns <> [] in
  let has_globals = ir.Interm.ir_consts <> [] || ir.Interm.ir_globfns <> [] || ir.Interm.ir_globtys <> [] in
  let fname' = Misc.prefix_dir Genmake.target.Genmake.dir fname in
  let oc = Misc.open_out fname' in
  let headers = 
    if cfg.sc_use_templates
    then Misc.flatmap header_name ir.Interm.ir_actors
    else Misc.flatmap header_name ir.Interm.ir_actors in
  let splitters = List.fold_left Interm.extract_split_boxes [] ir.Interm.ir_boxes in
  dump_banner oc;
  List.iter (function h -> fprintf oc "#include %s\n" h) (cfg.sc_top_headers @ headers);
  fprintf oc "#ifdef _CPP11\n#include <tuple>\n#endif\n";
  if has_externs then fprintf oc "#include %s\n" cfg.sc_extfn_header;
  if has_globals then fprintf oc "#include \"%s\"\n" (prefix ^ cfg.sc_global_suffix ^ ".h");
  if splitters <> [] then fprintf oc "#include \"%s\"\n"  (prefix ^ cfg.sc_splitters_suffix ^ ".h");
  fprintf oc "\n";
  List.iter (dump_array_constants oc) ir.Interm.ir_boxes;
  fprintf oc "\n";
  let io_mon_wires =
    List.fold_left
    (fun ws (bid,b) ->
      let bname = b.ib_name ^ "_" ^ string_of_int bid in
      match b.ib_tag with
      |  OutB Syntax.StreamIO ->
             { iow_name = bname ^ "_started"; iow_box=bname; iow_kind=IOW_Outp; iow_sig=IOW_Started }
          :: { iow_name = bname ^ "_stopped"; iow_box=bname; iow_kind=IOW_Outp; iow_sig=IOW_Stopped }
          :: ws 
      |  InpB Syntax.StreamIO ->
             { iow_name = bname ^ "_started"; iow_box=bname; iow_kind=IOW_Inp; iow_sig=IOW_Started }
          :: ws 
         | _ -> ws)
      []
      ir.Interm.ir_boxes in
  if cfg.sc_io_monitor or cfg.sc_stop_idle_time > 0 then
    dump_io_monitor_module oc io_mon_wires;
  fprintf oc "int sc_main(int argc, char* argv[]) {\n";
  List.iter (dump_wire oc ir.Interm.ir_boxes) ir.Interm.ir_wires;
  fprintf oc "\n";
  fprintf oc "  sc_clock %s(\"%s\", %d, SC_NS, 0.5);\n" cfg.sc_mod_clock cfg.sc_mod_clock cfg.sc_clock_period_ns;
List.iter (function w -> fprintf oc "  sc_signal<bool> _%s;\n" w.iow_name) io_mon_wires;
  fprintf oc "\n";
  if cfg.sc_trace_fifos then begin
    fprintf oc "  sc_trace_file *fifo_trace_file;\n";
    fprintf oc "  fifo_trace_file = sc_create_vcd_trace_file (\"%s_fifos\");\n" prefix; 
    fprintf oc "  sc_trace(fifo_trace_file, %s, \"%s\");\n" cfg.sc_mod_clock cfg.sc_mod_clock;
    List.iter
      (function (i,_) -> fprintf oc "  w%d.trace(fifo_trace_file);\n" i)
      (List.filter (is_fifo_wire ir.Interm.ir_boxes) ir.Interm.ir_wires);
    end;
  List.iter (dump_box ir io_mon_wires oc) ir.Interm.ir_boxes;
  if cfg.sc_io_monitor or cfg.sc_stop_idle_time > 0 then begin
    fprintf oc "  %s _io_monitor(\"io_monitor\");\n" cfg.sc_io_monitor_mod_name;
    List.iter (fun w -> fprintf oc "  _io_monitor.%s(_%s);\n" w.iow_name w.iow_name) io_mon_wires;
    end;
  fprintf oc "\n";
  if cfg.sc_stop_time > 0 then begin
    fprintf oc "  sc_start(%d, SC_NS);\n" cfg.sc_stop_time;
    fprintf oc "  cout << \"Simulation stopped at t=\" << sc_time_stamp() << \"\\n\";\n";
    end
  else
    fprintf oc "  sc_start();\n";
  if cfg.sc_dump_fifo_stats then begin
    fprintf oc "  ofstream fifo_stat_file (\"%s\");\n" cfg.sc_fifo_stats_file;
    List.iter
      (function (i,_) -> fprintf oc "  w%d.dump_stat(fifo_stat_file,%d);\n" i 0)
      (List.filter (is_fifo_wire ir.Interm.ir_boxes) ir.Interm.ir_wires);
    fprintf oc "  fifo_stat_file.close();\n";
    fprintf oc "  cout << \"Wrote file %s\" << endl;\n" cfg.sc_fifo_stats_file
    end;
  if cfg.sc_trace_fifos then begin
    fprintf oc "  sc_close_vcd_trace_file (fifo_trace_file);\n";
    fprintf oc "  cout << \"Wrote file %s_fifos.vcd\" << endl;\n" prefix
    end;
  fprintf oc "  return EXIT_SUCCESS;\n";
  fprintf oc "}\n" ;
  Logfile.write fname';
  close_out oc;
  { has_externs = if has_externs then Some cfg.sc_extfn_header else None;
    has_globals = if has_globals then Some (prefix ^ cfg.sc_global_suffix ^ ".h") else None;
    splitters = splitters }

and header_name (id,a) =
  match List.assoc_opt "systemc" a.ia_impl with
  | Some [f] -> [ "\"" ^ f ^ ".h\"" ]
  | _ ->
    if cfg.sc_use_templates then 
      [ "\"" ^ id ^ cfg.sc_mod_suffix ^ ".h\"" ]
    else
      begin match ActInsts.to_list a.ia_insts with
        [_] -> 
           ["\"" ^ a.ia_name ^ cfg.sc_mod_suffix ^ ".h\""]
      | insts -> 
          List.map
            (function ((ty,fns),_) -> "\"" ^ Mangling.string_of_name a.ia_name [ty] [] fns ^ cfg.sc_mod_suffix ^ ".h\"")
            insts
      end

and dump_wire oc boxes (wid,(((src,_),(dst,_)),ty)) =
  if Interm.is_input_port boxes src then
      fprintf oc "  buffer_in<%s > w%d(\"w%d\");\n" (string_of_type ty) wid wid
  else if Interm.is_output_port boxes dst then
      fprintf oc "  buffer_out<%s > w%d(\"w%d\");\n" (string_of_type ty) wid wid
  else
      fprintf oc "  fifo<%s > w%d(\"w%d\", %d, %b, %b);\n"
        (string_of_type ty) wid wid cfg.sc_fifo_capacity cfg.sc_dump_fifos cfg.sc_dump_fifo_stats; 

and dump_box ir stop_wires oc (i,b) =
  let bname = b.ib_name ^ "_" ^ (string_of_int i) in
  match b.ib_tag with
    RegularB when b.ib_name = "_split" ->
      let ty = begin match b.ib_ins with 
      | (id,(wid,ty))::_ -> ty
      | _ -> failwith "Systemc.dump_box: split box" (* should not happen *) end in
      fprintf oc "  %s<%s > %s(\"%s\", %s %b);\n"
        ("split" ^ string_of_int (List.length b.ib_outs)) (string_of_type ty) bname bname
        (string_of_param_values ir bname b.ib_params) cfg.sc_trace;
      fprintf oc "  %s.%s(%s);\n" bname cfg.sc_mod_clock cfg.sc_mod_clock;
      List.iter (dump_box_input oc bname) b.ib_ins;
      List.iter (dump_box_output oc bname) b.ib_outs
  | RegularB ->
      let modname = 
        if b.ib_ninsts < 2 then 
          b.ib_name ^ cfg.sc_mod_suffix
        else
          if cfg.sc_use_templates then 
              b.ib_name
            ^ cfg.sc_mod_suffix
            ^ (match b.ib_tvbs, b.ib_svbs with
                  [], [] -> ""
                | tvbs, svbs -> 
                    "<"
                    ^ Misc.string_of_two_lists string_of_type string_of_size "," (List.map snd tvbs) (List.map snd svbs)
                    ^ ">")
          else
            Mangling.string_of_name b.ib_name [b.ib_tysig] [] (List.map snd b.ib_fpbs) ^ cfg.sc_mod_suffix in
      fprintf oc "  %s %s(\"%s\", %s %s %b);\n"
        modname
        bname
        bname
        (string_of_param_values ir bname b.ib_params)
        (string_of_var_init_values bname b.ib_vars)
        cfg.sc_trace;
      fprintf oc "  %s.%s(%s);\n" bname cfg.sc_mod_clock cfg.sc_mod_clock;
      List.iter (dump_box_input oc bname) b.ib_ins;
      List.iter (dump_box_output oc bname) b.ib_outs
  | InpB Syntax.StreamIO ->
      let wid, ty = begin match b.ib_outs with
        [id,([wid],ty)] -> wid, ty 
      | _ -> Misc.not_implemented "Systemc.dump_box: multi-output stream_in box" end in
      let ifs = Filepat.expand b.ib_device in
      fprintf oc "  char *%s_ifiles[] = { %s, 0 };\n"
        bname
        (Misc.string_of_list (function f -> "\"" ^ f ^ "\"") ", " ifs);
      if is_dc_type ty then
        fprintf oc "  %s<%s > %s(\"%s\", %s_ifiles, %b, %d, %d, %d);\n"
          cfg.sc_stream_in_dc_mod_name
          (string_of_type ty)
          bname
          bname
          bname
          cfg.sc_trace
          cfg.sc_stream_in_period
          cfg.sc_stream_in_hblank
          cfg.sc_stream_in_vblank
      else
        fprintf oc "  %s<%s > %s(\"%s\", %s_ifiles[0], %b, %d);\n"
          cfg.sc_stream_in_mod_name
          (string_of_type ty)
          bname
          bname
          bname
          cfg.sc_trace
          cfg.sc_stream_in_period;
      fprintf oc "  %s.%s(%s);\n" bname cfg.sc_mod_clock cfg.sc_mod_clock;
      fprintf oc "  %s.%s(w%d);\n" bname cfg.sc_stream_in_out_id wid;
      fprintf oc "  %s.started(_%s_started);\n" bname bname
  | InpB Syntax.ParamIn ->
     Misc.not_implemented "SystemC translation of input parameters"
  | OutB Syntax.StreamIO ->
      let wid, ty = begin match b.ib_ins with
        [id,(wid,ty)] -> wid, ty 
      | _ -> Misc.not_implemented "Systemc.dump_box: multi-input stream_out box" end in
      let ofs = Filepat.expand b.ib_device in
      fprintf oc "  char *%s_ofiles[] = { %s, 0 };\n"
        bname
        (Misc.string_of_list (function f -> "\"" ^ f ^ "\"") ", " ofs);
      if is_dc_type ty then
        fprintf oc "  %s<%s > %s(\"%s\", %s_ofiles, %b, %b, %d);\n"
          cfg.sc_stream_out_dc_mod_name
          (string_of_type ty)
          bname
          bname
          bname
          cfg.sc_trace
          !Streams.split_output_frames
          cfg.sc_stop_idle_time
      else
        fprintf oc "  %s<%s > %s(\"%s\", %s_ofiles[0], %b, %d);\n"
          cfg.sc_stream_out_mod_name
          (string_of_type ty)
          bname
          bname
          bname
          cfg.sc_trace
          cfg.sc_stop_idle_time;
      fprintf oc "  %s.%s(%s);\n" bname cfg.sc_mod_clock cfg.sc_mod_clock;
      fprintf oc "  %s.%s(w%d);\n" bname cfg.sc_stream_out_in_id wid;
      fprintf oc "  %s.started(_%s_started);\n" bname bname;
      fprintf oc "  %s.stopped(_%s_stopped);\n" bname bname
  | InpB Syntax.PortIO ->
      let wid, ty = begin match b.ib_outs with
        [id,([wid],ty)] -> wid, ty 
      | _ -> Misc.not_implemented "Systemc.dump_box: multi-output port_in box" end in
      fprintf oc "  %s<%s > %s(\"%s\", \"%s\", %s, %b);\n"
          cfg.sc_port_in_mod_name
          (string_of_type ty)
          bname
          bname
          b.ib_device
          (Misc.string_of_opt (function v -> string_of_val ty v) b.ib_ival)
          cfg.sc_trace;
      fprintf oc "  %s.%s(%s);\n" bname cfg.sc_mod_clock cfg.sc_mod_clock;
      fprintf oc "  %s.%s(w%d);\n" bname cfg.sc_stream_in_out_id wid
  | OutB Syntax.PortIO ->
      let wid, ty = begin match b.ib_ins with
        [id,(wid,ty)] -> wid, ty 
      | _ -> Misc.not_implemented "Systemc.dump_box: multi-input port_out box" end in
      fprintf oc "  %s<%s > %s(\"%s\", \"%s\", %b);\n"
          cfg.sc_port_out_mod_name (string_of_type ty) bname bname b.ib_device cfg.sc_trace;
      fprintf oc "  %s.%s(%s);\n" bname cfg.sc_mod_clock cfg.sc_mod_clock;
      fprintf oc "  %s.%s(w%d);\n" bname cfg.sc_stream_out_in_id wid
  | OutB Syntax.ParamIn ->
     Misc.fatal_error "Systemc.dump_box"
  | DummyB ->  Misc.fatal_error "Systemc.dump_box: dummy box"

and io_fmt dev = 
   if Filename.check_suffix dev "pgm" then "PGM"
   else if Filename.check_suffix dev "bin" then "Bin"
   else "Raw"

and string_of_param_values ir bname params =
  Misc.string_of_list (function p -> string_of_param_value ir bname p ^ ",") "" params

and string_of_param_value ir bname (name, (v,ty)) = match v with
  | Expr.Val_int (n,_) -> string_of_int n
  | Expr.Val_bool n -> string_of_bool n
  | Expr.Val_float n -> string_of_float n
  | Expr.Val_extern { Expr.ef_c_name = f } -> f
  | Expr.Val_fun (fid,_,_) ->
      let fd = Misc.lookup "Systemc.string_of_param_value" fid ir.ir_globfns in
      if TypeInsts.card fd.Syntax.gf_insts > 1 then 
        let ty_arg, _ = 
          try Types.arrow_types ty 
          with Invalid_argument _ -> failwith "Systemc.string_of_param_value" (* should not happen *) in
        full_fn_name fid (Some (fd,ty_arg))
      else
        fid 
  | Expr.Val_array1 (_, _)
  | Expr.Val_array2 (_, _)
  | Expr.Val_array3 (_, _) -> box_param_value_name bname name (* Array params are declared and passed as global values *)
  | _ -> failwith "Systemc.string_of_param_value"

and string_of_var_init_values bname vars =
  Misc.string_of_list (string_of_var_init_value bname) "" vars

and string_of_var_init_value bname = function 
  | name, (Some _, Types.Tconstr({tc_name="array"},_,_), _, _) ->
      box_var_value_name bname name ^ ", " (* Array var inits are declared and passed as global values *)
  | _, _ -> ""

and dump_box_input oc bname (id,(wid,ty)) =
     fprintf oc "  %s.%s(w%d);\n" bname id wid

and dump_box_output oc bname (id,(wids,ty)) =
   List.iter (function wid -> fprintf oc "  %s.%s(w%d);\n" bname id wid) wids
