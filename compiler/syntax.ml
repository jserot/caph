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

(* Abstract syntax *)

open Location

type ident = string
type uident = string

let allow_pragmas = ref true

type program =
  { types: ty_decl list;  
    vals: val_decl list; 
    actors: actor_decl list;
    ios: io_decl list;
    nets: net_decl list;
    decls: (string * string) list;
    pragmas: pragma_decl list }

and decl = 
    TyDecl of ty_decl
  | ValDecl of val_decl
  | ActDecl of actor_decl
  | IoDecl of io_decl
  | NetDecl of net_decl
  | PragmaDecl of pragma_decl

and ty_decl =
  { td_desc: tdecl_desc;
    td_loc: location }
and tdecl_desc =
    Variant_type_decl of string * type_param list * size_param list * constr_decl list (* name, params, defn *)
  | Abbrev_type_decl of string * type_expression                 (* name, body *)

and type_param = string
and size_param = string

and constr_decl =
  | Constr0_decl of string * int option
  | Constr1_decl of string * type_expression  * int option

and val_decl = {
    val_desc: val_desc;
    val_loc: location;
    mutable val_typ: Types.typ_scheme }
and val_desc = 
    VConst of string * expr * const_annot option (* Global constant *)
  | VFun of string * string list * expr * type_expression option (* Global function *)
  | VExtern of string * type_expression * string * string * string (* External function *)
               (* name, type, vhdl fn name, c fn name *)

and const_annot =
    Cst_annot of string     (* Reserved for future use *)

and pragma_decl = {
    pragma_desc: pragma_desc;
    pragma_loc: location }
and pragma_desc =
    string * string list (* name, args *)

(* Type expressions *)

and type_expression =
  { te_desc: type_expression_desc;
    te_loc: location;
    mutable te_typ: Types.typ }

and type_expression_desc =
  | Typevar of string
  | Typetuple of type_expression list
  | Typearrow of type_expression * type_expression
  | Typeconstr of ident * type_expression list * size_expression list
  | TypeIntRange of size_expression * size_expression  (* Only for local variables *)
  | TypeEnum of uident list    (* Only for local variables *)

(* Size expressions *)

and size_expression =
  { se_desc: size_expression_desc;
    se_loc: location }

and size_expression_desc =
  | Sizeconst of int
  | Sizevar of string

(* Expressions *)

and expr = 
  { e_desc: expr_desc;
    e_loc: location;
    mutable e_typ: Types.typ }

and expr_desc =
    EConst of Const.t
  | EVar of ident
  | EAttr of ident * ident
  | ECon of ident * expr list (* Constr application *)
  | ECond of expr * expr * expr
  | ELet of (ident * expr) list * expr (* [Let([v1=e1;...;vn=en],e)] means "let v1=a1 in let .. in let vn=en in e" *)
  | EApp of expr * (gf_desc * Types.typ) option * expr list 
        (* Note 2015-06-11, JS.
           When applying _global_ fns, we annote the application with a global function descriptor and
           actual argument type. These informations will be used by the backend code generators *)
  | EArrayExt1 of expr list               (* Note 2014-04-22. Now limited to array initialisation *)
  | EArrayExt2 of expr list list
  | EArrayExt3 of expr list list list
  | EArrayCompr of array_comprehension
  | EArrRead of ident * array_index list
  | EIgnored   (* Only in rule rhs *)
  | ECast of expr * type_expression
  | ETuple of expr list

and index_range = string * expr * expr   (* id, lower bound, higher bound *)

and array_index = expr

and array_comprehension = index_range list * expr

(* Global function descriptors *)

and gf_desc = { 
    gf_def: gf_defn;                                                        (* Definition *)
    mutable gf_insts: (expr, Location.location) Types.TypeInsts.t;   (* Instances *)
  }

and gf_defn = { 
    gf_args: string list;
    gf_body: expr;
    gf_typ: Types.typ_scheme;
  }

(* Actors *)

and actor_decl =
  { act_desc: act_desc;
    act_loc: location }
and act_desc = {
  a_id: ident;
  a_params: param list;
  a_ins: actor_io list;
  a_outs: actor_io list;
  a_vars: var list;
  a_rsch: rule_schema;
  a_rules: rule list;
  mutable a_impl: actor_impl
  }

and actor_impl = { 
  ai_vhdl: string;
  ai_systemc: string;
  }

and param =
  { param_desc: ident * type_expression;
    param_loc: location }

and actor_io = 
  { aio_desc: ident * type_expression;
    aio_loc: location }

and var = 
  { var_desc: ident * type_expression * expr option;
    var_loc: location }

and rule_schema = 
  { rsch_desc: rule_comp list * rule_comp list;
    rsch_loc: location }

and rule_comp = qualifier
    
and rule = 
  { rule_desc: rule_lhs * rule_guard * rule_rhs;
    rule_loc: location }

and rule_lhs = 
  { rlhs_desc: qualified_rule_pattern list;
    rlhs_loc: location }

and rule_rhs = 
  { rrhs_desc: qualified_expr list;
    rrhs_loc: location }

and rule_guard = 
  { rgrd_desc: expr list;
    rgrd_loc: location }

and qualified_rule_pattern = qualifier * rule_pattern

and qualified_expr = qualifier * expr

and qualifier =
  { q_desc: qual_desc;
    q_loc: location }

and qual_desc = 
    QIdent of string * expr list        (* Before pre-processing *)
  | QNone 
  | QIn of string                       (* After pre-processing *)
  | QOut of string
  | QVar of string * expr list        

(* Actor rule patterns *)

and rule_pattern =
  { rp_desc: rpat_desc;
    rp_loc: location;
    mutable rp_typ: Types.typ }

and rpat_desc = 
    RPatConst of Const.t
  | RPatVar of ident
  | RPatWild      (* '_' : for inputs -> read but dont care about value; for variables or state -> wildcard *)
  | RPatCon of ident * rule_pattern list

(* Network ios *)

and io_decl =
  { io_desc: io_decl_desc;
    io_loc: location }

and io_decl_desc = io_kind * io_id * type_expression * io_dir * io_dev * expr option

and io_kind = StreamIO | PortIO

and io_id = ident
and io_dir = IoIn | IoOut
and io_dev = string (* file or socket description *)

(* Network declarations *)

and net_decl =
  { nd_desc: net_decl_desc;
    nd_loc: location }

and net_decl_desc = bool * net_binding list

and net_binding =
  { nb_desc: net_binding_desc;
    nb_loc: location }

and net_binding_desc = net_pattern * net_expr 

and net_pattern =
  { np_desc: net_pattern_desc;
    np_loc: location;
    mutable np_typ: Types.typ }

and net_pattern_desc =
  | NPat_var of string
  | NPat_tuple of net_pattern list

and net_expr =
  { ne_desc: net_expr_desc;
    ne_loc: location;
    mutable ne_typ: Types.typ }

and net_expr_desc =
     NConst of Const.t
   | NArray1Const of Const.t list
   | NArray2Const of Const.t list list
   | NArray3Const of Const.t list list list
   | NArrayItem of ident * net_expr list
   | NVar of ident
   | NCast of net_expr * type_expression
   | NTuple of net_expr list
   | NApp of net_expr * net_expr
   | NFun of net_pattern * net_expr (* single match here ! *)
   | NLet of bool * net_binding list * net_expr (* rec / non rec*)

(* Accessors *)

let array_dim te =
  match te with 
  | Typeconstr("array", [te1], [se1]) ->
      begin match te1.te_desc, se1.se_desc with
      | Typeconstr("array", [te2], [se2]), s1 ->
          begin match te2.te_desc, se2.se_desc with
          | Typeconstr("array", [te3], [se3]), s2  ->  
              te3.te_desc, [s1;s2;se3.se_desc]    
          | _, s2 ->                                      
              te2.te_desc, [s1; s2]
          end
      | _, _ ->
          te1.te_desc, [se1.se_desc]
      end
  | _ -> te, []

let rec is_simple_rule_pattern p = match p.rp_desc with 
  (* Note 2015-08-13, JS
     We want to disallow nested rule patterns, such as [i:Con(...,Con'(...),...)] *)
    RPatConst _
  | RPatVar _
  | RPatWild -> true
  | RPatCon (_, ps) -> List.for_all is_simple_rule_pattern ps

let is_fun_definition = function
  { nb_desc={np_desc=NPat_var _}, {ne_desc=NFun (_,_)} } -> true
| _ -> false

let name_of_ty_decl d = match d.td_desc with
  Variant_type_decl (s,_,_,_) -> s
| Abbrev_type_decl (s,_) -> s

let name_of_val_decl d = match d.val_desc with
  VConst (s,_,_) -> s
| VFun (s,_,_,_) -> s
| VExtern (s,_,_,_,_) -> s

let name_of_actor_decl d = match d.act_desc with { a_id = s } -> s

let name_of_io_decl d = match d.io_desc with (_, s, _, _, _, _) -> s

let name_of_pragma_decl d = match d.pragma_desc with (s, _) -> s

let rec names_of_net_pattern p = match p.np_desc with
  NPat_var v -> [v]
| NPat_tuple ps -> List.flatten (List.map names_of_net_pattern ps)

let names_of_net_binding ns b = match b.nb_desc with
  p, _ -> ns @ names_of_net_pattern p

let names_of_net_decl d = match d.nd_desc with
  _, bs -> List.fold_left names_of_net_binding [] bs

(* Program manipulation *)

let no_impl = { ai_vhdl = ""; ai_systemc = "" }

let empty_program = { types=[]; vals=[]; actors=[]; ios=[]; nets=[]; pragmas=[]; decls=[] }

let add_program_decl what p name =
    if List.mem_assoc name p.decls
    then
      begin match what, List.assoc name p.decls with
      | "pragma", _
      | "io", "net"
      | "net", "io" -> () (* These classes of redefinition are normal *)
      | _,  what' -> Error.global_redefinition what' what name
    end;
    { p with decls = (name,what) :: p.decls }

let add_type_decl p d = let p' = add_program_decl "type" p (name_of_ty_decl d) in { p' with types = p'.types @ [d] }

let add_val_decl p d = let p' = add_program_decl "value" p (name_of_val_decl d) in { p' with vals = p'.vals @ [d] }

let add_actor_decl p d = let p' = add_program_decl "actor" p (name_of_actor_decl d) in { p' with actors = p'.actors @ [d] }

let add_io_decl p d = let p' = add_program_decl "io" p (name_of_io_decl d) in { p' with ios = p'.ios @ [d] }

let add_net_decl p d =
  let names = names_of_net_decl d in
  let p' = List.fold_left (add_program_decl "net") p names in
  { p' with nets = p'.nets @ [d] }

let add_pragma_decl p d =
  let p' = add_program_decl "pragma" p (name_of_pragma_decl d) in
  { p' with pragmas = p'.pragmas @ [d] }

let add_decl p decl =
  match decl with
    TyDecl d -> add_type_decl p d
  | ValDecl d -> add_val_decl p d
  | ActDecl d -> add_actor_decl p d
  | IoDecl d -> add_io_decl p d
  | NetDecl d -> add_net_decl p d
  | PragmaDecl d -> add_pragma_decl p d

let check_actor_desc a =
  let check what what' f f' ds ds'=
    List.iter
      (function d ->
        if List.exists (function d' -> f' d' = f d) ds'
        then Error.actor_redefinition a.a_id what what' (f d))
      ds
  in
  (* Check for name clash btw local variables and i/os *)
  check "parameter" "input" (function {param_desc=id,_} -> id) (function {aio_desc=id,_} -> id) a.a_params a.a_ins;
  check "parameter" "output" (function {param_desc=id,_} -> id) (function {aio_desc=id,_} -> id) a.a_params a.a_outs;
  check "output" "input" (function {aio_desc=id,_} -> id) (function {aio_desc=id,_} -> id) a.a_outs a.a_ins;
  check "variable" "input" (function {var_desc=id,_,_} -> id) (function {aio_desc=id,_} -> id) a.a_vars a.a_ins;
  check "variable" "output" (function {var_desc=id,_,_} -> id) (function {aio_desc=id,_} -> id) a.a_vars a.a_outs;
  check "variable" "parameter" (function {var_desc=id,_,_} -> id) (function {param_desc=id,_} -> id) a.a_vars a.a_params

(* Printing *)

let rec string_of_expr e = string_of_exp e.e_desc

and string_of_exp e = match e with
  (* TODO : add parens to reflect nesting and priorities *)
    EConst c -> Const.string_of_const c
  | EVar v -> v
  | EAttr (v,a) -> v ^ "." ^ a
  | ECon (c,[]) -> c
  | ECon (c,es) -> c ^ "(" ^ Misc.string_of_list string_of_expr "," es ^ ")"
  | ECond (e1,e2,e3) -> "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^ " else " ^ string_of_expr e3
  | ELet (bs,e2) -> "let " ^ string_of_letdefs bs ^ " in " ^ string_of_expr e2
  | EApp ({e_desc=EVar (">>" as op)},_,[e1;e2]) -> "(" ^ string_of_expr e1 ^ ")" ^ op ^ string_of_expr e2
  | EApp ({e_desc=EVar ("<<" as op)},_,[e1;e2]) -> "(" ^ string_of_expr e1 ^ ")" ^ op ^ string_of_expr e2
  | EApp ({e_desc=EVar op},_,[e1;e2]) when is_binop op -> string_of_expr e1 ^ op ^ string_of_expr e2
  | EApp ({e_desc=EVar op},_,es) -> op ^ "(" ^ Misc.string_of_list string_of_expr "," es ^ ")"
  | EApp (_,_,_) -> Misc.fatal_error "Syntax.string_of_expr" (* should not happen *)
  | EArrRead (ar,[idx]) -> ar ^ "[" ^ string_of_expr idx ^ "]"
  | EArrRead (ar,[idx1;idx2]) -> ar ^ "[" ^ string_of_expr idx1 ^ "]"^ "[" ^ string_of_expr idx2 ^ "]"
  | EArrRead (ar,[idx1;idx2;idx3]) ->
      ar ^ "[" ^ string_of_expr idx1 ^ "]" ^ "[" ^ string_of_expr idx2 ^ "]" ^ "[" ^ string_of_expr idx3 ^ "]"
  | EArrRead (_, _) -> failwith "Syntax.string_of_exp: EArrRead"  (* should not happen *)
  | ECast (e,te) ->
      let t = begin match te.te_typ with
        Types.Tproduct [] -> string_of_ty_expr te
      | t -> Pr_type.string_of_type t end in
      "(" ^ string_of_expr e ^ " : " ^ t ^ ")"
  | EIgnored -> "_"
  | EArrayExt1 es -> string_of_array_row es
  | EArrayExt2 ess -> "[" ^ Misc.string_of_list string_of_array_row "," ess ^ "]"
  | EArrayExt3 esss -> "[" ^ Misc.string_of_list string_of_array_rows "," esss ^ "]"
  | EArrayCompr a -> string_of_array_compr a
  | ETuple es -> "(" ^ Misc.string_of_list string_of_expr "," es ^ ")"

and string_of_array_compr (irs,e) = 
      "[" ^ Misc.string_of_list string_of_index_range "," irs ^ " <- " ^ string_of_expr e ^ "]"

and string_of_index_range (id,lo,hi) = id ^ " in " ^ string_of_expr lo ^ " .. " ^ string_of_expr hi

and string_of_array_row vs = "[" ^ Misc.string_of_list string_of_expr "," vs ^ "]"

and string_of_array_rows vss = "[" ^ Misc.string_of_list string_of_array_row "," vss ^ "]"

and string_of_letdefs bs = Misc.string_of_list string_of_letdef " in " bs

and string_of_letdef (v,e) = v ^ "=" ^ string_of_expr e

and is_unop op = List.mem op [ "lnot"; "not"; "~-"; "~-." ]      (* TO ADJUST WITH PARSER/LEXER *)

and is_binop op = List.mem op [                                  (* TO ADJUST WITH PARSER/LEXER *)
  "+"; "-"; "*"; "/"; "+."; "-."; "*."; "/."; "mod";
  "<"; ">"; "="; "!="; "="; ">="; "<="; "&&"; "||";
  "land"; "lnand"; "lor"; "lnor"; "lxor"; "lxnor"; "<<"; ">>";
   "=."; "!=."; ">."; "<."; ">=."; "<=." ]
   
and is_op p id = List.exists (function {act_desc={a_id=id'}} when id=id' -> true | _ -> false) p.actors

and string_of_ty_expr te = string_of_ty_exp te.te_desc

and string_of_ty_exp = function
  | Typevar v -> v
  | Typetuple ts -> "(" ^ Misc.string_of_list string_of_ty_expr "," ts ^ ")"
  | Typearrow (t1,t2) -> string_of_ty_expr t1 ^ "->" ^ string_of_ty_expr t2
  | Typeconstr("int",[t],[s]) -> "int<" ^ string_of_ty_expr t ^ "," ^ string_of_size_expr s ^ ">" 
  | Typeconstr("array",[_],[_]) as t ->
      let t', ss = array_dim t in
      string_of_ty_exp t' ^ " array" ^ Misc.string_of_list (function s -> "[" ^ string_of_size_exp s ^ "]") "" ss 
  | Typeconstr(c,[],szs) -> c ^ string_of_size_exprs szs
  | Typeconstr(c,[t],szs) -> string_of_ty_expr t ^ " " ^ c ^ string_of_size_exprs szs
  | Typeconstr(c,ts,szs) -> "(" ^ Misc.string_of_list string_of_ty_expr "," ts ^ ") " ^ c ^ string_of_size_exprs szs
  | TypeEnum cs -> "{" ^ Misc.string_of_list (function id -> id) "," cs ^ "}"
  | TypeIntRange (lo,hi) -> "{" ^ string_of_size_expr lo ^ ",..," ^ string_of_size_expr hi ^ "}"

and string_of_size_exprs = function
    [] -> ""
  | ss ->  "<" ^ Misc.string_of_list string_of_size_expr "," ss ^ ">"

and string_of_size_expr e = string_of_size_exp e.se_desc

and string_of_size_exp e =
  match e with
    Sizeconst n -> string_of_int n
  | Sizevar v -> v

let string_of_const = Const.string_of_const

let rec string_of_qualified_pattern (q,p) = 
  string_of_qualifier q.q_desc ^ ":" ^ string_of_rpat p

and string_of_qualified_expr (q,e) = 
  string_of_qualifier q.q_desc ^ ":" ^ string_of_expr e

and string_of_qualifier = function 
    QNone -> ""
  | QIdent (i, []) -> i
  | QIdent (i,js) -> i ^ "[" ^ Misc.string_of_list string_of_expr "," js ^ "]"
  | QIn i -> i
  | QOut i -> i
  | QVar (i,[]) -> i
  | QVar (i,js) -> i ^ "[" ^ Misc.string_of_list string_of_expr "," js ^ "]"

and string_of_rpat p = string_of_rpat_desc p.rp_desc

and string_of_rpat_desc p = match p with
    RPatConst c -> Const.string_of_const c
  | RPatVar v -> v
  | RPatCon (c,[]) -> c
  | RPatCon (c,ps) -> c ^ "("  ^ Misc.string_of_list string_of_rpat "," ps ^ ")"
  | RPatWild -> "_"

let string_of_guards gds = match gds with
  [] -> ""
| _ -> "when " ^ Misc.string_of_list string_of_expr "and " gds

let string_of_rule r = match r.rule_desc with 
  lhs, gds, rhs ->
      Misc.string_of_listp string_of_qualified_pattern ", " lhs.rlhs_desc
    ^ string_of_guards gds.rgrd_desc
    ^ " -> "
    ^ Misc.string_of_listp string_of_qualified_expr ", " rhs.rrhs_desc

let string_of_const_annot = function None -> "" | Some (Cst_annot ann) -> "[" ^ ann ^ "]"

let string_of_io_kind = function StreamIO -> "stream" | PortIO -> "port"

let string_of_const = Const.string_of_const

let rec string_of_net_expr ne = string_of_net_exp ne.ne_desc

and string_of_net_exp = function
   | NVar v -> v
   | NConst c -> string_of_const c
   | NTuple es -> "(" ^ Misc.string_of_list string_of_net_expr "," es ^ ")"
   | NApp (e1,e2) -> string_of_net_expr e1 ^ " " ^ string_of_net_expr e2
   | NFun (p,e) -> "<fun>"
   | NLet (isrec,nbs,e) ->
         "let " ^ if isrec then "rec " else " " ^  Misc.string_of_list string_of_net_binding "and" nbs
       ^ " in " ^ string_of_net_expr e
   | NCast (e,te) -> "(" ^ string_of_net_expr e ^ ":" ^ string_of_ty_expr te ^ ")"
   | _ -> failwith "Syntax.string_of_net_expr: ..."

and string_of_net_binding nb = string_of_net_bind nb.nb_desc

and string_of_net_bind (np,ne) = string_of_net_pattern np ^ "=" ^ string_of_net_expr ne

and string_of_net_pattern np = string_of_net_pat np.np_desc

and string_of_net_pat = function
    NPat_var v -> v
  | NPat_tuple ps -> "(" ^ Misc.string_of_list string_of_net_pattern "," ps ^ ")"

(* Re-externalizing actor decls *)

let string_of_opt_expr = function None -> "" | Some e -> " = " ^ string_of_expr e
let string_of_actor_io {aio_desc=id,ty} = id ^ ":" ^ string_of_ty_expr ty
let string_of_actor_param {param_desc=id,ty} = id ^ ":" ^ string_of_ty_expr ty
let string_of_actor_var {var_desc=(id,ty,iv)} = id ^ ":" ^ string_of_ty_expr ty ^ string_of_opt_expr iv
let string_of_qualifiers qs = 
  let s = Misc.string_of_list (function q -> string_of_qualifier q.q_desc) "," qs in
  if List.length qs > 2 then "(" ^ s ^ ")" else s
let string_of_actor_rule_schema {rsch_desc=(lhs,rhs)} = 
  match lhs,rhs with
    [],[] -> ""
  | _,_ -> string_of_qualifiers lhs ^ " -> " ^ string_of_qualifiers rhs

let dump_actor_decl oc a = 
  Printf.fprintf oc "actor %s (%s)\n" a.a_id (Misc.string_of_list string_of_actor_param ", " a.a_params);
  Printf.fprintf oc "  in (%s)\n" (Misc.string_of_list string_of_actor_io ", " a.a_ins);
  Printf.fprintf oc " out (%s)\n" (Misc.string_of_list string_of_actor_io ", " a.a_outs);
  List.iter (function v -> Printf.fprintf oc "var %s\n" (string_of_actor_var v)) a.a_vars;
  Printf.fprintf oc "rules %s\n" (string_of_actor_rule_schema a.a_rsch);
  List.iter (function r -> Printf.fprintf oc "| %s\n" (string_of_rule r)) a.a_rules;
  Printf.fprintf oc ";\n"

let dump_actor_declaration oc a = dump_actor_decl oc a.act_desc 

let dump_actor_decls p = List.iter (dump_actor_declaration stdout) p.actors

(* (\* Internalizers - used to build location-less representations *\) *)

(* let mk_szexp d = { se_desc = d; se_loc = Location.no_location } *)

(* let mk_tyexp d = *)
(*   let rec mk = match d with *)
(*     Typetuple tes -> Typetuple (List.map mk tes) *)
(*   | Typearrow (te1,te2) -> Typearrow (mk te1, mk te2) *)
(*   | Typeconstr (c,tes,ses) -> Typeconstr (c, List.map mk tes, List.map mk_szexp ses) *)
(*   | te -> te in *)
(*   { te_desc = mk d; te_loc = Location.no_location; te_typ = Types.no_type } *)

(* let mk_exp d = *)
(*   let rec mk = match d with *)
(*   | ECon (c,es) -> ECon (c, List.map mk es) *)
(*   | ECond (e1,e2,e3) -> ECond (mk e1, mk e2, mk e2) *)
(*   | ELet (bs,e2) -> ELet (bs, mk e2) *)
(*   | EApp (e1,x,es) -> EApp(mk e1, x, List.map mk es) *)
(*   | EArrRead (ar,idxs) -> EArrRead (ar, List.map mk idxs) *)
(*   | ECast (e,te) -> ECast (mk e, mk_tyexp te) *)
(*   | EArrayExt1 es -> EArrayExt (List.map mk es) *)
(*   | EArrayExt2 ess -> EArrayExt (List.map (function es -> List.map mk es)) *)
(*   | EArrayExt3 esss -> EArrayExt (List.map (function ess -> List.map (function es -> List.map mk es))) *)
(*   | EArrayCompr (ir,e) -> EArrayCompr (ir, mk e) *)
(*   | ETuple es -> ETuple (List.map es) *)
(*   | e -> e in *)
(*   { e_desc = mk d; e_loc = Location.no_location; e_typ = Types.no_type } *)

(* let mk_int c = mk_exp (EConst (CInt (c,None))) *)

(* let mk_io (id,ty) = { aio_desc = (id, ty); aio_loc = Location.no_location } *)
(* let mk_param (id,ty) = { param_desc = (id, ty); param_loc = Location.no_location } *)
(* let mk_var (id,ty,iv) = { var_desc = (id, mk_tyexp ty, iv); var_loc = Location.no_location } *)
(* let mk_rsch d = { rsch_desc = d; rsch_loc = Location.no_location } *)
(* let mk_rqual d = { q_desc = d; q_loc = Location.no_location } *)
(* let mk_rpat d = { rp_desc = d; rp_loc = Location.no_location; rp_typ = Types.no_type } *)
(* let mk_rlhs d = { rlhs_desc = List.map (function (q,rp) -> mk_rqual q, mk_rpat rp) d; rlhs_loc = Location.no_location } *)
(* let mk_rgrd d = { rgrd_desc = d; rgrd_loc = Location.no_location } *)
(* let mk_rrhs d = { rrhs_desc = List.map (function (q,e) -> mk_rqual q, mk_exp e) d; rrhs_loc = Location.no_location } *)
(* let mk_rule (lhs,guard,rhs) = { rule_desc = mk_rlhs lhs, mk_rgrd guard, mk_rrhs rhs; rule_loc = Location.no_location } *)
(* let no_impl = { ai_vhdl=""; ai_systemc="" } *)
(* let no_rsch = mk_rsch ([],[]) *)

(* Translation from unqualified to qualified rule patterns and exprs *)
(* v1.8.0, 2013-01-12, JS *)

let rec qualify_actor_rules a = { a with act_desc = qualify_rules a.act_desc }

and qualify_rules a = { a with a_rules = List.map (qualify_rule a) a.a_rules }

and qualify_rule a { rule_desc = lhs, guard, rhs; rule_loc = loc } = 
  let qualify_rpat a i (q,p) = qualify a (i, fst a.a_rsch.rsch_desc) p.rp_loc q, p in 
  let qualify_rexp a i (q,e) = qualify a (i, snd a.a_rsch.rsch_desc) e.e_loc q, e in 
  let qualify_lhs lhs = { lhs with rlhs_desc = Misc.list_map_index (qualify_rpat a) lhs.rlhs_desc } in
  let qualify_rhs rhs = { rhs with rrhs_desc = Misc.list_map_index (qualify_rexp a) rhs.rrhs_desc } in
  { rule_desc = qualify_lhs lhs, guard, qualify_rhs rhs;
    rule_loc = loc }

and qualify a (i,r_comps) loc q =
  let rec qual = function
    QNone -> (* The pattern (resp. expr) was unqualified.
                We retrieve the qualifier using the rule schema  and the provided index *)
      let q' =
        begin try List.nth r_comps i 
        with Failure "nth" -> Error.rule_mismatch_err loc  end in
     qual q'.q_desc
  | QIdent (i,[]) -> (* The pattern (resp. expr) was qualified with an identifier.
                        We determine whether this identifier corresponds to an input, output or variable in this actor *)
      begin match
          List.exists (function inp -> fst inp.aio_desc = i) a.a_ins,
          List.exists (function outp -> fst outp.aio_desc = i) a.a_outs,
          List.exists (function var -> Misc.fst3 var.var_desc = i) a.a_vars with
        true, false, false -> QIn i
      | false, true, false -> QOut i
      | false, false, true -> QVar (i,[])
      | false, false, false -> Error.invalid_qualifier i loc
      | _, _, _ -> Error.ambiguous_qualifier i loc end
  | QIdent (i,j) -> (* The pattern (resp. expr) was qualified with an identifier indexed by an expr.
                       The identifier must correspond to a variable with an array type *)
      begin match
          List.exists (function var -> Misc.fst3 var.var_desc = i) a.a_vars with
      | true -> QVar (i,j)
      | false -> Error.invalid_qualifier i loc end
  | q -> q in
  { q_desc = qual q.q_desc; q_loc = q.q_loc }


(* Translation from unqualified to qualified rule patterns and exprs *)

let rec qualify_actor_rules a = { a with act_desc = qualify_rules a.act_desc }

and qualify_rules a = { a with a_rules = List.map (qualify_rule a) a.a_rules }

and qualify_rule a { rule_desc = lhs, guard, rhs; rule_loc = loc } = 
  let qualify_rpat a i (q,p) = qualify a (i, fst a.a_rsch.rsch_desc) p.rp_loc q, p in 
  let qualify_rexp a i (q,e) = qualify a (i, snd a.a_rsch.rsch_desc) e.e_loc q, e in 
  let qualify_lhs lhs = { lhs with rlhs_desc = Misc.list_map_index (qualify_rpat a) lhs.rlhs_desc } in
  let qualify_rhs rhs = { rhs with rrhs_desc = Misc.list_map_index (qualify_rexp a) rhs.rrhs_desc } in
  { rule_desc = qualify_lhs lhs, guard, qualify_rhs rhs;
    rule_loc = loc }

and qualify a (i,r_comps) loc q =
  let rec qual = function
    QNone -> (* The pattern (resp. expr) was unqualified.
                We retrieve the qualifier using the rule schema  and the provided index *)
      let q' =
        begin try List.nth r_comps i 
        with Failure "nth" -> Error.rule_mismatch_err loc  end in
     qual q'.q_desc
  | QIdent (i,[]) -> (* The pattern (resp. expr) was qualified with an identifier.
                        We determine whether this identifier corresponds to an input, output or variable in this actor *)
      begin match
          List.exists (function inp -> fst inp.aio_desc = i) a.a_ins,
          List.exists (function outp -> fst outp.aio_desc = i) a.a_outs,
          List.exists (function var -> Misc.fst3 var.var_desc = i) a.a_vars with
        true, false, false -> QIn i
      | false, true, false -> QOut i
      | false, false, true -> QVar (i,[])
      | false, false, false -> Error.invalid_qualifier i loc
      | _, _, _ -> Error.ambiguous_qualifier i loc end
  | QIdent (i,j) -> (* The pattern (resp. expr) was qualified with an identifier indexed by an expr.
                       The identifier must correspond to a variable with an array type *)
      begin match
          List.exists (function var -> Misc.fst3 var.var_desc = i) a.a_vars with
      | true -> QVar (i,j)
      | false -> Error.invalid_qualifier i loc end
  | q -> q in
  { q_desc = qual q.q_desc; q_loc = q.q_loc }

(* Collecting #pragma's *)

let parse_pragmas actors pragmas =
    List.fold_left
      (fun impls p -> match p.pragma_desc with
        "implemented", [actor_id;target;src_file] when List.mem target ["vhdl";"systemc"] ->  (actor_id,(target,src_file)) :: impls
      | "implemented", _ -> Error.invalid_pragma "implemented" "illegal format" p.pragma_loc
      | _, _ -> impls)
      []
      pragmas

let rec update_actor impls ad =
  let a = ad.act_desc in
  a.a_impl <- get_actor_impl impls a.a_id

and get_actor_impl impls id =
  let ips = List.fold_left (fun z (id',(t,s)) -> if id=id' then (t,s)::z else z) [] impls in
  { ai_vhdl = (try List.assoc "vhdl" ips with Not_found -> "");
    ai_systemc = (try List.assoc "systemc" ips with Not_found -> "") }

(* Pre-processing *)

let pre_process p =
  let pragmas = if !allow_pragmas then p.pragmas else [] in
  let impls = parse_pragmas p.actors pragmas in
  let p' = { p with actors = List.map qualify_actor_rules p.actors } in
  List.iter (update_actor impls) p'.actors;
  p'
