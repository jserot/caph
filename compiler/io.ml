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

(* Definitions common to streams and port IOs *)

(* open Dsval *)
open Misc
open Error
open Expr
open Types

let stdin_file = ref "stdin"
let stdout_file = ref "stdout"

let file_trace = ref false (* for debug only *)

let rec string_of_value v =
  let b = Buffer.create 80 in
  Stream.iter (Buffer.add_string b) (stream_of_value v);
  Buffer.contents b

and stream_of_value v = [< '(Expr.string_of_val v ^ " ") >]

let stream_close_fd fd = function () -> Unix.close fd

(* Parsers for scalar and constructed types *)

let rec mk_parser tp ty = 
  match real_type ty with
    Tconstr (_,_,_) ->
      begin match Typing.type_desc_of_type tp ty with
      | { ty_desc=Abstract_type; ty_constr={tc_name="int"} } -> p_int
      | { ty_desc=Abstract_type; ty_constr={tc_name="bool"} } -> p_bool
      | { ty_desc=Abstract_type; ty_constr={tc_name="float"} } -> p_float
      | { ty_desc=Variant_type (_,_,cds); ty_constr={tc_name="dc"} } when !Expr.abbrev_dc_ctors ->
          let c =
            begin
              try List.find (function c -> c.cs_name = "Data") cds
              with Not_found -> fatal_error "Streams.mk_parser"  (* should not happen *)
            end in
          let p_arg =
            begin match mk_variant_arg_parser tp ty c with
              | Some p -> p 
              | None -> cannot_build_parser_for_type ty
            end in
          p_dc p_arg
      | { ty_desc=Variant_type (_,_,cds); ty_constr={tc_name=name} } -> p_variant (mk_variant_arg_parsers tp ty cds)
      | _ -> cannot_build_parser_for_type ty end
  | Tproduct ts ->
      p_tuple (List.map (mk_parser tp) ts)
  | t ->
      cannot_build_parser_for_type t

and p_int = parser [< 'Genlex.Int i >] -> Some (Val_int (i,None))

and p_bool = parser
    [< 'Genlex.Int b >] -> Some (Val_bool (bool_of_int b))
  | [< 'Genlex.Kwd "true" >] -> Some (Val_bool true)
  | [< 'Genlex.Kwd "false" >] -> Some (Val_bool false)

and bool_of_int = function 0 -> false | 1 -> true | _ -> fatal_error "Io.bool_of_int"

and p_float = parser [< 'Genlex.Float i >] -> Some (Val_float i)

and p_tuple p_args =
  (* parser [< 'Genlex.Kwd "("; vs=p_list p_args; 'Genlex.Kwd ")" >] -> *)
  (* Note 2015-08-16, JS.
     Parsing "verbose" tuples is more fragile in c++.
     It's probably safer to get back to a "flat" (no paren, non comma) external representation *)
  parser [< vs=p_sequence p_args >] ->
    try
      Some (Val_tuple (List.map (function Some v -> v | None -> raise (Failure "Io.p_tuple")) vs))
    with
      Failure "Io.p_tuple" -> None

and p_list p_args = match p_args with
  [] -> (parser [< >] -> [])
| p::ps -> (parser [< v=p; vs=p_list' ps >] -> v::vs) 
and p_list' p_args = parser 
| [< 'Genlex.Kwd ","; vs = p_list p_args >] -> vs
| [< >] -> []

and p_sequence p_args = match p_args with
  [] -> (parser [< >] -> [])
| p::ps -> (parser [< v=p; vs=p_sequence ps >] -> v::vs) 

and p_variant p_args = 
  parser [< c = p_con p_args;
            carg = match List.assoc c p_args with 
              None -> p_arg0
            | Some p_arg -> p_arg >] -> 
          begin match carg with
          Some Val_ignored -> Some (Val_con (c, []))
          | Some v -> Some (Val_con (c, [v]))
          | None -> None end

and p_arg0 = parser 
    [< >] -> Some Val_ignored  (* hack *)

and p_con cds = 
  parser [< 'Genlex.Ident c >] ->
    if List.mem_assoc c cds
    then c
    else fatal_error ("Parse error when reading input stream: unrecognized contructor " ^ c)

and mk_variant_arg_parsers tp ty cds =
  List.map (function c -> c.cs_name, mk_variant_arg_parser tp ty c) cds

and mk_variant_arg_parser tp ty c = 
  match c.cs_arity with
    0 ->
      None
  | 1 ->
      let ty' = actual_carg_type ty c in
      Some (mk_parser tp ty')
  | n ->
      let ty' = actual_carg_type ty c in
      Some (mk_parser tp ty')

and actual_carg_type ty cdesc =
  match real_type ty with
    Tconstr (tc, ty_params, sz_params) ->
      let t = type_copy (type_arrow cdesc.cs_arg cdesc.cs_res) in
      let ty_arg = new_type_var () in
      begin try
        Types.unify t (type_arrow ty_arg ty)
      with 
        Types.TypeConflict _ 
      | Types.TypeCircularity _ -> cannot_build_parser_for_type ty end;
      ty_arg
  | _ -> cannot_build_parser_for_type ty

and p_dc p_arg = parser
        [< 'Genlex.Ident "<" >] -> Some (Val_con ("SoS", []))
      | [< 'Genlex.Ident ">" >] -> Some (Val_con ("EoS", []))
      | [< v = p_arg >] -> match v with Some v -> Some (Val_con ("Data", [v])) | None -> None


