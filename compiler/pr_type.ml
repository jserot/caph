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

(* Printing a type expression *)

open Types
open Buffer

let print_type_repr = ref false

module TypeVarNames =
  Varname.Make
    (struct
      type t = typ var
      let init = 0
      let to_string cnt = Misc.int_to_alpha 'a' cnt
    end)

module SizeVarNames =
  Varname.Make
    (struct
      type t = siz var
      let init = 0
      let to_string cnt = Misc.int_to_alpha 's' cnt
    end)

let print_var_quote = ref true

let output_string b s = add_string b s

let output_tvar, output_svar, output_tvar', output_svar' =
  let output_var pfx name_of b v =
    output_string b pfx;
    output_string b (name_of v);
(*     if !Misc.trace_level > 1 then begin *)
(*       output_string b "("; *)
(*       output_string b v.stamp; *)
(*       output_string b ")" *)
(*     end in *)
  in
  output_var ""  TypeVarNames.name_of,
  output_var "" SizeVarNames.name_of,
  output_var "'"  TypeVarNames.name_of,
  output_var "#" SizeVarNames.name_of

let output_int b n = output_string b (string_of_int n)

let output_list b f sep l =
  let rec outp = function
    [] ->
      ()
  | [v] ->
      f b v;
  | v::vs ->
      f b v;
      output_string b sep;
      outp vs in
  outp l

let rec output_typ priority b t =
  match type_repr t with
    Tvar v ->
      output_tvar' b v
  | Tarrow (ty1, ty2) ->
      if priority >= 1 then output_string b "(";
      output_typ 1 b ty1;
      output_string b " -> ";
      output_typ 0 b ty2;
      if priority >= 1 then output_string b ")"
  | Tproduct ts ->
      if priority >= 2 then output_string b "(";
      output_list b (output_typ 0) " * " ts;
      if priority >= 2 then output_string b ")"
  | Tconstr({tc_name="int"}, [sg], [sz]) -> (* special case *)
      begin match !print_type_repr, type_repr sg with
        false, Tconstr({tc_name="_signed" as name}, _, _) 
      | false, Tconstr({tc_name="_unsigned" as name}, _, _) ->
          output_string b (Misc.string_after "_" name);
          output_string b "<";
          output_siz b sz;
          output_string b ">"
      | _, _ ->
        output_string b "int";
        output_string b "<";
        output_typ 0 b sg;
        output_string b ",";
        output_siz b sz;
        output_string b ">"
      end
  | Tconstr({tc_name="array"}, [t], [s]) when not !print_type_repr ->
      output_typ 2 b t; output_string b " ";
      output_string b "array";
      output_string b "[";
      output_siz b s;
      output_string b "]"
  | Tconstr(cstr, args, sizes) ->
      begin match args with
        []    -> ()
      | [ty1] ->
          output_typ 2 b ty1; output_string b " "
      | tyl ->
          output_string b "(";
          output_list b (output_typ 0) "," tyl;
          output_string b ") "
      end;
      output_string b cstr.tc_name;
      begin match sizes with 
        [] -> ()
      | szs ->
          output_string b "<";
          output_list b output_siz "," szs;
          output_string b ">"
      end

and output_siz b sz =
  match Types.size_repr sz with
  | SzVar v ->
      output_svar b v
  | SzConst n ->
      output_int b n
  | SzRef k ->
      output_string b "@";
      output_int b k

let output_type b ty = output_typ 0 b ty

let output_one_type b ty =
  TypeVarNames.reset ();
  SizeVarNames.reset ();
  output_typ 0 b ty

let output_size b sz = output_siz b sz

let output_one_size b sz =
  SizeVarNames.reset ();
  output_siz b sz

let rec output_schema b sch =
  TypeVarNames.reset ();
  SizeVarNames.reset ();
  begin match sch.ts_tparams, sch.ts_sparams with 
    [], [] -> ()
  | [], sparams ->
      output_string b "forall ";
      output_list b output_svar' ", " sparams;
      output_string b ". "
  | tparams, [] ->
      output_string b "forall ";
      output_list b output_tvar' ", " tparams;
      output_string b ". "
  | tparams, sparams ->
      output_string b "forall ";
      output_list b output_tvar' ", " tparams;
      output_string b ", ";
      output_list b output_svar' ", " sparams;
      output_string b ". " end;
  output_typ 0 b sch.ts_body

let string_of_type ty = let b = Buffer.create 64 in output_type b ty; Buffer.contents b
let string_of_one_type ty = let b = Buffer.create 64 in output_one_type b ty; Buffer.contents b
let string_of_size sz = let b = Buffer.create 64 in output_size b sz; Buffer.contents b
let string_of_one_size sz = let b = Buffer.create 64 in output_one_size b sz; Buffer.contents b
let string_of_type_scheme ts = let b = Buffer.create 64 in output_schema b ts; Buffer.contents b
let string_of_tvar v = let b = Buffer.create 64 in output_tvar b v; Buffer.contents b
let string_of_svar v = let b = Buffer.create 64 in output_svar b v; Buffer.contents b
let string_of_constr c = c.cs_name
