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

(* Printing of error messages and warnings *)

open Misc
open Location
open Printf

type err_site =
    ELoc of Location.location
  | EItem of string

let report_cast_warnings = ref true

let unsupported_rule_pattern loc =
  eprintf "%aThis rule pattern is not (yet) supported.\n" 
    output_location loc ;
  raise Error

let rule_mismatch_err loc =
  eprintf "%aThis rule does not match the rule schema.\n" 
    output_location loc ;
  raise Error

let ambiguous_qualifier id loc =
  eprintf "%aThe qualifier of this pattern/expr is ambiguous\n" 
    output_location loc;
  raise Error

let invalid_qualifier id loc =
  eprintf "%aInvalid qualifier for this pattern/expr (neither a input, output or variable name)\n" 
    output_location loc;
  raise Error

let unbound_constant_err name loc =
  eprintf "%aThe constant identifier %a is unbound.\n" 
    output_location loc output_string name;
  raise Error

let unbound_rule_expr_var name loc =
  eprintf "%aThe variable identifier %a is unbound in the context of this rule expression.\n" 
    output_location loc output_string name;
  raise Error

let uninitialized_var_pat loc v =
  eprintf "%aThe variable %a in this pattern is not initialized.\n" 
    output_location loc output_string v;
  raise Error

let uninitialized_var loc v =
  eprintf "%aUnknown value for variable %a (probably not initialized).\n" 
    output_location loc output_string v;
  raise Error

let unbound_value_err name loc =
  eprintf "%aThe value identifier %a is unbound.\n" 
    output_location loc output_string name;
  raise Error

let application_of_non_function_err loc =
  eprintf "%aThis expression is not a function, it cannot be applied.\n"
      output_location loc;
  raise Error

let illegal_actor_instanciation loc =
  eprintf "%aThis expression is not a valid actor instanciation.\n"
      output_location loc;
  raise Error

let output_opt_msg oc msg = match msg with "" -> output_string oc "" | _ -> output_string oc (" (" ^ msg ^ ")")

let illegal_parameters aid loc msg =
  eprintf "%aIllegal parameter value(s) for actor %s%a.\n"
    output_location loc aid output_opt_msg msg;
  raise Error

let illegal_attribute loc attr =
  eprintf "%aIllegal or unknown attribute (%s).\n" output_location loc attr;
  raise Error

let illegal_application loc =
  eprintf "%aIllegal application.\n" output_location loc;
  raise Error

let illegal_expression loc =
  eprintf "%aThis expression is not valid in this context.\n"
      output_location loc;
  raise Error

let illegal_letrec_expr loc =
  eprintf "%aThis kind of expression is not allowed in \
           right-hand sides of \"let rec\".\n"
    output_location loc;
  raise Error

let illegal_guard_expr loc =
  eprintf "%aThis kind of expression is not allowed in guards.\n"
    output_location loc;
  raise Error

let illegal_constant loc what =
  eprintf "%aThis constant is illegal (%s).\n"
      output_location loc what;
  raise Error

let unbound_type_err name loc =
  eprintf "%aThe type identifier %a is unbound.\n" 
    output_location loc output_string name;
  raise Error

let unbound_constr_err ctid loc =
  eprintf "%aUnbound value constructor : %a.\n" 
    output_location loc output_string ctid;
  raise Error

let duplicate_param_in_type_decl_err loc =
  eprintf "%aRepeated type parameter in type declaration.\n"
    output_location loc;
  raise Error

let unbound_type_err ctid loc =
  eprintf "%aUnbound type constructor : %a.\n" 
    output_location loc output_string ctid;
  raise Error

let illegal_type_expr loc =
  eprintf "%aIllegal type expression.\n" 
    output_location loc;
  raise Error

let unbound_type_var_err v loc =
  eprintf "%aThe type variable %s is unbound.\n"
    output_location loc v;
  raise Error

let unbound_rule_component id loc =
  eprintf "%aUnbound rule component : %a.\n" 
    output_location loc output_string id;
  raise Error

let invalid_type ty loc =
  eprintf "%aThis type is unknown or invalid in this context : %s\n"
    output_location loc
    (Pr_type.string_of_one_type ty);
  raise Error

let spurious_type_var ty loc =
  eprintf "%aThe type %s contains generic variables, which is invalid in this context\n"
    output_location loc
    (Pr_type.string_of_one_type ty);
  raise Error

let cannot_type_constant loc =
  eprintf "%aCannot get type of constant here\n"
    output_location loc;
  raise Error

let wrong_type_err site ty1 ty2 = function
    ELoc loc -> 
      eprintf "%aAn error occured when typing this %s : types [%s] and [%s] cannot be unified.\n"
        output_location loc
        site
        (Pr_type.string_of_one_type ty1)
        (Pr_type.string_of_one_type ty2);
      raise Error
  | EItem item ->
      eprintf "An error occured when typing %s %s : types %s and %s cannot be unified.\n"
        site item
        (Pr_type.string_of_one_type ty1)
        (Pr_type.string_of_one_type ty2);
      raise Error

let type_warning ty1 ty2 = function
    ELoc loc -> 
      eprintf "%aWarning: types %s and %s have been unified.\n"
        output_location loc
        (Pr_type.string_of_one_type ty1)
        (Pr_type.string_of_one_type ty2)
  | EItem item ->
      eprintf "Warning: types %s and %s have been unified.\n"
        (Pr_type.string_of_one_type ty1)
        (Pr_type.string_of_one_type ty2)

let output_type_mismatch id ty1 ty2 =
  eprintf "An error occured when typing output %s : types %s and %s cannot be unified.\n"
    id
    (Pr_type.string_of_one_type ty1)
    (Pr_type.string_of_one_type ty2);
  raise Error

let circular_type_err site ty1 ty2 = function
    ELoc loc -> 
      eprintf "%aAn error occured when typing this %s : a cycle was detected between types %s and %s.\n"
        output_location loc
        site
        (Pr_type.string_of_one_type ty1)
        (Pr_type.string_of_one_type ty2);
      raise Error
  | EItem item ->
      eprintf "An error occured when typing %s %s : a cycle was detected between types %s and %s.\n"
        site item
        (Pr_type.string_of_one_type ty1)
        (Pr_type.string_of_one_type ty2);
      raise Error

let illegal_definition loc =
  eprintf "%aThis style of definition is illegal or not supported.\n"
    output_location loc;
  raise Error

let illegal_rec_definition loc =
  eprintf "%aThis style of recursive definition is illegal or not supported.\n"
    output_location loc;
  raise Error

let illegal_let_expr loc =
  eprintf "%aThis style of let expression is illegal or not supported.\n"
    output_location loc;
  raise Error

let illegal_type loc what item ty =
  eprintf "%aIllegal type (%s) for %s \"%s\".\n"
    output_location loc (Pr_type.string_of_one_type ty) what item;
  raise Error

let illegal_rule_pattern loc =
  eprintf "%aThis type of rule pattern is illegal or not supported.\n"
    output_location loc;
  raise Error

let illegal_rule_expr loc =
  eprintf "%aThis type of rule expression is illegal or not supported.\n"
    output_location loc;
  raise Error

let illegal_rule_expression loc =
  eprintf "%aThis type of rule expression is illegal or not supported.\n"
    output_location loc;
  raise Error

let illegal_binding loc =
  eprintf "%aIllegal binding.\n" output_location loc;
  raise Error

let illegal_actor_parameter a v =
  eprintf "Illegal parameter for actor %s: %s is not valid in this context or not a static constant.\n" a v;
  raise Error

let binding_error loc =
  eprintf "%aThis kind of pattern binding is not accepted.\n"
    output_location loc;
  raise Error

let matching_failure loc =
  eprintf "%aMatch failure\n"
    output_location loc;
  raise Error

let size_mismatch_error sz loc =
  eprintf "%aThis list expression should have size %d\n"
    output_location loc sz;
  raise Error

let array_index_out_of_range loc =
  eprintf "%aArray index out of range\n" output_location loc;
  raise Error

let array_bound_out_of_range loc =
  eprintf "%aArray bound(s) out of range\n" output_location loc;
  raise Error

let illegal_array_extension loc =
  eprintf "%aIllegal extension definition for array\n" output_location loc;
  raise Error

let illegal_array_range loc =
  eprintf "%aIllegal range for this array comprehension (should start at 0)\n" output_location loc;
  raise Error

let partial_array_comprehension loc =
  eprintf "%aWarning: this array comprehension is partial (it should be 0...sz-1)\n" output_location loc

let unbound_output_warning id =
  eprintf "Warning: the output identifier %s is unbound\n" id

let unrecognized_output_fmt id =
  eprintf "Error: unrecognized output format : %s\n" id;
  raise Error

let unrecognized_edge_label_fmt id =
  eprintf "Error: unrecognized edge_label format : %s\n" id;
  raise Error

let warning_undeclared_type tid loc =
  eprintf "%a** Warning: type %s has not been declared. Doing it for you...\n" 
    output_location loc tid

let unsized_array loc =
  eprintf "%a** Error: cannot retrieve size for array\n" 
    output_location loc;
  raise Error

let illegal_array_dimension loc =
  eprintf "%a** Error: illegal dimension for array\n" 
    output_location loc;
  raise Error

let invalid_array_dimension which =
  eprintf "Invalid runtime dimension for array %s\n" which;
  raise Error

let initial_value_mismatch v v' =
  eprintf "Error: initial values %s and %s mismatch\n" v v';
  raise Error

let no_type_converter ty =
  eprintf "Error when executing graph: no converter provided for type %s\n" ty;
  raise Error

let invalid_rt_value v ty =
  eprintf "Error when executing graph: invalid value %s (of declared type %s)\n" v ty;
  raise Error

let wrong_rt_number_of_args f =
  eprintf "Error when executing graph: got wrong number of args for function %s\n" f;
  raise Error

let illegal_fn_type f =
  eprintf "Illegal type for function %s\n" f;
  raise Error

let warning_cycle_with_no_iv c =
  eprintf "Warning: no initial value given for cycle %s\n" c

let channel_overwrite cid =
  eprintf "Overwrite on channel %d.\n" cid;
  raise Error

let cannot_read_from_file fname =
  eprintf "Cannot initialize input stream from file %s.\n" fname;
  raise Error

let err_open_socket (host,port) =
  eprintf "Cannot open socket %s:%d.\n" host port;
  raise Error

let err_reopen_socket (host,port) =
  eprintf "The socket %s:%d is opened several times.\n" host port;
  raise Error

let err_invalid_port_number src =
  eprintf "Invalid port number in socket spec %s.\n" src;
  raise Error

let warn_full_channel c =
  eprintf "Warning: channel W%d is full !\n" c

let warn_stdin () =
  eprintf "Warning: using stdin as input stream; this probably won't work properly ; use -stdin option\n";
  flush stderr

let duplicate_rule_pattern id =
  eprintf "Error: the pattern variable %s is bound several times in the same rule\n" id;
  raise Error

let duplicate_pat_var loc id =
  eprintf "%aError: the variable %s is bound several times in this pattern\n"
    output_location loc id;
  raise Error

let duplicate_pattern id =
  eprintf "Error: the pattern variable %s is used several times with incompatible types in the rule set\n" id;
  raise Error

let cannot_implement_unsized_array () =
  eprintf "Error: cannot generate code for unsized arrays\n";
  raise Error

let warn_sign_mismatch () =
  eprintf "Warning: trying to match a unsigned value against a negative constant\n";
  flush stderr

let illegal_cast loc t t' = 
  eprintf "%aError: cannot cast type %s to type %s\n"
    output_location loc (Pr_type.string_of_one_type t) (Pr_type.string_of_one_type t');
  raise Error

let dubious_cast loc t t' = 
  if !report_cast_warnings then begin
    eprintf "%aWarning: casting type %s to type %s may be unsafe\n"
      output_location loc (Pr_type.string_of_one_type t) (Pr_type.string_of_one_type t');
    flush stderr
    end

let illegal_static_constant loc =
  eprintf "%aError: this expression does not denote a statically bound constant\n"
    output_location loc;
  raise Error

let illegal_static_value what loc =
  eprintf "%aError: this (static) value is illegal in this context"
    output_location loc;
  if what <> "" then eprintf " (it should be an %s)" what;
  eprintf "\n";
  raise Error

let illegal_static_expression loc =
  eprintf "%aError: this expression does not denote a statically bound value\n"
    output_location loc;
  raise Error

let unknown_static_value where loc v =
  eprintf "%aError: the value of variable %s cannot be determined statically is this %s.\n"
    output_location loc v where;
  raise Error

let ill_dimensionned_array loc =
  eprintf "%aError: dimension mismatch for array\n"
    output_location loc;
  raise Error

let empty_array loc =
  eprintf "%aError: this array is empty\n"
    output_location loc;
  raise Error

let too_many_array_initializers loc =
  eprintf "%aError: too many initializers for this array (check option -max_array_size)\n"
    output_location loc;
  raise Error

let warning_bool loc =
  eprintf "%aWarning: the runtime value of this expression is not a true boolean (0/1).\n"
    output_location loc;
  flush stderr

let dynload_error msg =
  eprintf "Dynamic load error: %s\n" msg;
  raise Error

let illegal_extra_file opt exts fname = 
  eprintf "Illegal argument %s for option %s (accepted: %s)\n" fname opt exts;
  raise Error

let unknown_extern_function fname cmos =
  eprintf "The function %s is not registered in the dynamically loaded modules [%s]\n" fname (Misc.string_of_list Misc.id "," cmos);
  raise Error

let cast_error where ty loc =
  eprintf "%aAn error occured when trying to cast a value to %s in this %s.\n" 
    output_location loc ty where;
  raise Error

let warning_ignore_array_init a =
  eprintf "Warning: ignoring initialization for array %s.\n" a;
  flush stderr

let invalid_lib_name name =
  eprintf "%s is not a valid library name.\n" name;
  raise Error

let invalid_pgm_header fname =
  eprintf "invalid PGM header in file %s.\n" fname;
  raise Error

let invalid_pgm_pixel fname =
  eprintf "invalid PGM pixel in file %s.\n" fname;
  raise Error

let pgm_write_error () =
  eprintf "error while writing PGM file : bad format.\n";
  raise Error

let invalid_annot_file fname line =
  eprintf "error reading annotation file %s (line %d).\n" fname line;
  raise Error

let invalid_input_data_file fname =
  eprintf "cannot handle input data file %s (don't know how to convert it to .txt).\n" fname;
  raise Error

let invalid_output_data_file fname =
  eprintf "cannot handle output data file %s (don't know how to convert from .txt).\n" fname;
  raise Error

let cannot_open_file fname =
  eprintf "cannot open file %s.\n" fname;
  raise Error

let fifo_size_must_be_pow2 fid sz =
  eprintf "illegal size (%d) for fifo %s: must be a power of 2.\n" sz fid;
  raise Error

let warning_truncated_cast loc v ty =
  eprintf "%aWarning: Some information was lost during this type coercion: %s cannot be represented as a %s value.\n" 
    output_location loc v ty;
  flush stderr

let illegal_function_type where fid t = 
  eprintf "[** %s]: %s: the type of this function (%s) is illegal or not supported (try to refine it)\n"
    where fid (Pr_type.string_of_one_type t);
  raise Error

let non_top_ignore_expr where = 
  eprintf "%s: the \"_\" pattern can only appear at top level in expressions.\n" where;
  raise Error

let unknown_annotation f id a =
  eprintf "Warning: file %s: ignoring unknow annotation %s for %s\n" f a id

let illegal_var_init_val v vv =
  eprintf "This kind of value is illegal or not supported for initializing variable %s: %s.\n" v vv;
  raise Error

let non_constant_constr_err cstr loc =
  eprintf "%aThe constructor %s requires an argument.\n"
    output_location loc
    (Pr_type.string_of_constr cstr);
  raise Error

let constant_constr_err cstr loc =
  eprintf "%aThe constant constructor %s cannot accept an argument.\n"
    output_location loc
    (Pr_type.string_of_constr cstr);
  raise Error

let unsized_vhdl_int_const loc  =
  eprintf "%aAn integer constant cannot be assigned a size (in bits) and/or a sign-ness at the VHDL level.\n\
           This may prevent subsequent code analysis or synthesis.\n\
           Please declare them with a size (like const c = (1 : signed<8>), or use the -vhdl_default_int_size option\n"
  output_location loc

let ambiguous_fn_type id =
  eprintf "The functionr %s has an incomplete or ambiguous type (maybe insert a refine its type signature ?).\n"
    id;
  raise Error

let cannot_retrieve_int_size where box slot =
  eprintf "[%s] Cannot retrieve actual int size for slot %s of box %s.\n" where box slot;
  raise Error

let ios_in_prelude_warning fname =
  eprintf "Warning: the prelude file %s contains some IO stream definition(s) : this may cause problems.\n" 
    fname;
  flush stderr

let polymorphic_box name loc =
  eprintf "%aThe box resulting from instantiating actor [%s] here is not monomorphic (some type variables remain free).\n"
    output_location loc
    name;
  raise Error

let unsized_int_warning what revert =
  eprintf "** Warning: cannot determine size of %s quantity: reverting to [%s]\n" what revert

let unqual_int_warning revert =
  eprintf "** Warning: cannot determine sign-ness and size of integer constant: reverting to [%s] \
           (this may cause problems latter..)\n" revert

let cannot_build_parser_for_type ty =
  eprintf "** Cannot build stream parser for type %s.\n" (Pr_type.string_of_one_type ty);
  raise Error

let stream_input_error what =
  eprintf "** Error reading input stream (%s)\n" what;
 raise Error

let file_syntax_error fname =
  eprintf "** Syntax error reading file %s\n" fname;
 raise Error

let duplicate_port_ival pname =
  eprintf "Warning: two initial values specified for input port %s.\n" pname;
  flush stderr

let invalid_ctor_arity where cname =
  eprintf "The value constructor %s has arity > 1, which is not supported %s\n" cname where;
 raise Error

let cannot_specialize ?loc what name =
  begin match loc with
    None -> eprintf "Cannot specialize %s %s (some type variables remain unknown)\n" what name
  | Some loc -> eprintf "%aCannot specialize this %s (some type variable(s) remain unknown)\n" output_location loc what end;
  raise Error

let unsized_vhdl_int ()  =
  eprintf "Some int value(s) (probably constant(s)) cannot be assigned a size (in bits) at the VHDL level\n\
           Please declare them with a size (like const c = (1 : signed<8>), or use the -vhdl_default_int_size option\n";
  raise Error

let ambiguous_fn_type id =
  eprintf "The functionr %s has an incomplete or ambiguous type (maybe insert a refine its type signature ?).\n"
    id;
  raise Error

let cannot_retrieve_int_size where box slot =
  eprintf "[%s] Cannot retrieve actual int size for slot %s of box %s.\n" where box slot;
  raise Error

let ios_in_prelude_warning fname =
  eprintf "Warning: the prelude file %s contains some IO stream definition(s) : this may cause problems.\n" 
    fname;
  flush stderr

let polymorphic_box name loc =
  eprintf "%aThe box resulting from instantiating actor [%s] here is not monomorphic (some type variables remain free).\n"
    output_location loc
    name;
  raise Error

let unqual_int_warning revert =
  eprintf "** Warning: cannot determine sign-ness and size of integer constant: reverting to [%s]\
           (this may cause problems latter..)\n" revert

let cannot_build_parser_for_type ty =
  eprintf "** Cannot build stream parser for type %s.\n" (Pr_type.string_of_one_type ty);
  raise Error

let stream_input_error what =
  eprintf "** Error reading input stream (%s)\n" what;
 raise Error

let invalid_ctor_arity where cname =
  eprintf "The value constructor %s has arity > 1, which is not supported %s\n" cname where;
 raise Error

let illegal_range_expression loc = 
  eprintf "%aThis kind of expression cannot be used as range in an array comprehension.\n"
      output_location loc;
  raise Error

let invalid_index_expression loc = 
  eprintf "%aThis kind of index expression is illegal or not supported in this context.\n"
      output_location loc;
  raise Error

let invalid_foldl_argument loc = 
  eprintf "%aIllegal argument for [foldl] (must be a bundle of size 2 at least).\n"
      output_location loc;
  raise Error

let invalid_foldt_argument loc =
  eprintf "%aWrong number of arguments for [foldt] : must be a power of 2.\n" output_location loc;
  raise Error

let invalid_pipe_argument loc =
  eprintf "%aIllegal second argument for [pipe] (must be a positive integer constant).\n" output_location loc;
  raise Error

let global_redefinition what what' item =
  begin
    if what = what'
    then eprintf "The %s %s is multiply defined.\n" what item
    else eprintf "The symbol [%s], previously defined as [%s], is redefined as [%s] .\n" item what what'
  end;
  raise Error
 let actor_redefinition a what1 what2 item =
  eprintf "** In actor [%s], %s [%s] is redefined as %s.\n" a what2 item what1;
  raise Error

let cannot_specialize where what =
  if where = Location.no_location
  then eprintf "** Type specialization error for %s.\n" what
  else eprintf "%aType specialization error for this %s.\n" output_location where what;
  raise Error

let array_index_size_mismatch loc sz1 sz2 = 
  eprintf "%aWarning: the size (in bits) of this index expression (%d) is too small wrt the range of the indexed array (%d).\n"
    output_location loc
    sz1
    sz2

let port_short_circuit wid =
  eprintf "** Wire %d directly connects an input port to an output port, which is forbiden.\n" wid;
  raise Error

let unsafe_run fname =
  eprintf "** Reading/writing from/to file %s is forbidden is safe mode.\n" fname;
  raise Error

let illegal_port_init_expr loc =
  eprintf "%aIllegal init value for port.\n" output_location loc;
  raise Error

let type_arity_err td targs sargs loc =
  eprintf "%aThe type constructor %a expects %d type argument(s) and %d size argument(s)s,\n\
           but is here given %d type argument(s) and %d size argument(s).\n"
    output_location loc
    output_string td.Types.ty_constr.Types.tc_name
    (fst td.Types.ty_arity)
    (snd td.Types.ty_arity)
    (List.length targs)
    (List.length sargs);
  raise Error

let unwired_box where name sel =
  eprintf "Unconnected %s slot (#%d) for box %s.\n" where sel name;
  raise Error

let invalid_pragma name what loc =
  eprintf "%aInvalid %s pragma declaration: %s.\n" output_location loc name what;
  raise Error

let invalid_size_attribute loc =
  eprintf "%aCannot compute size attribute.\n" output_location loc;
  raise Error

let warning_dynamic_int_range where v =
  eprintf "** Warning: cannot compute static range for variable %s in %s.\n" v where

let state_out_of_range where sn sv =
  eprintf "** Out of range Value (%s) for state %s in %s.\n" sv sn where;
  raise Error

let invalid_box_id id =
  eprintf "** Invalid box id: %d.\n" id;
  raise Error

let cannot_build_sdf_graph bid name =
  eprintf "** Cannot build SDF graph: box %d (%s) has not been attributed an SDF MoC.\n" bid name;
  raise Error
