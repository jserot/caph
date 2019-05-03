/************************************************************************************/
/*                                                                                  */
/*                                     CAPH                                         */
/*                            http://caph.univ-bpclermont.fr                        */
/*                                                                                  */
/*                                  Jocelyn SEROT                                   */
/*                         Jocelyn.Serot@univ-bpclermont.fr                         */
/*                                                                                  */
/*         Copyright 2011-2019 Jocelyn SEROT.  All rights reserved.                 */
/*  This file is distributed under the terms of the Q Public License version 1.0.   */
/*                                                                                  */
/************************************************************************************/

/* The parser definition */

%{
open Printf
open Syntax
open Par_aux
open Misc
%}

/* Tokens */

/* Identifiers, prefixes, infixes */
%token <string> LIDENT
%token <string> UIDENT
%token <string> PREFIX
%token <string> INFIX
%token <string> INFIX0
%token <string> INFIX1
%token <string> INFIX2
%token <string> SUBTRACTIVE
%token <string> INFIX3
/* Literals */
%token <int> INT
%token <int> UINT
%token <int> SINT
%token <float> FLOAT
%token <string> STRING
/* The end-of-file marker */
%token EOF
/* Special symbols */
%token EQUAL          /* "=" */
%token EQUALEQUAL     /* "==" */
%token GREATER        /* ">" */
%token LESS           /* "<" */
%token GREATEREQUAL   /* ">=" */
%token LESSEQUAL      /* "<=" */
%token AMPERAMPER     /* "&&" */
%token BARBAR         /* "||" */
%token LPAREN         /* "(" */
%token RPAREN         /* ")" */
%token LBRACE         /* "{" */
%token RBRACE         /* "}" */
%token SOS            /* "'<" */
%token EOS            /* "'>" */
%token STAR           /* "*" */
%token COMMA          /* "," */
%token ARROW          /* "->" */
%token LARROW         /* "<-" */
%token COLON          /* ":" */
%token PERCENT        /* "%" */
/* %token COLONCOLON     /* "::" */
%token UNDERSCORE     /* "_" */
%token LBRACKET       /* "[" */
%token RBRACKET       /* "]" */
%token BAR            /* "|" */
%token QUOTE          /* "'" */
%token BACKQUOTE      /* "`" */
%token DOLLAR         /* "$" */
%token SHARP          /* "#" */
%token DOTDOT         /* ".."  */
/* %token AS             /* "@"  */
%token SEMIC          /* ";"  */
/* Keywords */
%token TYPE       /* "type" */
%token OF         /* "of" */
%token NET        /* "net/ndef" */
%token LET        /* "let" */
%token IN         /* "in" */
%token IF         /* "if" */
%token THEN       /* "then" */
%token ELSE       /* "else" */
%token WHEN       /* "when" */
%token AND       /* "and" */
%token TRUE       /* "true" */
%token FALSE      /* "false" */
%token OUT        /* "out" */
%token ACTOR      /* "actor" */
%token VAR        /* "var" */
%token RULES      /* "rules" */
%token CONST      /* "const" */
%token INRAM      /* "in_ram" */
%token VARIABLE   /* "variable" */
%token STREAM     /* "stream" */
%token TO         /* "to" */
%token FROM       /* "from" */
%token REC            /* "rec" */
%token FUNCTION       /* "function" */
%token EXTERN         /* "extern" */
%token VHDL           /* "vhdl" */
%token SYSTEMC        /* "systemc" */
%token TY_INT      /* "int" */
%token TY_SIGNED      /* "signed" */
%token TY_UNSIGNED    /* "unsigned" */
%token TY_ARRAY       /* "array" */
%token PRAGMA         /* "#pragma" */
%token PORT           /* "port" */
%token INIT           /* "init" */ 

/* Precedences and associativities. Lower precedences first. */

%right prec_let
%right prec_define
/* %nonassoc star_alone */
%nonassoc COLON
%right ARROW 
%right prec_list
%right prec_if
%left  BAR
%left  COMMA
%left  BARBAR
%left  AMPERAMPER
%left  INFIX0 EQUAL GREATER LESS GREATEREQUAL LESSEQUAL /* comparisons */
%left  INFIX2 SUBTRACTIVE               /* additives, subtractives */
%left  STAR INFIX3                      /* multiplicatives */
%nonassoc prec_uminus              /* unary - */
%left  INFIX
%right prec_app
%nonassoc prec_simple
%right PREFIX                           /* prefix operators, e.g. ! */

/* Entry points */

%start program
%type <Syntax.program> program

%%

/* ========================= PROGRAMS ========================== */

program:
    decls { mk_program(List.rev $1) }
;

decls:
      | /* nothing */
          { [] }
      | decls decl
          { $2 :: $1 }
;

decl:
    type_decl SEMIC { $1 }
  | val_decl SEMIC { $1 }
  | io_decl SEMIC { $1 }
  | actor_decl SEMIC { $1 }
  | net_decl SEMIC { $1 }
  | pragma { $1 }
;

/* =================== PRAGMA ========================== */

pragma:
  | PRAGMA LIDENT opt_pragma_params
     { mk_pragma_decl ($2,$3) } 
;

opt_pragma_params:
      /* Nothing */
    { [] }
  | pragma_param
      { [$1] }
  | LPAREN pragma_param_list RPAREN
      { $2 }
;

pragma_param_list:
      pragma_param COMMA pragma_param_list
      { $1 :: $3 }
  | pragma_param
      { [$1] }
;

pragma_param:
  | LIDENT 
      { $1 }
  | INT
      { string_of_int $1}
;
      
/* =================== TYPE DECLARATION ========================== */

type_decl:
  | TYPE LIDENT EQUALEQUAL type_expr           /* Only non-parameterized type abbrevs are supported */
       { mk_ty_decl(Abbrev_type_decl($2, $4)) }
  | TYPE LIDENT opt_size_params EQUAL variant_type_defn
       { mk_ty_decl(Variant_type_decl($2, [], $3, $5)) }
  | TYPE type_params LIDENT opt_size_params EQUAL variant_type_defn
       { mk_ty_decl(Variant_type_decl($3, $2, $4, $6)) }
;

variant_type_defn:
      | constr_decl_bar_list
          { $1 }
;

constr_decl_bar_list:
        constr_decl BAR constr_decl_bar_list
          { $1 :: $3 }
      | constr_decl
          { [$1] }
;

constr_decl:
        UIDENT opt_tag OF type_expr
          { Constr1_decl($1, $4, $2) }
      | UIDENT opt_tag
          { Constr0_decl ($1, $2) }
;

opt_tag:
       /* Nothing */
          { None }
      | PERCENT INT 
          { Some $2 }
;

type_params:
        LPAREN type_param_list RPAREN
          { $2 }
      | type_param
          { [$1] }
;

type_param_list:
        type_param COMMA type_param_list
          { $1 :: $3 }
      | type_param
          { [$1] }
;

type_param:
        DOLLAR LIDENT
          { $2 }
;

opt_size_params:
        /* Nothing */
          { [] }
      | LESS size_param_list GREATER
          { $2 }
;

size_param_list:
        size_param COMMA size_param_list
          { $1 :: $3 }
      | size_param
          { [$1] }
;

size_param:
      | LIDENT
          { $1 }
;

/* =================== GLOBAL VALUE DECLARATION ========================== */

val_decl:
      | CONST LIDENT EQUAL val_defn
          { mk_val_decl(VConst($2, $4, None)) }
      | FUNCTION LIDENT fun_pat EQUAL expr opt_typ_sig
          { mk_val_decl(VFun($2,$3,$5,$6)) }
      | FUNCTION LIDENT EQUAL EXTERN STRING COMMA STRING COMMA STRING COLON type_expr
          { mk_val_decl(VExtern($2, $11, $5, $7, $9)) }
;

val_defn:
        val_init_value
          { $1 }
      | val_init_value COLON type_expr
          { mk_expr(ECast($1,$3)) }
;

val_init_value:
        expr
          { $1 }
      | array_init_expr
          { $1 }
;

opt_typ_sig:
       /* Nothing */
          { None }
      | COLON type_expr 
          { Some $2 }
;

fun_pat:
          LIDENT 
          { [$1] }
      | LPAREN ident_comma_list RPAREN
          { $2 }
;

ident_comma_list:
        LIDENT COMMA ident_comma_list
          { $1 :: $3 }
      | LIDENT
          { [$1] }
;

/* =================== ACTOR DECLARATION ========================== */

actor_decl:
        ACTOR LIDENT actor_opt_params IN LPAREN actor_io RPAREN OUT LPAREN actor_io RPAREN
        actor_vars actor_rules 
          { mk_actor {a_id=$2; a_params=$3; a_ins=$6; a_outs=$10; a_vars=$12; a_rsch=fst($13); a_rules=snd($13); a_impl=no_impl} }
;

/* -- ACTOR PARAMETERS */

actor_opt_params:
       /* Nothing */
          { [] }
      | LPAREN actor_params RPAREN 
          { $2 }
;

actor_params:
       /* Nothing */
          { [] }
      | actor_param COMMA actor_params
          { $1 :: $3 }
      | actor_param
          { [$1] }
;

actor_param:
        LIDENT COLON type_expr
          { mk_param($1, $3) }
;

/* -- ACTOR IOs */

actor_io:
        act_io_comma_list
          { $1 }

act_io_comma_list:
        act_io COMMA act_io_comma_list
          { $1 :: $3 }
      | act_io
          { [$1] }
;

act_io:
        LIDENT COLON type_expr
          { mk_act_io($1, $3) }
;

/* -- ACTOR BODY */

actor_vars:
      | /* nothing */
          { [] }
      | actor_vars var_decl
          { $2 :: $1 }
;

var_decl:
        VAR LIDENT COLON var_type_expr opt_var_init
          { mk_var($2, $4, $5) }
;

opt_var_init:
          /* Nothing */
          { None }
      | EQUAL var_init_value
          { Some $2 }
;

var_init_value:
        expr
          { $1 }
      | UIDENT 
          { mk_expr(ECon($1,[])) }
      | UIDENT simple_rule_expr
          { mk_expr(ECon($1,[$2])) }
      | UIDENT LPAREN simple_rule_expr_comma_list RPAREN
          { mk_expr(ECon($1,$3)) }
      | array_init_expr
          { $1 }
;

/* -- -- RULES SECTION */

actor_rules: 
        RULES rule_schema unqual_rule_decls
           { $2, $3 }
      | RULES qual_rule_decls
           { empty_rule_schema, $2 }
;

/* -- -- -- RULE SCHEMA */

rule_schema: 
        qualifiers ARROW qualifiers
           { mk_rule_schema($1,$3) }
;

qualifiers:
   |  qualifier
          { [$1] }
   |  LPAREN qualifier_comma_list RPAREN
          { $2 }
;

qualifier_comma_list:
     qualifier COMMA qualifier_comma_list
          { $1 :: $3 }
   | qualifier
	  { [$1] }
;

qualifier:
     LIDENT
       { mk_qualifier (QIdent ($1,[])) }
   | LIDENT LBRACKET array_index RBRACKET 
       { mk_qualifier (QIdent ($1,[$3])) }
   | LIDENT LBRACKET array_index RBRACKET LBRACKET array_index RBRACKET 
       { mk_qualifier (QIdent ($1,[$3;$6])) }
   | LIDENT LBRACKET array_index RBRACKET LBRACKET array_index RBRACKET LBRACKET array_index RBRACKET 
       { mk_qualifier (QIdent ($1,[$3;$6;$9])) }

/* -- -- -- RULES */

unqual_rule_decls:
        unqual_rule_decl unqual_rule_decls
          { $1 :: $2 }
      | unqual_rule_decl
	  { [$1] }
;
          
unqual_rule_decl:
     BAR unqual_rule_lhs rule_guards ARROW unqual_rule_rhs
	   { mk_rule($2, $3, $5) }
;

qual_rule_decls:
        qual_rule_decl qual_rule_decls
          { $1 :: $2 }
      | qual_rule_decl
	  { [$1] }
;
          
qual_rule_decl:
     BAR qual_rule_lhs rule_guards ARROW qual_rule_rhs
	   { mk_rule($2, $3, $5) }
;

/* -- -- -- -- UNQUALIFIED RULE LHS */

unqual_rule_lhs:
     rule_pattern
       { mk_rule_lhs([mk_qualifier QNone, $1]) }     
   | LPAREN rule_pattern_comma_list RPAREN
       { mk_rule_lhs (List.map (function p -> (mk_qualifier QNone, p)) $2) }     
;
    
rule_pattern_comma_list:
        rule_pattern COMMA rule_pattern_comma_list
          { $1 :: $3 }
      | rule_pattern
	  { [$1] }
;

/* -- -- -- -- QUALFIED RULE LHS */

qual_rule_lhs:
     qual_rule_pattern
       { mk_rule_lhs([$1]) }     
   | LPAREN qual_rule_pattern_comma_list RPAREN
       { mk_rule_lhs($2) }     
;
    
qual_rule_pattern_comma_list:
        qual_rule_pattern COMMA qual_rule_pattern_comma_list
          { $1 :: $3 }
      | qual_rule_pattern
	  { [$1] }
;

qual_rule_pattern:
       qualifier COLON rule_pattern
         { ($1, $3) }
;

/* -- -- -- -- RULE PATTERN */

rule_pattern:
      | LIDENT
	      { mk_rpat(RPatVar($1)) }
      | INT
          { mk_rpat(RPatConst(Const.CInt ($1,None,None))) }
      | UINT
          { mk_rpat(RPatConst(Const.CInt ($1,Some Const.Unsigned,None))) }
      | SINT
          { mk_rpat(RPatConst(Const.CInt ($1,Some Const.Signed,None))) }
      | SUBTRACTIVE INT
          { mk_rpat(RPatConst(Const.CInt(-($2),Some Const.Signed,None))) }
      | TRUE
          { mk_rpat(RPatConst(Const.CBool true)) }
      | FALSE
          { mk_rpat(RPatConst(Const.CBool false)) }
      | UIDENT
          { mk_rpat(RPatCon($1,[])) }
      | UIDENT simple_rule_pattern
          { mk_rpat(RPatCon($1,[$2])) }
      | UIDENT LPAREN simple_rule_pattern_comma_list RPAREN
          { mk_rpat(RPatCon($1,$3)) }
      | SOS
	      { mk_rpat(RPatCon("SoS",[])) }
      | EOS
	      { mk_rpat(RPatCon("EoS",[])) }
      | QUOTE simple_rule_pattern
          { mk_rpat(RPatCon("Data",[$2])) }
      | UNDERSCORE
          { mk_rpat(RPatWild) }

simple_rule_pattern:
      | LIDENT
	      { mk_rpat(RPatVar($1)) }
      | INT
          { mk_rpat(RPatConst(Const.CInt ($1,None,None))) }
      | UINT
          { mk_rpat(RPatConst(Const.CInt ($1,Some Const.Unsigned,None))) }
      | SINT
          { mk_rpat(RPatConst(Const.CInt ($1,Some Const.Signed,None))) }
      | SUBTRACTIVE INT
          { mk_rpat(RPatConst(Const.CInt(-($2),Some Const.Signed,None))) }
      | TRUE
          { mk_rpat(RPatConst(Const.CBool true)) }
      | FALSE
          { mk_rpat(RPatConst(Const.CBool false)) }
      | UNDERSCORE
          { mk_rpat(RPatWild) }
;

simple_rule_pattern_comma_list:
        simple_rule_pattern COMMA simple_rule_pattern_comma_list
          { $1 :: $3 }
      | simple_rule_pattern
	  { [$1] }
;

/* -- -- -- -- RULE GUARDS */

rule_guards:
        /* Nothing */
          { mk_rule_grd [] }
      | WHEN rule_guard_and_list
          { mk_rule_grd $2 }     
;
    
rule_guard_and_list:
        rule_guard AND rule_guard_and_list
          { $1 :: $3 }
      | rule_guard
	      { [$1] }
;

rule_guard:
        simple_rule_expr
          { $1 }
;

/* -- -- -- -- UNQUALIFIED RULE RHS */

unqual_rule_rhs:
     rule_expr
       { mk_rule_rhs [mk_qualifier QNone, $1] }     
   | LPAREN rule_expr_comma_list RPAREN
       { mk_rule_rhs (List.map (function e -> (mk_qualifier QNone, e)) $2) }     
;

rule_expr_comma_list:
     rule_expr COMMA rule_expr_comma_list
       { $1 :: $3 }
   | rule_expr
	   { [$1] }
;

/* -- -- -- -- QUALIFIED RULE RHS */

qual_rule_rhs:
     qual_rule_expr
       { mk_rule_rhs [$1] }     
   | LPAREN qual_rule_expr_comma_list RPAREN
       { mk_rule_rhs ($2) }     
;

qual_rule_expr_comma_list:
     qual_rule_expr COMMA qual_rule_expr_comma_list
       { $1 :: $3 }
   | qual_rule_expr
	   { [$1] }
;

qual_rule_expr:
     qualifier COLON rule_expr
       { ($1, $3) }
;

/* -- -- -- -- RULE EXPRESSION */

rule_expr:
   | simple_rule_expr
       { $1 }
   | UIDENT 
       { mk_expr(ECon($1,[])) }
   | UIDENT simple_rule_expr
       { mk_expr(ECon($1,[$2])) }
   | UIDENT LPAREN simple_rule_expr_comma_list RPAREN
       { mk_expr(ECon($1,$3)) }
   | SOS
       { mk_expr(ECon("SoS",[])) }
   | EOS
       { mk_expr(ECon("EoS",[])) }
   | QUOTE simple_rule_expr
       { mk_expr(ECon("Data",[$2])) }
   | UNDERSCORE
       { mk_expr(EIgnored) }
;

simple_rule_expr:
   | expr
       { $1 }
;

simple_rule_expr_comma_list:
     simple_rule_expr COMMA simple_rule_expr_comma_list
       { $1 :: $3 }
   | simple_rule_expr
	   { [$1] }
;

/* =================== IO DECLARATION ========================== */

io_decl:
      | STREAM LIDENT COLON type_expr io_dir STRING
          { mk_io(StreamIO, $2, $4, $5, $6, None) }
      | PORT LIDENT COLON type_expr opt_inp_dev INIT simple_net_constant
          { mk_io(PortIO, $2, $4, IoIn, $5, Some (mk_expr (EConst $7))) }
      | PORT LIDENT COLON type_expr TO STRING
          { mk_io(PortIO, $2, $4, IoOut, $6, None) }
;

io_dir:
        FROM
          { IoIn }
      | TO
          { IoOut }
;

opt_inp_dev:
      | /* nothing */
          { "" }
      | FROM STRING 
          { $2 }
;

/* =================== NETWORK LEVEL DECLARATIONS ========================== */

net_decl:
        NET net_bindings
          { mk_net_decl(false,$2) }
       | NET REC net_bindings 
           { mk_net_decl(true,$3) } 
;

net_bindings:
        net_binding AND net_bindings
          { $1 :: $3 }
      | net_binding
          { [$1] }
;

net_binding:
      net_pattern EQUAL net_expr  %prec prec_define
          { mk_net_binding($1, $3) }
    | LIDENT net_pattern_list EQUAL net_expr  %prec prec_define
          { mk_net_binding(mk_net_pat (NPat_var $1), mk_nfun $4 $2) }
;

/* -- NETWORK LEVEL PATTERNS */

net_pattern_list:
        net_pattern net_pattern_list
          { $1 :: $2 }
      | net_pattern
          { [$1] }
;

net_pattern:
      | LIDENT
          { mk_net_pat(NPat_var $1) }
      | LPAREN net_pattern_comma_list RPAREN
          { match $2 with [x] -> x | xs -> mk_net_pat(NPat_tuple(xs)) }
;

net_pattern_comma_list:
   | net_pattern COMMA net_pattern_comma_list
       { $1 :: $3 }
   | net_pattern
       { [$1] }
;

/* -- NETWORK LEVEL EXPRESSIONS */

net_expr:
        simple_net_expr
          { $1 }
      | simple_net_expr simple_net_expr_list   %prec prec_app
          { mk_napply($1, $2) }
      | net_expr_comma_list
          { mk_net_expr(NTuple(List.rev $1)) }
      | FUNCTION net_pattern ARROW net_expr
          { mk_net_expr(NFun($2,$4)) }
      | LET net_bindings IN net_expr  %prec prec_let 
          { mk_net_expr(NLet(false, $2, $4)) } 
      | LET REC net_bindings IN net_expr  %prec prec_let 
          { mk_net_expr(NLet(true, $3, $5)) } 
;

simple_net_constant:
      | scalar_constant
          { $1 }
      | SUBTRACTIVE scalar_constant %prec prec_uminus
          { negate_constant $1 $2 }
;

simple_net_expr:
      | LIDENT
          { mk_net_expr (NVar $1) }
      | LPAREN RPAREN 
          { mk_net_expr (NUnit) }
      | simple_net_constant
          { mk_net_expr(NConst $1) }
      | net_array1_constant 
          { mk_net_expr(NArray1Const($1)) }
      | net_array2_constant 
          { mk_net_expr(NArray2Const($1)) }
      | net_array3_constant 
          { mk_net_expr(NArray3Const($1)) }
      | LPAREN LIDENT LBRACKET net_array_index RBRACKET RPAREN
          { mk_net_expr(NArrayItem($2,[$4])) }
      | LPAREN LIDENT LBRACKET net_array_index RBRACKET LBRACKET net_array_index RBRACKET RPAREN
          { mk_net_expr(NArrayItem($2,[$4;$7])) }
      | LPAREN LIDENT LBRACKET net_array_index RBRACKET LBRACKET net_array_index RBRACKET LBRACKET net_array_index RBRACKET RPAREN
          { mk_net_expr(NArrayItem($2,[$4;$7;$10])) }
      | LPAREN simple_net_expr COLON type_expr RPAREN
          { mk_net_expr(NCast($2,$4)) }
      | LPAREN net_expr RPAREN 
          { $2 }
;

simple_net_expr_list:
        simple_net_expr simple_net_expr_list
          { $1 :: $2 }
      | simple_net_expr
          { [$1] }
;

net_expr_comma_list:
        net_expr_comma_list COMMA net_expr
          { $3 :: $1 }
      | net_expr COMMA net_expr
          { [$3; $1] }
;

net_array1_constant:
        LBRACKET simple_net_constant_comma_list RBRACKET
          { $2 }
;

net_array2_constant:
        LBRACKET net_array1_constant_comma_list RBRACKET
          { $2 }
;

net_array3_constant:
        LBRACKET net_array2_constant_comma_list RBRACKET
          { $2 }
;

net_array1_constant_comma_list:
     net_array1_constant COMMA net_array1_constant_comma_list
       { $1 :: $3 }
   | net_array1_constant
	   { [$1] }
;

net_array2_constant_comma_list:
     net_array2_constant COMMA net_array2_constant_comma_list
       { $1 :: $3 }
   | net_array2_constant
	   { [$1] }
;

simple_net_constant_comma_list:
     simple_net_constant COMMA simple_net_constant_comma_list
       { $1 :: $3 }
   | simple_net_constant
	   { [$1] }
;

/* =================== CORE EXPRESSIONS ========================== */

expr:
      | scalar_constant
          { mk_expr(EConst($1)) }
      | LIDENT
          { mk_expr(EVar($1)) }
      | LIDENT BACKQUOTE LIDENT
          { mk_expr(EAttr($1,$3)) }
      | expr INFIX3 expr
          { mk_binop $2 $1 $3 }
      | expr STAR expr
          { mk_binop "*" $1 $3 }
      | expr INFIX2 expr
          { mk_binop $2 $1 $3 }
      | expr SUBTRACTIVE expr
          { mk_binop $2 $1 $3 }
      | expr INFIX1 expr
          { mk_binop $2 $1 $3 }
      | expr INFIX0 expr
          { mk_binop $2 $1 $3 }
      | expr INFIX expr
          { mk_binop $2 $1 $3 }
      | expr EQUAL expr
          { mk_binop "=" $1 $3 }
      | expr GREATER expr
          { mk_binop ">" $1 $3 }
      | expr LESS expr
          { mk_binop "<" $1 $3 }
      | expr GREATEREQUAL expr
          { mk_binop ">=" $1 $3 }
      | expr LESSEQUAL expr
          { mk_binop "<=" $1 $3 }
      | expr AMPERAMPER expr
          { mk_binop "&&" $1 $3 }
      | expr BARBAR expr
          { mk_binop "||" $1 $3 }
      | PREFIX expr
          { mk_unop $1 $2 }
      | SUBTRACTIVE expr  %prec prec_uminus
          { negate_expr $1 $2 }
      | LIDENT LPAREN expr_comma_list RPAREN
          { mk_expr(EApp(mk_expr(EVar $1), None, $3)) }
      | LET LIDENT EQUAL expr IN rule_expr  %prec prec_let
          { mk_expr(ELet([$2,$4], $6)) }
      | IF expr THEN rule_expr ELSE rule_expr  %prec prec_if
          { mk_expr(ECond($2, $4, $6)) }
      | LIDENT LBRACKET array_index RBRACKET
          { mk_expr(EArrRead($1,[$3])) }
      | LIDENT LBRACKET array_index RBRACKET LBRACKET array_index RBRACKET
          { mk_expr(EArrRead($1,[$3;$6])) }
      | LIDENT LBRACKET array_index RBRACKET LBRACKET array_index RBRACKET LBRACKET array_index RBRACKET
          { mk_expr(EArrRead($1,[$3;$6;$9])) }
      | LPAREN expr COLON type_expr RPAREN
          { mk_expr(ECast($2,$4)) }
      | LPAREN expr RPAREN
          { $2 }
;

expr_comma_list:
     expr COMMA expr_comma_list
       { $1 :: $3 }
   | expr
	   { [$1] }
;

array_init_expr:
        array_extension_1
          { mk_expr(EArrayExt1 $1) }
      | array_extension_2
          { mk_expr(EArrayExt2 $1) }
      | array_extension_3
          { mk_expr(EArrayExt3 $1) }
      | LBRACKET array_comprehension RBRACKET
        { mk_expr(EArrayCompr $2) }
;

array_extension_1:
        LBRACKET expr_comma_list RBRACKET
          { $2 }
;

array_extension_2:
        LBRACKET array_ext1_comma_list RBRACKET
          { $2 }
;

array_ext1_comma_list:
     array_extension_1 COMMA array_ext1_comma_list
       { $1 :: $3 }
   | array_extension_1
	   { [$1] }
;

array_extension_3:
        LBRACKET array_ext2_comma_list RBRACKET
          { $2 }
;

array_ext2_comma_list:
     array_extension_2 COMMA array_ext2_comma_list
       { $1 :: $3 }
   | array_extension_2
	   { [$1] }
;

array_comprehension:
      | expr BAR index_range_comma_list 
          { ($3,$1) }
;


index_range_comma_list:
     index_range COMMA index_range_comma_list
       { $1 :: $3 }
   | index_range
	   { [$1] }
;

index_range:
     LIDENT EQUAL expr TO expr
       { ($1,$3,$5) }
;

array_index:
     expr 
       { $1 }
;

net_array_index:
      | LIDENT
          { mk_net_expr (NVar $1) }
      | scalar_constant
          { mk_net_expr(NConst($1)) }
;

/* ========================== CONSTANTS =========================== */

scalar_constant:
        INT
          { Const.CInt($1, None,None) }
      | UINT
          { Const.CInt($1, Some Const.Unsigned,None) }
      | SINT
          { Const.CInt($1, Some Const.Signed,None) }
      | FLOAT
          { Const.CFloat($1) }
      | TRUE
          { Const.CBool(true) }
      | FALSE
          { Const.CBool(false) }
;

/* ========================== TYPE EXPRESSIONS =========================== */

type_expr:
        simple_type_expr
          { $1 }
      | type_expr_star_list
          { mk_tyexp(Typetuple (List.rev $1)) }
      | type_expr ARROW type_expr
          { mk_tyexp(Typearrow ($1,$3)) }
;

simple_type_expr:
      | DOLLAR LIDENT
          { mk_tyexp(Typevar($2)) }
      | TY_INT opt_sign_and_size_expr
          { mk_tyexp(Typeconstr("int", fst $2, snd $2)) }
      | TY_UNSIGNED LESS size_expr GREATER 
          { mk_tyexp(Typeconstr("unsigned", [], [$3])) }
      | TY_SIGNED LESS size_expr GREATER 
          { mk_tyexp(Typeconstr("signed", [], [$3])) }
      | LIDENT opt_size_exprs
          { mk_tyexp(Typeconstr($1, [], $2)) }
      | simple_type_expr TY_ARRAY LBRACKET size_expr RBRACKET
          { mk_tyexp(Typeconstr("array", [$1], [$4])) }
      | simple_type_expr TY_ARRAY LBRACKET size_expr RBRACKET LBRACKET size_expr RBRACKET
          /* t array[m][n] is parsed as t array[n] array[m] */
          { let t = mk_tyexp(Typeconstr("array", [$1], [$7])) in
             mk_tyexp(Typeconstr("array", [t], [$4])) }
      | simple_type_expr TY_ARRAY LBRACKET size_expr RBRACKET LBRACKET size_expr RBRACKET LBRACKET size_expr RBRACKET
          /* t array[m][n][p] is parsed as t array[p] array[n] array[m] */
          { let t = mk_tyexp(Typeconstr("array", [$1], [$10])) in
            let t' = mk_tyexp(Typeconstr("array", [t], [$7])) in
             mk_tyexp(Typeconstr("array", [t'], [$4])) }
      | simple_type_expr LIDENT opt_size_exprs
          { mk_tyexp(Typeconstr($2, [$1], $3)) }
      | LPAREN type_expr COMMA type_expr_comma_list RPAREN LIDENT opt_size_exprs
          { mk_tyexp(Typeconstr($6, $2::$4, $7)) }
      | LPAREN type_expr RPAREN
          { $2 }
;

opt_sign_and_size_expr:
        /* Nothing */
          { [], [] }
      | LESS size_expr GREATER
          { [], [$2] }
      | LESS sign_expr COMMA size_expr GREATER
          { [$2], [$4] }
;

opt_size_exprs:
        /* Nothing */
          { [] }
      | LESS size_expr_comma_list GREATER
          { $2 }
;

size_expr_comma_list:
        size_expr COMMA size_expr_comma_list
          { $1 :: $3 }
      | size_expr
          { [$1] }
;

size_expr:
        INT
          { mk_szexp(Sizeconst $1) }
      | LIDENT
          { mk_szexp(Sizevar $1) }
;

sign_expr:
      | LIDENT
          { mk_tyexp(Typevar $1) }
;


type_expr_star_list:
        type_expr_star_list STAR simple_type_expr
          { $3 :: $1 }
      | simple_type_expr STAR simple_type_expr
          { [$3; $1] }
;

type_expr_comma_list:
        type_expr COMMA type_expr_comma_list
          { $1 :: $3 }
      | type_expr
          { [$1] }
;

var_type_expr:
        type_expr 
          { $1 }
      | range_type_expr
          { $1 }
      | enum_type_expr
          { $1 }
;

range_type_expr:
      | LBRACE size_expr COMMA DOTDOT COMMA size_expr RBRACE
          { mk_tyexp(TypeIntRange ($2,$6)) }
;

enum_type_expr:
      | LBRACE enum_ctors RBRACE
          { mk_tyexp(TypeEnum ($2)) }
;

enum_ctors:
        enum_ctor COMMA enum_ctors
          { $1 :: $3 }
      | enum_ctor
          { [$1] }
;

enum_ctor:
        UIDENT
          { $1 }
;
%%
