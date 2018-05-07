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

(* The lexer definition *)

(* Largely inspired from the file lexer.mll found in the Caml Light 0.75 distribution  *)
(* and written by Xavier Leroy, Damien Doligez, Francois Rouaix, Jerome Vouillon and Pierre Weis, *)
(* research workers for the Institut National de Recherche en Informatique et *)
(* en Automatique (INRIA) - Domaine de Voluceau - Rocquencourt - 78153 Le Chesnay - France *)

{
open Parser

type lexical_error =
    Illegal_character
  | Unterminated_string
  | Unterminated_cond
  | Bad_char_constant
  | Undefined_macro
  | Unspecified_macro
  | Nested_ifdef
  | Dangling_cond of string

exception Lexical_error of lexical_error * int * int

(* To buffer string literals *)

let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0;
  ()

let store_string_char c =
  if !string_index >= String.length (!string_buff) then begin
    let new_buff = String.create (String.length (!string_buff) * 2) in
      String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
  end;
  String.set (!string_buff) (!string_index) c;
  incr string_index

let get_stored_string () =
  let s = String.sub (!string_buff) 0 (!string_index) in
    string_buff := initial_string_buffer;
    s

(* To translate escape sequences *)

let char_for_backslash = function
    'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c = 
    100 * (int_of_char(Lexing.lexeme_char lexbuf i) - 48) +
     10 * (int_of_char(Lexing.lexeme_char lexbuf (i+1)) - 48) +
          (int_of_char(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  char_of_int(c land 0xFF)

(* The table of keywords *)

let keyword_table = [
  "type", TYPE;
  "of", OF;
  "net", NET;
  "let", LET;
  "in", IN;
  "rec", REC;
  "if", IF;
  "then", THEN;
  "else", ELSE;
  "actor", ACTOR;
  "out", OUT;
  "var", VAR;
  "rules", RULES;
  "stream", STREAM;
  "to", TO;
  "from", FROM;
  "when", WHEN;
  "and", AND;
  "function", FUNCTION;
  "const", CONST;
  "extern", EXTERN;
  "true", TRUE;
  "false", FALSE;
(*   "and", INFIX3("and"); *)
(*   "or", INFIX3("or"); *)
  "not", PREFIX("not");
  "lnot", PREFIX("lnot");
  "int", TY_INT;
  "signed", TY_SIGNED;
  "unsigned", TY_UNSIGNED;
  "array", TY_ARRAY;
  "mod", INFIX3("mod");
  "land", INFIX3("land");
  "lor", INFIX3("lor");
(*   "lnand", INFIX3("lnand"); *)
(*   "lnor", INFIX3("lnand"); *)
  "lxor", INFIX3("lxor");
(*   "lxnor", INFIX3("lxor"); *)
(*   "sll", INFIXOP4("lsl"); *)
(*   "srl", INFIXOP4("lsr"); *)
(*   "sla", INFIXOP4("lsl"); *)
(*   "sra", INFIXOP4("lsr"); *)
(*   "rol", INFIXOP4("rol"); *)
(*   "ror", INFIXOP4("ror"); *)
  "port", PORT;
  "init", INIT;
]

(* To handle #include directive (added 2014-11-04, JS) *)

let include_path = ref ([] : string list)

type lexing_context = {
    input_name: string;
    input_chan: in_channel;
    input_lexbuf: Lexing.lexbuf
  }

let ctxs = (Stack.create () : lexing_context Stack.t)

let included_files = ref ([] : string list)

let start_incl path =
  let fname = Filename.basename path in
  let fullname =
    try Misc.find_in_path !include_path path
    with Not_found -> 
      let loc = Location.Loc (!Location.input_name, Lexing.lexeme_start !Location.input_lexbuf, Lexing.lexeme_end !Location.input_lexbuf) in
      Printf.eprintf "%aCannot open file %s for inclusion.\n" Location.output_location loc fname;
      raise Misc.Error in
  if List.mem fname !included_files then
      !Location.input_lexbuf
  else begin
      let ic = open_in fullname in
      let lexbuf = Lexing.from_channel ic in
      Stack.push
        { input_name = !Location.input_name; input_chan = !Location.input_chan; input_lexbuf = !Location.input_lexbuf}
        ctxs;
      Location.input_name := fullname;
      Location.input_chan := ic;
      Location.input_lexbuf := lexbuf;
      included_files := fname :: !included_files;
      lexbuf
    end

let stop_incl () =
  close_in !Location.input_chan;
  let c = Stack.pop ctxs in
  Location.input_name := c.input_name;
  Location.input_chan := c.input_chan;
  Location.input_lexbuf := c.input_lexbuf;
  c.input_lexbuf

(* To handle #ifdef directive (added 2015-10-24, JS) *)

type cond_comp_state = CC_Out | CC_InclIf  | CC_ExclIf  | CC_ExclElse  | CC_InclElse 

let cc_state = ref CC_Out

}

rule main = parse
  | "#include" [' ' '\t']+ '\"' ([^'\"']+ as fname) '\"' { let lexbuf' = start_incl fname in main lexbuf' }
  | "#ifdef" [' ' '\t']+ (['A'-'Z' 'a'-'z' '0'-'9' '_']+ as symbol)
      { match !cc_state with 
          CC_Out ->
            if Macro.is_defined symbol then begin
              cc_state := CC_InclIf;
              main !Location.input_lexbuf
              end
            else begin
              cc_state := CC_ExclIf;
              exclude !Location.input_lexbuf;
            end 
        | _ -> (* TO BE FIXED TO SUPPORT NESTED #IF/#ELSE  *)
            raise (Lexical_error(Nested_ifdef,
                   Lexing.lexeme_start !Location.input_lexbuf, Lexing.lexeme_end !Location.input_lexbuf)) }
  | "#else"
      { match !cc_state with 
          CC_InclIf ->
            cc_state := CC_ExclElse;
            exclude !Location.input_lexbuf
        | CC_Out ->
            raise (Lexical_error(Dangling_cond "#else",
                   Lexing.lexeme_start !Location.input_lexbuf, Lexing.lexeme_end !Location.input_lexbuf))
        | _ ->
            Misc.fatal_error "Lexer: #else in invalid context" (* should not happen *) }
  | "#endif"
      { match !cc_state with 
          CC_InclIf | CC_InclElse -> 
            cc_state := CC_Out;
            main !Location.input_lexbuf
        | CC_Out ->
            raise (Lexical_error(Dangling_cond "#endif",
                   Lexing.lexeme_start !Location.input_lexbuf, Lexing.lexeme_end !Location.input_lexbuf))
        | _ ->
            Misc.fatal_error "Lexer: #endif in invalid context" (* should not happen *) }
  | "#pragma" { PRAGMA }
  | [' ' '\t' '\010' '\013' ] +
      { main !Location.input_lexbuf }
  | "%"['a'-'z' ]
    ( ['A'-'Z' 'a'-'z' '0'-'9' '_' ''' ] ) *
      { let s = Lexing.lexeme !Location.input_lexbuf  in
        try
          (match Macro.lookup (String.sub s 1 (String.length s-1)) with
            Some (Macro.M_Int (v,None)) -> INT v
          | Some (Macro.M_Int (v,Some Const.Unsigned)) -> UINT v
          | Some (Macro.M_Int (v,Some Const.Signed)) -> SINT v
          | Some (Macro.M_String s) -> STRING s
          | None -> raise (Lexical_error(Unspecified_macro,
                   Lexing.lexeme_start !Location.input_lexbuf, Lexing.lexeme_end !Location.input_lexbuf)))
        with Macro.Undefined ->
           raise (Lexical_error(Undefined_macro,
                   Lexing.lexeme_start !Location.input_lexbuf, Lexing.lexeme_end !Location.input_lexbuf)) }
  | ['a'-'z' ] ( ['A'-'Z' 'a'-'z' '0'-'9' '_' ] ) *
      { let s = Lexing.lexeme !Location.input_lexbuf  in
        try List.assoc s keyword_table
        with Not_found -> LIDENT s }
  | ['A'-'Z' ] ( ['A'-'Z' 'a'-'z' '0'-'9' '_' ] ) *
      { let s = Lexing.lexeme !Location.input_lexbuf in UIDENT s }
  | "\""
      { reset_string_buffer();
        let string_start = !Location.input_lexbuf.Lexing.lex_start_pos + !Location.input_lexbuf.Lexing.lex_abs_pos in
        begin try
          string !Location.input_lexbuf
        with Lexical_error(Unterminated_string, _, string_end) ->
          raise(Lexical_error(Unterminated_string, string_start, string_end))
        end;
        !Location.input_lexbuf.Lexing.lex_start_pos <- string_start - !Location.input_lexbuf.Lexing.lex_abs_pos;
        STRING (get_stored_string()) }
  | ['0'-'9']+
      { INT (int_of_string(Lexing.lexeme !Location.input_lexbuf)) }
  | "0x"['0'-'9''A'-'F']+
      { INT (int_of_string(Lexing.lexeme !Location.input_lexbuf)) }
  | "0b"['0'-'1']+
      { INT (int_of_string(Lexing.lexeme !Location.input_lexbuf)) }
  | ['0'-'9']+'U'
      { let s = Lexing.lexeme !Location.input_lexbuf in UINT (int_of_string(String.sub s 0 (String.length s - 1))) }
  | "Ox"['0'-'9''A'-'F']+'U'
      { let s = Lexing.lexeme !Location.input_lexbuf in UINT (int_of_string(String.sub s 0 (String.length s - 1))) }
  | "Ob"['0'-'1']+'U'
      { let s = Lexing.lexeme !Location.input_lexbuf in UINT (int_of_string(String.sub s 0 (String.length s - 1))) }
  | ['0'-'9']+'S'
      { let s = Lexing.lexeme !Location.input_lexbuf in SINT (int_of_string(String.sub s 0 (String.length s - 1))) }
  | "Ox"['0'-'9''A'-'F']+'S'
      { let s = Lexing.lexeme !Location.input_lexbuf in SINT (int_of_string(String.sub s 0 (String.length s - 1))) }
  | "Ob"['0'-'1']+'S'
      { let s = Lexing.lexeme !Location.input_lexbuf in SINT (int_of_string(String.sub s 0 (String.length s - 1))) }
  | ['0'-'9']+ ('.' ['0'-'9']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?
      { FLOAT (float_of_string(Lexing.lexeme !Location.input_lexbuf)) }
  | "--"
      { comment !Location.input_lexbuf; main !Location.input_lexbuf }
  | "'<" { SOS }
  | "'>" { EOS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "," { COMMA }
  | "->" { ARROW }
  | "<-" { LARROW }
  | ";" { SEMIC }
  | ".." { DOTDOT }
  | ":" { COLON }
  | "%" { PERCENT }
(*   | "@" { AS } *)
(*   | "::" { COLONCOLON } *)
  | "_" { UNDERSCORE }
  | "'" { QUOTE }
  | "`" { BACKQUOTE }
  | "$" { DOLLAR }
(*   | "#" { SHARP } *)
  | "=" { EQUAL }
  | "==" { EQUALEQUAL }
  | ">>" { INFIX3 ">>" }
  | "<<" { INFIX3 "<<" }
  | ">" { GREATER }
  | "<" { LESS }
  | "&&" { AMPERAMPER }
  | "||" { BARBAR }
  | ">=" { GREATEREQUAL }
  | "<=" { LESSEQUAL }
  | "|" { BAR }
  | "!="    { INFIX0 "!=" }
  | "-"     { SUBTRACTIVE "-" }
  | "-."    { SUBTRACTIVE "-." }
  | "*" { STAR }
  | [ '<' '>' ] '=' ? '.' ? 
      { INFIX0(Lexing.lexeme !Location.input_lexbuf) }
  | [ '+' '-' ] '.' ?
      { INFIX2(Lexing.lexeme !Location.input_lexbuf) }
  | [ '*' '/' '%' ] '.' ?
      { INFIX3(Lexing.lexeme !Location.input_lexbuf) }
  | eof {
      if Stack.is_empty ctxs then
        begin
          if !cc_state = CC_Out then
            EOF
          else
            raise (Lexical_error (Unterminated_cond, 0, Lexing.lexeme_start !Location.input_lexbuf))
        end
      else 
        let lexbuf' = stop_incl () in 
        main lexbuf'
      }
  | _
      { raise (Lexical_error(Illegal_character,
                            Lexing.lexeme_start !Location.input_lexbuf, Lexing.lexeme_end !Location.input_lexbuf)) }

and comment = parse
  | "\n"
      { () }
  | eof
      { () }
  | _
      { comment !Location.input_lexbuf }

and exclude = parse
  | "#endif"
      { match !cc_state with 
          CC_ExclElse | CC_ExclIf ->
            cc_state := CC_Out;
            main !Location.input_lexbuf
        | _ ->
            Misc.fatal_error "Lexer: #endif in invalid context" (* should not happen *) }
  | "#else"
      { match !cc_state with 
          CC_ExclIf ->
            cc_state := CC_InclElse;
            main !Location.input_lexbuf
        | _ ->
            Misc.fatal_error "Lexer: #endif in invalid context" (* should not happen *) }
  | eof 
      { raise (Lexical_error
                (Unterminated_cond, 0, Lexing.lexeme_start !Location.input_lexbuf)) }
  | _
      { exclude !Location.input_lexbuf }

and string = parse
    '"' (*"'"'*)
      { () }
  | '\\' ("\010" | "\013" | "\013\010") [' ' '\009'] *
      { string !Location.input_lexbuf }
  | '\\' ['\\' '"' (*"'"'*) 'n' 't' 'b' 'r']
      { store_string_char(char_for_backslash(Lexing.lexeme_char !Location.input_lexbuf 1));
        string !Location.input_lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string_char(char_for_decimal_code !Location.input_lexbuf 1);
         string !Location.input_lexbuf }
  | eof
      { raise (Lexical_error
                (Unterminated_string, 0, Lexing.lexeme_start !Location.input_lexbuf)) }
  | _
      { store_string_char(Lexing.lexeme_char !Location.input_lexbuf 0);
        string !Location.input_lexbuf }
