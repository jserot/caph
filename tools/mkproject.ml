(***************************************************************************************/
/*                                                                                     */
/*                This file is part of the CAPH Compiler distribution                  */
/*                            http://caph.univ-bpclermont.fr                           */
/*                                                                                     */
/*                                  Jocelyn SEROT                                      */
/*                         Jocelyn.Serot@univ-bpclermont.fr                            */
/*                                                                                     */
/*         Copyright 2011-2018 Jocelyn SEROT.  All rights reserved.                    */
/*  This file is distributed under the terms of the GNU Library General Public License */
/*      with the special exception on linking described in file ../LICENSE.            */
/*                                                                                     */
/***************************************************************************************)

(* Generate project .xml file from .proj project description *)

open Printf

type t_args = {
  mutable ifile: string;
  mutable ofile: string;
  mutable caph_root: string;
  mutable symbols: (string * string) list;
  mutable dotviewer: string;
  mutable pgmviewer: string;
  mutable main: string;
  mutable name: string;
  mutable platform: string;
  }

let args = {
  ifile = "";
  ofile = "";
  caph_root = "";
  symbols = [];
  dotviewer = "";
  pgmviewer = "";
  main = "main.cph";
  name = "";
  platform = "";
  }

let parse_symbol_def s = 
  if String.contains s '=' then
    let i = String.index s '=' in
    String.sub s 0 i, String.sub s (i+1) (String.length s - i - 1) 
  else
    s, ""

let options_spec = [
"-o", Arg.String (function s -> args.ofile <- s), "set name of the output file (default: <name>.cphpro)";
"-D", Arg.String (function s -> args.symbols <- parse_symbol_def s :: args.symbols), "define macro symbol for translation";
"-caph", Arg.String (function s -> args.caph_root <- s), "set path to caph install dir";
"-platform", Arg.String (function s -> args.platform <- s), "set target platform (unix|macos|win32)";
"-dotviewer", Arg.String (function s -> args.dotviewer <- s), "set path to .dot file visualizer (default: none)";
"-pgmviewer", Arg.String (function s -> args.pgmviewer <- s), "set path to .pgm file visualizer (default: none)";
"-main", Arg.String (function s -> args.name <- s), "set main input file (default: main.cph)";
"-name", Arg.String (function s -> args.name <- s), "set project name (default: directory name)"
]

let usage = "usage: mkproject [-platform name] [-o fname] [-caph path] [-dotviewer path] [-pgmviewer path] [-name name] [-main fname] fname"

let anonymous f = args.ifile <- f

let trim_comment ws =
  let rec h = function
      [] -> []
    | "#"::_ -> []
    | x::xs -> x :: h xs in
  h ws

let snd3 (_,x,_) = x

exception UnknownOption of string * string
exception MissingOptionArgument of string
exception UndefinedMacro of string

let opt_kind cat name = 
  try snd3 (List.find (function (name',_,_) -> name=name') Options_spec.options_spec)
  with Not_found -> raise (UnknownOption (cat,name))
    
type opt_cat =
    UnitOpt of string
  | AttrOpt of string * string

let string_of_opt = function
    UnitOpt name -> name
  | AttrOpt (name,value) -> name ^ "=" ^ value

let maybe_deref s =
  if Str.string_match (Str.regexp "\\$(\\([A-Z_]+\\))") s 0
  then 
    try List.assoc (Str.matched_group 1 s) args.symbols
    with Not_found -> raise (UndefinedMacro s)
  else s

let sort_opts (cat, vals) =
  let rec scan = function
    [] -> []
  | x::xs ->
      match opt_kind cat x with
        Arg.Unit _ ->
          UnitOpt x :: scan xs
      | Arg.String _  
      | Arg.Int _ ->
          begin match xs with
            [] -> raise (MissingOptionArgument x)
          | y::ys -> AttrOpt (x, maybe_deref y) :: scan ys
          end
      | _ -> 
          scan xs (* just skip *) in
  cat, scan vals

let xml_opt_cat = function 
  | "DOT_OPTS" -> "dot"
  | "SIM_OPTS" -> "simu"
  | "SC_OPTS" -> "systemc"
  | "VHDL_OPTS" -> "vhdl"
  | _ -> ""

let dump_xml_option oc cat = function
  | UnitOpt n ->
      Printf.fprintf oc "    <option type=\"unit\" value=\"true\" name=\"%s\" category=\"%s\"/>\n" n (xml_opt_cat cat)
  | AttrOpt (n,v) ->
      Printf.fprintf oc "    <option type=\"attr\" value=\"%s\" name=\"%s\" category=\"%s\"/>\n" v n (xml_opt_cat cat)

let xml_act_cat = function 
  | "PRE_PROC" -> "pre"
  | "POST_PROC" -> "post"
  | _ -> ""

let concat_path s ss = 
  let sep = match args.platform with
    "win32" -> "\\" 
  | _ -> "/" in
  s ^ sep ^ Misc.string_of_list Misc.id sep ss

let prefix_cmd c =
  match args.platform with
    "macos" -> "/Applications/Caph.app/Contents/MacOS/" ^ c 
  | "win32" -> args.caph_root ^ "\\bin\\" ^ c
  | _ -> args.caph_root ^ "/bin/" ^ c

let localize_cmd cmd =
    if List.mem cmd ["txt2bin"; "bin2txt"; "txt2pgm"; "pgm2txt"; "bin2pgm"; "pgm2bin"; "mkconv"]
    then prefix_cmd cmd
    else cmd

let dump_xml_action oc (cat,act) = 
  match act with
    cmd::args ->
      Printf.fprintf oc "    <action type=\"%s\" command=\"%s\" args=\"%s\"/>\n"
        (xml_act_cat cat)
        (localize_cmd cmd)
        (Misc.string_of_list Misc.id " " args)
  | [] ->
      ()

let unquote s = 
  let l = String.length s in
  if l > 2 && s.[0] = '\"' && s.[l-1] = '\"' then String.sub s 1 (l-2) else s

let dump_xml_project dir pname opts acts = 
  let f = match args.ofile with "" -> pname ^ ".cphpro" | s -> s in
  let oc = open_out f in
  Printf.fprintf oc "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"; 
  Printf.fprintf oc "<project mainfile=\"%s\" name=\"%s\">\n" args.main pname;
  Printf.fprintf oc "  <tools>\n";
  Printf.fprintf oc "    <tool path=\"%s\" name=\"caphc\"/>\n" (prefix_cmd "caphc");
  Printf.fprintf oc "    <tool path=\"%s\" name=\"dotViewer\"/>\n" (unquote args.dotviewer);
  Printf.fprintf oc "    <tool path=\"%s\" name=\"pgmViewer\"/>\n" (unquote args.pgmviewer);
  Printf.fprintf oc "  </tools>\n";
  Printf.fprintf oc "  <options>\n";
  List.iter (function (cat,opts') -> List.iter (dump_xml_option oc cat) opts') opts;
  Printf.fprintf oc "  </options>\n";
  Printf.fprintf oc "  <actions>\n";
  List.iter (dump_xml_action oc) acts;
  Printf.fprintf oc "  </actions>\n";
  Printf.fprintf oc "</project>\n";
  Printf.printf "Wrote file %s\n" f;
  close_out oc

let parse_config keys file = 
  (* Parse a file containing lines of the form "KEY = ARGS [# comments]" *)
  let ic = open_in file in
  let res = ref [] in
  try
    while true do
      let line = input_line ic in
      begin match Str.split (Str.regexp "[ \t]+") line with
      | key :: "=" :: args when List.mem key keys -> 
          res := (key, trim_comment args) :: !res
      | _ ->
          ()
      end
    done; 
    []
  with End_of_file ->
    !res

let main () =
try
  Arg.parse options_spec anonymous usage;
  let dir = Sys.getcwd () in
  let pname = if args.name <> "" then args.name else Filename.basename dir in
  let opts = parse_config ["DOT_OPTS"; "SIM_OPTS"; "SC_OPTS"; "VHDL_OPTS"] args.ifile in
  let acts = 
    let f' = pname ^ ".procs" in
    if Sys.file_exists f' then parse_config ["PRE_PROC"; "POST_PROC"] f' else [] in
  args.symbols <- ("CAPHLIB", concat_path args.caph_root ["lib";"caph"]) :: args.symbols;
  dump_xml_project dir pname (List.map sort_opts opts) acts
with
| Sys_error msg ->
    eprintf "Input/output error: %s.\n" msg;
    flush stderr;
    exit 6
| UnknownOption (c,s) ->
    eprintf "Unrecognized option \"%s:%s\"\n" c s;
    flush stderr; exit 7
| MissingOptionArgument s ->
    eprintf "Missing argument for option \"%s\"\n" s;
    flush stderr; exit 8
| UndefinedMacro s ->
    eprintf "No definition for macro symbol \"%s\". This can be fixed using the [-D] option\n" s;
    flush stderr; exit 9
| e ->
    eprintf "Internal error: %s.\n" (Printexc.to_string e);
    flush stderr; exit 10

let _ = Printexc.print main ()
