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

(* Moc-based classification of boxes (since v2.8.4) *)

open Static

let compute_box_moc (bid,box) = 
  let rule_sig (_,_,s) = s in
  let m = match box.b_rules with
    [] ->
      Moc_Unknown
  | r::rs ->
      if Misc.list_same rule_sig box.b_rules then
        Moc_SDF (rule_sig r)
      else
        begin
          Printf.printf "> Running abstract interpreter on box B%d(%s) to infer Moc... " bid box.b_name;
          let m = match Absint.run_box box with
            Absint.AI_Cycle (ncy, rs) -> Moc_CSDF (List.map rule_sig (List.map fst rs))
          | Absint.AI_Overrun _ -> Moc_DDF
          | Absint.AI_Nondet _ -> Moc_DDF
          | _ -> Moc_Unknown in
          Printf.printf "Done.\n";
          m
        end in
  box.b_moc <- m

let dump_box_moc oc (bid,box) = 
  let string_of_moc = function 
      Moc_SDF r -> "sdf; " ^ Ssval.string_of_rule_sig r
    | Moc_CSDF rs -> "csdf; " ^ Misc.string_of_list (function r -> "[" ^ Ssval.string_of_rule_sig r ^ "]") "," rs
    | Moc_DDF -> "ddf"
    | Moc_Unknown -> "???" in
  match box.b_tag with
    RegularB -> Printf.fprintf oc "B%d; %s; %s\n" bid box.b_name (string_of_moc box.b_moc)
  | _ -> ()

let compute_mocs name sp = 
  List.iter compute_box_moc sp.boxes;
  let fname = Misc.prefix_dir Genmake.target.Genmake.dir name ^ "_mocs.dat" in
  let oc = open_out fname  in
  List.iter (dump_box_moc oc) sp.boxes;
  Logfile.write fname;
  close_out oc
