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

let fname = "caph.output"
let channel = ref None

let stop () = match !channel with
  Some oc -> close_out oc
| None -> ()
  
let start () = 
  stop ();
  let oc = open_out fname in
  channel := Some oc

let write fname =
  Genmake.add_target fname;
  if not !Misc.generate_makefiles then begin
    Printf.printf "Wrote file %s\n" fname;
    match !channel with
      Some oc -> Printf.fprintf oc "%s\n" fname
    | None -> ()
    end
