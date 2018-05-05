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

(* Tracing facilities *)

open Dsval
open Printf
open Dynamic

let tracing = ref false

let msg0 m = if !tracing then printf m; flush stdout
let msg1 m x = if !tracing then printf m x; flush stdout
let msg2 m x y = if !tracing then printf m x y; flush stdout
let msg3 m x y z = if !tracing then printf m x y z; flush stdout
let msg4 m x y z t = if !tracing then printf m x y z t; flush stdout
let msg5 m x y z t u = if !tracing then printf m x y z t u; flush stdout

let rec print_list p sep = function
    [] -> ()
  | [x] -> p x
  | x::xs -> p x; printf sep; print_list p sep xs

let dump_proc_sets title aps ips = 
      let pproc (pid,_) = printf "P%d" pid in
      printf "%s : A=[" title; 
      print_list pproc ";" aps;
      printf "] I=["; 
      print_list pproc ";" ips;
      printf "]\n"; flush stdout

let dump_channel (cid,c) =
  Printf.printf "W%d=[%s] "
    cid (Misc.string_of_list Expr.string_of_val "," c.ch_val) (* (List.length c.ch_val) s.ch_cap *)

let stat_channel c =
  let l = List.length c.ch_val in 
  if l > c.ch_stat.max_occ then c.ch_stat.max_occ <- l

let dump_channels title channels =
  printf "%s : " title ;
  List.iter dump_channel channels; 
  printf "\n"; flush stdout

let dump_channel_stats (cid,c) = 
  Printf.printf "W%d: %s %s\n" cid (string_of_chan_occ c) (string_of_chan_stat c.ch_stat)
  
let trace_channel cid v = 
  printf "** Got %a on wire W%d.\n" output_value v cid
