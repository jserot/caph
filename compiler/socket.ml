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

open Misc

let connect host port =
  let server_addr =
    try  Unix.inet_addr_of_string host 
    with Failure("inet_addr_of_string") -> 
      try  (Unix.gethostbyname host).Unix.h_addr_list.(0) 
      with Not_found -> 
        fatal_error ("cannot contact host " ^ host)
  in 
  let sockaddr = Unix.ADDR_INET(server_addr,port) in 
  let domain = Unix.domain_of_sockaddr sockaddr in
  let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
  try
    Unix.connect sock sockaddr;
    sock
  with exn ->
    Unix.close sock; 
    Error.err_open_socket (host, port)

let send fd m = 
      let mes =  Marshal.to_string m [] in
      let l = String.length mes in
      let ll = string_of_int l in
      let buffer = String.make 12 ' ' in 
      for i=0 to (String.length ll)-1 do buffer.[i] <- ll.[i] done ;
      ignore (Unix.write fd buffer 0 12);
      let written = ref 0 in
      while ( !written < l ) do 
        written := !written +  Unix.write fd mes !written (l - !written)
      done
        
let recv fd = 
      let buffer = String.make 12 ' '  in
      ignore (Unix.read fd buffer 0 12);
      let l =
        let i = ref 0 in
        while (buffer.[!i]<>' ') do incr i done;
        int_of_string (String.sub buffer 0 !i) in 
      let buffer = String.create l in
      let recvd = ref 0 in
      while ( !recvd < l ) do 
        recvd := !recvd +  Unix.read fd buffer !recvd (l - !recvd)
      done;
      Marshal.from_string buffer 0
