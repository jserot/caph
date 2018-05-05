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

(* Type/size var name handling *)

module type Var = sig
  type t
  val init: int
  val to_string: int -> string
end

module type S = sig
  type var
  val reset: unit -> unit
  val name_of: var -> string
end

module Make (V:Var) : (S with type var=V.t) = struct
  type var = V.t
  let counter = ref V.init
  let vars = ref ([] : (var * string) list)
  let reset () = counter := V.init; vars := []
  let name_of var =
    try
      List.assq var !vars
    with Not_found ->
      let name = V.to_string !counter in
      incr counter;
      vars := (var, name) :: !vars;
      name 
end
