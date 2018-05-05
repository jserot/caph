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

(* A general mechanism for keeping track of object "instances" *)

(* An object of type [('inst,'loc) S.t] is essentially an association table mapping keys to values
   where values are viewed as "instances" of generic objects and where we keep track of the location
   where the instanciation took place.
   - [S.key] is the type of the key 
   - [inst] is the type of instanciated values
   - [loc] is the type of locations.
   For example, in the static analysis step of the compiler, we need to keep track of each distinct
   instanciation (in terms of type) for each actor (so that the backend can latter generate specialized code).
   In this case :
   - [S.key] is the type signature of the instanciated actor
   - [inst] is the actor descripton in which all type (resp. size) variables have been replaced by
     their corresponding binding  (ex: [var z:int<s>] has been replaced by [var z:int<8>])
   - [locs] is the list of the all box indices corresponding to this particular instanciation (there
     may be several of them)
   A similar mechanism is used for type constructors and global functions (in this case locations are source file locations)
   May 18, 2015. JS
*)

module type EqType = sig
  type t
  val equal: t -> t -> bool
end

module type S = sig
  type key
  type ('inst, 'loc) t
  val empty: unit -> ('inst,'loc) t
  val card: ('inst,'loc) t -> int
  val mem: key -> ('inst,'loc) t -> bool
  val add: key -> 'inst * 'loc -> ('inst,'loc) t -> unit
  val to_list: ('inst,'loc) t -> (key * ('inst * 'loc list)) list
  val iter: (key * ('a * 'b list) -> unit) -> ('a,'b) t -> unit
  val map: ('a * 'b list -> 'c * 'd list) -> ('a,'b) t -> ('c,'d) t
end

module Make (A:EqType) : (S with type key=A.t) = struct
  type key = A.t
  type 'a lst = { mutable contents: 'a list }
  type ('inst, 'loc) t = (key * ('inst * 'loc lst)) lst
  let empty () = { contents = []  }
  let card m = List.length m.contents
  let mem k m = List.exists (function (k',_) -> A.equal k k') m.contents
  let add k (v,l) m =
    try
     let _, (v',ls') = List.find (function (k',_) -> A.equal k k') m.contents in
     ls'.contents <- l :: ls'.contents  (* Add [l] to the list of this particular entry locations *)
   with 
     Not_found ->
       m.contents <- (k,(v,{contents=[l]}))::m.contents  (* Add a new entry *)
  let to_list v = List.map (function (b,(i,ls)) -> b,(i,ls.contents)) v.contents
  let iter f m = List.iter (function (k,(v,l)) -> f (k,(v,l.contents))) m.contents
  let map f m = {
    contents =
    List.map
      (function (k,(v,l)) ->
        let v',l' = f (v, l.contents)
        in k, (v',{contents=l'}))
      m.contents
    }
end
    
