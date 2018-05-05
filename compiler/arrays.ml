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

module Array1 = struct
  type 'a t = 'a array
  exception Invalid_index of int
  exception Invalid_size
  let make n v = Array.make n v
  let init n f = Array.init n f
  let copy a = Array.copy a
  let size a = Array.length a
  let get a i = try a.(i) with Invalid_argument _ -> raise (Invalid_index i)
  let set a i v = try a.(i) <- v with Invalid_argument _ -> raise (Invalid_index i)
  let of_list l = Array.of_list l
  let to_list a = Array.to_list a
  let iter f a = Array.iter f a
  let iteri f a = Array.iteri f a
  let map f a = Array.map f a
  let mapi f a = Array.mapi f a
  let to_string ?(ld="[") ?(rd="]") ?(sep=",") f a = ld ^ Misc.string_of_list f sep (to_list a) ^ rd
end

module Array2 = struct
  type 'a t = 'a array array
  exception Invalid_index of int * int
  exception Invalid_size
  let init (n1,n2) f = Array.init n1 (function i -> Array1.init n2 (f i)) 
  let make (n1,n2) v = init (n1,n2) (fun i j -> v)
  let copy a = Array.init (Array.length a) (function i -> Array1.copy a.(i))
  let size a = Array.length a, try Array.length a.(0) with Invalid_argument _ -> 0
  let get a (i,j) = try a.(i).(j) with Invalid_argument _ -> raise (Invalid_index (i,j))
  let set a (i,j) v = try a.(i).(j) <- v with Invalid_argument _ -> raise (Invalid_index (i,j))
  let of_list ll = 
    if Misc.list_same List.length ll
    then Array.of_list (List.map Array1.of_list ll)
    else raise Invalid_size
  let to_list a = List.map Array1.to_list (Array.to_list a)
  let iter f a = Array.iter (Array1.iter f) a
  let iteri f a = Array.iteri (fun i a' -> Array1.iteri (f i) a') a
  let map f a = Array.map (Array1.map f) a
  let to_string ?(ld="[") ?(rd="]") ?(sep=",") f a =
    ld ^ Misc.string_of_list (Array1.to_string ~ld:ld ~rd:rd ~sep:sep f) sep (Array1.to_list a) ^ rd
end

module Array3 = struct
  type 'a t = 'a array array array
  exception Invalid_index of int * int * int
  exception Invalid_size
  let init (n1,n2,n3) f = Array.init n1 (function i -> Array2.init (n2,n3) (f i)) 
  let make (n1,n2,n3) v = init (n1,n2,n3) (fun i j k -> v)
  let copy a = Array.init (Array.length a) (function i -> Array2.copy a.(i))
  let size a = Array.length a, (try Array.length a.(0) with Invalid_argument _ -> 0), (try Array.length a.(0).(0) with Invalid_argument _ -> 0)
  let get a (i,j,k) = try a.(i).(j).(k) with Invalid_argument _ -> raise (Invalid_index (i,j,k))
  let set a (i,j,k) v = try a.(i).(j).(k) <- v with Invalid_argument _ -> raise (Invalid_index (i,j,k))
  let of_list lll = 
    if Misc.list_same List.length lll && Misc.list_same List.length (List.flatten lll)
    then Array.of_list (List.map Array2.of_list lll)
    else raise Invalid_size
  let to_list a = Array.to_list (Array.map Array2.to_list a)
  let iter f a = Array.iter (Array2.iter f) a
  let iteri f a = Array.iteri (fun i a' -> Array2.iteri (f i) a') a
  let map f a = Array.map (Array2.map f) a
  let map f a = Array.map (Array2.map f) a
  let to_string ?(ld="[") ?(rd="]") ?(sep=",") f a =
    ld ^ Misc.string_of_list (Array2.to_string ~ld:ld ~rd:rd ~sep:sep f) sep (Array1.to_list a) ^ rd
end

(* And so on.. *)

(* Note 2016-05-19, JS. An interesting question is whether this scheme can be generalized to provide 
 * an implementation for n-D arrays .. *) 
(* Unfortunatly it can't. The idea would be to define a functor 
 *    module LiftDim (A: ARRAY) = (struct ... : ARRAY)
 * taking a module implementing a n-D array and returning a module implementing a (n+1)-D array,
 * where ARRAY would be the signature common to arrays of any dimension :
 *   module type ARRAY = sig ... end
 * We would then define the implementation of an 1D array :
 *   module Array1 = (struct ... end : ARRAY)
 * and then create 2D, 3D, .. arrays by simply applying the LiftDim functor :
 *   module Array2 = LiftDim(Array1)
 *   module Array3 = LiftDim(Array2)
 *   etc..
 * The problem is that we can't define such a common signature.
 * If we write 
 *   module type ARRAY = sig type 'a t = 'a array ... end
 * then we have to define LiftDim as 
 *   module LiftDim (A: ARRAY) = struct type 'a t = 'a A.t array ... end
 * But then the signature of the input oand output modules do not match because 
         type 'a t = 'a A.t array
       is not included in
         type 'a t = 'a array
 *)
