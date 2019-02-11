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

open Printf
open Syntax
open Const
open Misc

type t_mode =  Shifted | Centered | Neigh

(* AST aux builders *)

let mk_exp d = { e_desc = d; e_loc = Location.no_location; e_typ = Types.no_type }
let mk_int c = mk_exp (EConst (CInt (c,None,None)))
let mk_tyexp d = { te_desc = d; te_loc = Location.no_location; te_typ = Types.no_type }
let mk_szexp d = { se_desc = d; se_loc = Location.no_location }
let mk_io (id,ty) = { aio_desc = (id, ty); aio_loc = Location.no_location }
let mk_param (id,ty) = { param_desc = (id, ty); param_loc = Location.no_location }
let mk_var (id,ty,iv) = { var_desc = (id, mk_tyexp ty, iv); var_loc = Location.no_location }
let mk_rsch d = { rsch_desc = d; rsch_loc = Location.no_location }
let mk_rqual d = { q_desc = d; q_loc = Location.no_location }
let mk_rpat d = { rp_desc = d; rp_loc = Location.no_location; rp_typ = Types.no_type }
let mk_rlhs d = { rlhs_desc = List.map (function (q,rp) -> mk_rqual q, mk_rpat rp) d; rlhs_loc = Location.no_location }
let mk_rgrd d = { rgrd_desc = d; rgrd_loc = Location.no_location }
let mk_rrhs d = { rrhs_desc = List.map (function (q,e) -> mk_rqual q, mk_exp e) d; rrhs_loc = Location.no_location }
let mk_rule (lhs,rhs) = { rule_desc = mk_rlhs lhs, mk_rgrd [], mk_rrhs rhs; rule_loc = Location.no_location }
let mk_op op e1 e2 = EApp (mk_exp (EVar op), None, [mk_exp e1; mk_exp e2])
let no_impl = []
let no_rsch = mk_rsch ([],[])

let id p i = p ^ string_of_int i
let idd p i j = p ^ string_of_int i ^ string_of_int j
let qin i = QIn i
let qout o = QOut o
let qvar v = QVar (v,[])
let qarr a is = QVar (a,is)
let pcon0 c = RPatCon (c,[])
let pcon1 c p = RPatCon (c, [mk_rpat p])
let econ0 c = ECon (c, [])
let econ1 c es = ECon (c, List.map mk_exp es)
let rCon0 i c = qin i, pcon0 c
let wCon0 o c = qout o, econ0 c

let mk_arr1_ty t n = Typeconstr ("array",[t],[mk_szexp (Sizeconst n)])
let mk_arr2_ty t n1 n2 = Typeconstr ("array",[mk_tyexp (mk_arr1_ty t n2)],[mk_szexp (Sizeconst n1)])

let arr1_rd a i = EArrRead (a, [mk_int i])
let arr2_rd a i j = EArrRead (a, [mk_int i; mk_int j])

let rTrans s0 s1 rule_conds rule_acts = mk_rule ((qvar "s", pcon0 s0) :: rule_conds, (qvar "s", econ0 s1) :: rule_acts)

let rInpC c = qin "i", pcon0 c 
let wOutC c = qout "o", econ0 c 
let rFifoC j c = qvar (id "z" j), pcon0 c 
let wFifoC j c = qvar (id "oz" j), econ0 c 
let rInpD v = qin "i", pcon1 "Data" v
let rFifo j v = qvar (id "z" j), v
let rFifoD j v = qvar (id "z" j), pcon1 "Data" v
let wOutD e = qout "o", econ1 "Data" [e]
let wFifo j e = qvar (id "oz" j), e
let wFifoD j e = qvar (id "oz" j), econ1 "Data" [e]

let rData s p = s, pcon1 "Data" p
let wData s e = s, econ1 "Data" [e]

(* Conv specific shortcuts *)

let t0 = mk_tyexp (Typeconstr ("int", [], []))
let t1 = mk_tyexp (Typeconstr ("int", [mk_tyexp (Typevar "s")], [mk_szexp (Sizevar "m")]))
let t2 = mk_tyexp (Typeconstr("dc",[t1],[]))

let shift n p z =
  [qarr z [mk_int j], (if j=0 then EVar p else arr1_rd z (j-1)) | j <- range 0 (n-2)]

(* 1D *)

let conv1 n p k z = 
  List.fold_left 
      (fun acc j -> mk_op "+" acc (mk_op "*" (EArrRead (z,[mk_int j])) (EArrRead (k, [mk_int (n-2-j)]))))
      (mk_op "*" (EVar "p") (EArrRead (k,[mk_int (n-1)])))
      (Misc.list_make 0 (n-2) (function j -> j))

let sconv1 n s p k z = 
  mk_op ">>" (conv1 n p k z) (EVar s)

let mk_shifted_conv1_actor name w = 
  let st = id "S" in
  { a_id = name;
    a_params = List.map mk_param [
      "k", mk_tyexp (mk_arr1_ty t1 w);
      "n", t0;
      "v", t1
      ];
    a_ins = [mk_io ("i", t2)];
    a_outs = [mk_io ("o", t2)];
    a_vars = [
      mk_var ("s", TypeEnum [st j | j <- range 0 w], Some (mk_exp (econ0 "S0")));
      mk_var ("z", mk_arr1_ty t1 (w-1), None)
      ];
    a_rsch = no_rsch;
    a_rules = 
         [rTrans "S0" "S1" [rInpC "SoS"] [wOutC "SoS"]]
       @ [rTrans "S1" "S0" [rInpC "EoS"] [wOutC "EoS"]]
       @ [rTrans (st j) (st (j+1)) [rData (qin "i") (RPatVar "p")]
                                   (wData (qout "o") (EVar "v") :: shift (j+1) "p" "z") | j <- range 1 (w-1)] 
       @ [rTrans (st w) (st w) [rData (qin "i") (RPatVar "p")]
                                   (wData (qout "o") (sconv1 w "n" "p" "k" "z") :: shift w "p" "z")] 
       @ [rTrans (st w) "S0" [rInpC "EoS"] [wOutC "EoS"]];
    a_impl = no_impl
  }

let mk_centered_conv1_actor name n =
  let st = id "S" in
  let m = (n-1)/2 in
  { a_id = name;
    a_params = List.map mk_param [
      "k", mk_tyexp (mk_arr1_ty t1 n);
      "n", t0;
      "v", t1
      ];
    a_ins = [mk_io ("i", t2)];
    a_outs = [mk_io ("o", t2)];
    a_vars = [
      mk_var ("s", TypeEnum [st j | j <- range 0 (n+m)], Some (mk_exp (ECon ("S0",[]))));
      mk_var ("z", mk_arr1_ty t1 (n-1), None)
      ];
    a_rsch = no_rsch;
    a_rules =
         [rTrans "S0" "S1" [rInpC "SoS"] [wOutC "SoS"]]
       @ [rTrans "S1" "S0" [rInpC "EoS"] [wOutC "EoS"]]
       @ [rTrans (st j) (st (j+1)) [rData (qin "i") (RPatVar "p")] (shift (j+1) "p" "z") | j <- range 1 m]
       @ [rTrans (st j) (st (j+1)) [rData (qin "i") (RPatVar "p")]
                                   (wData (qout "o") (EVar "v") :: shift (j+1) "p" "z") | j <- range (m+1) (n-1)]
       @ [rTrans (st n) (st n) [rData (qin "i") (RPatVar "p")]
                                   (wData (qout "o") (sconv1 n "n" "p" "k" "z") :: shift n "p" "z")]
       @ [rTrans (st n) (st (n+1)) [rInpC "EoS"] [wData (qout "o") (EVar "v")]]
       @ [rTrans (st j) (st (j+1)) [] [wData (qout "o") (EVar "v")] | j <- range (n+1) (n+m-1)]
       @ [rTrans (st (n+m)) "S0" [] [wOutC "EoS"]];
    a_impl = no_impl
  }

let mk_neigh1_actor name n =
  let st = id "S" in
  let wOutC c j = qout (id "o" j), econ0 c in
  { a_id = name;
    a_params = [mk_param ("v", t1)];
    a_ins = [mk_io ("i", t2)];
    a_outs = [ mk_io ("o" ^ string_of_int i, t2) | i <- range 1 n ];
    a_vars = [
      mk_var ("s", TypeEnum [st j | j <- range 0 n], Some (mk_exp (econ0 "S0")));
      mk_var ("z", mk_arr1_ty t1 (n-1), None)
      ];
    a_rsch = no_rsch;
    a_rules =
      [rTrans "S0" "S1" [rInpC "SoS"] [wOutC "SoS" j | j <- range 1 n]]
    @ [rTrans "S1" "S0" [rInpC "EoS"] [wOutC "EoS" j | j <- range 1 n]]
    @ [rTrans (st k) (st (k+1))
         [rData (qin "i") (RPatVar "p")]
         ([wData (qout (id "o" j)) (if j=1 then EVar "p" else if j<=k then arr1_rd "z" (j-2) else EVar "v") | j <- range 1 n]
          @ shift (k+1) "p" "z")
      | k <- range 1 (n-1)]
    @ [rTrans (st n) (st n)
         [rData (qin "i") (RPatVar "p")]
         ([wData (qout (id "o" j)) (if j=1 then EVar "p" else arr1_rd "z" (j-2)) | j <- range 1 n] @ shift n "p" "z")]
    @ [rTrans (st n) "S0" [rInpC "EoS"] [wOutC "EoS" j | j <- range 1 n]];
    a_impl = no_impl
  }

(* 2D *)

(* Shortcuts *)

let shiftx n i =
  let z = "x[" ^ string_of_int i ^ "]" in
  [ qarr z [mk_int j], if j=0 then EVar (id "p" i) else EArrRead (z,[mk_int (j-1)]) | j <- range 0 (n-1) ]

let conv2 nr nc k p x =
  let convl i = List.fold_left
      (fun acc j -> mk_op "+" acc (mk_op "*" (arr2_rd k i (nc-2-j)) (arr2_rd x (nr-1-i) j)))
      (mk_op "*" (EVar (id p (nr-1-i))) (arr2_rd k i (nc-1)))
      [ j | j <- range 0 (nc-2) ] in
  foldl1 (fun e1 e2 -> mk_op "+" e1 e2) [convl i | i <- range 0 (nr-1)]

let sconv2 nr nc s p k z = mk_op ">>" (conv2 nr nc k p z) (EVar s)

let mk_shifted_conv2_actor name h w = 
  { a_id = name;
    a_params = List.map mk_param [
      "k", mk_tyexp (mk_arr2_ty t1 h w);
      "n", t0;
      "v", t1
      ];
    a_ins = mk_io ("i", t2) :: [mk_io (id "z" j, t1) | j <- range 0 (h-2)];
    a_outs = mk_io ("o", t2) :: [mk_io (id "oz" j, t1) | j <- range 0 (h-2)];
    a_vars = 
         mk_var ("s",
                 TypeEnum (["SoF";"SoL1"] +@ [[id "L" j; id "SoL" (j+1)] | j <- range 1 (h-1)] @ [id "P" j | j <- range 1 w]),
                 Some (mk_exp (econ0 "SoF")))
      :: (if w > 1 then [mk_var ("x", mk_arr2_ty t1 h (w-1), None)] else []);
    a_rsch = no_rsch;
    a_rules = 
         [rTrans "SoF" "SoL1" [rInpC "SoS"] [wOutC "SoS"]]
       @ [rTrans "SoL1" "SoF" [rInpC "EoS"] [wOutC "EoS"]]
      +@ [[rTrans (id "SoL" (k+1)) (id "L" (k+1)) [rInpC "SoS"] [wOutC "SoS"];
          rTrans
            (id "L" (k+1))
            (id "L" (k+1))
            ((rData (qin "i") (RPatVar "p0")) :: [rFifo j (RPatVar (id "p" (j+1))) | j <- range 0 (k-1)])
            ((wData (qout "o") (EVar "v")) :: [wFifo j (EVar (id "p" j))  | j <- range 0 k]);
          rTrans (id "L" (k+1)) (id "SoL" (k+2)) [rInpC "EoS"] [wOutC "EoS"]]
          | k <- range 0 (h-2)]
       @ [rTrans (id "SoL" h) "P1" [rInpC "SoS"] [wOutC "SoS"]]
       @ [rTrans (id "SoL" h) "SoF" [rInpC "EoS"] [wOutC "EoS"]]
       @ [rTrans
            (id "P" (k+1))
            (id "P" (k+2))
            ((rData (qin "i") (RPatVar "p0")) :: [rFifo j (RPatVar (id "p" (j+1))) | j <- range 0 (h-2)])
            ((wData (qout "o") (EVar "v")) :: [wFifo j (EVar (id "p" j))  | j <- range 0 (h-2)] +@ [shiftx (k+1) j| j <- range 0 (h-1)])
          |  k <- range 0 (w-2)]
       @ [rTrans
             (id "P" w)
             (id "P" w)
             ((rData (qin "i") (RPatVar "p0")) :: [rFifo j (RPatVar (id "p" (j+1))) | j <- range 0 (h-2)])
             ((wData (qout "o") (sconv2 h w "n" "p" "k" "x")) :: [wFifo j (EVar (id "p" j))  | j <- range 0 (h-2)] +@ [shiftx (w-1) j| j <- range 0 (h-1)])]
       @ [rTrans (id "P" w) (id "SoL" h) [rInpC "EoS"] [wOutC "EoS"]];
    a_impl = no_impl
  }

let mk_centered_conv2_actor name h w =
  let h' = (h-1)/2 in
  let w' = (w-1)/2 in
  { a_id = name;
    a_params = List.map mk_param [
      "k", mk_tyexp (mk_arr2_ty t1 h w);
      "n", t0;
      "v", t1
      ];
    a_ins = mk_io ("i", t2) :: [mk_io (id "z" j, t2) | j <- range 0 (h-2)];
    a_outs = mk_io ("o", t2) :: [mk_io (id "oz" j, t2) | j <- range 0 (h-2)];
    a_vars =
          mk_var ("s",
                  TypeEnum (  ["SoF"] +@ [[id "SoL" k; id "L" k] | k <- range 1 (h-1)] @ [id "SoL" h] @ [id "P" k | k <- range 1 w]
                            @ [id "E" k | k <- range 1 w'] +@ [[idd "F" k 1; idd "F" k 2] | k <- range 1 (h'-1)] @ [idd "F" h' 1; "EoF"]),
                  Some (mk_exp (econ0 "SoF")))
      :: (if w > 1 then [mk_var ("x", mk_arr2_ty t1 h (w-1), None)] else []);
    a_rsch = no_rsch;
    a_rules =
         [rTrans "SoF" "SoL1" [rInpC "SoS"] [wOutC "SoS"]]
       @ [rTrans "SoL1" "SoF" [rInpC "EoS"] [wOutC "EoS"]]
      +@ [[rTrans
            (id "SoL" k)
            (id "L" k)
            ([rInpC "SoS"] @ [rFifoC j "SoS" | j <- range 0 (k-2)])
            ((if k>h' then [wOutC "SoS"] else []) @ [wFifoC j "SoS" | j <- range 0 (k-1)]);
          rTrans
            (id "L" k)
            (id "L" k)
            ([rInpD (RPatVar "p0")] @ [rFifoD j (RPatVar (id "p" (j+1))) | j <- range 0 (k-2)])
            ((if k>h' then [wOutD (EVar "v")] else []) @ [wFifoD j (EVar (id "p" j)) | j <- range 0 (k-1)]);
          rTrans
            (id "L" k)
            (id "SoL" (k+1))
            ([rInpC "EoS"] @ [rFifoC j "EoS" | j <- range 0 (k-2)])
            ((if k>h' then [wOutC "EoS"] else []) @ [wFifoC j "EoS" | j <- range 0 (k-1)])]
        | k <- range 1 (h-1)]
       @ [rTrans
            (id "SoL" h)
            "P1"
            ([rInpC "SoS"] @ [rFifoC j "SoS" | j <- range 0 (h-2)])
            ([wOutC "SoS"] @ [wFifoC j "SoS" | j <- range 0 (h-2)])]
       @ [rTrans
            (id "P" k)
            (id "P" (k+1))
            ([rInpD (RPatVar "p0")] @ [rFifoD j (RPatVar (id "p" (j+1))) | j <- range 0 (h-2)])
            ((if k>w' then [wOutD (EVar "v")] else [])
             +@ [shiftx k i | i <- range 0 (h-1)] @ [wFifoD j (EVar (id "p" j)) | j <- range 0 (h-2)])
         | k <- range 1 (w-1)]
       @ [rTrans
            (id "P" w)
            (id "P" w)
            ([rInpD (RPatVar "p0")] @ [rFifoD j (RPatVar (id "p" (j+1))) | j <- range 0 (h-2)])
            ([wOutD (sconv2 h w "n" "p" "k" "x")]
             +@ [shiftx (w-1) i | i <- range 0 (h-1)] @ [wFifoD j (EVar (id "p" j)) | j <- range 0 (h-2)])]
       @ [rTrans
            (id "P" w)
            (id "E" 1)
            ([rInpC "EoS"] @ [rFifoC j "EoS" | j <- range 0 (h-2)])
            ([wOutD (EVar "v")] @ [wFifoC j "EoS" | j <- range 0 (h-2)])]
       @ [rTrans (id "E" k) (id "E" (k+1)) ([]) ([wOutD (EVar "v")]) | k <- range 1 (w'-1)]
       @ [rTrans (id "E" w') (id "SoL" h) [] ([wOutC "EoS"])]
       @ [rTrans
            (id "SoL" h)
            (idd "F" (if h'>0 then 1 else 0) 1)
            ([rInpC "EoS"] @ [rFifoC j "SoS" | j <- range 0 (h-2)])
            ([wOutC "SoS"] @ (if h' > 1 then [wFifoC j "SoS" | j <- range 0 (h-2)] else []))]
      +@ [[rTrans
            (idd "F" k 1)
            (idd "F" k 1)
            ([rFifoD j (RPatVar (id "p" (j+1))) | j <- range 0 (h-2)])
            ([wOutD (EVar "v")] @ [wFifoD j (EVar "v") | j <- range 0 (h-2)]);
           rTrans
            (idd "F" k 1)
            (idd "F" k 2)
            ([rFifoC j "EoS" | j <- range 0 (h-2)])
            ([wOutC "EoS"] @ [wFifoC j "EoS" | j <- range 0 (h-2)]);
           rTrans
            (idd "F" k 2)
            (idd "F" (k+1) 1)
            ([rFifoC j "SoS" | j <- range 0 (h-2)])
            ([wOutC "SoS"])]
         | k <- range 1 (h'-1)]
       @ [rTrans
            (idd "F" h' 1)
            (idd "F" h' 1)
            ([rFifoD j (RPatVar (id "p" (j+1))) | j <- range 0 (h-2)])
            ([wOutD (EVar "v")])]
       @ [rTrans (idd "F" h' 1) "EoF" ([rFifoC j "EoS" | j <- range 0 (h-2)]) ([wOutC "EoS"])]
       @ [rTrans "EoF" "SoF" [] ([wOutC "EoS"])];
    a_impl = no_impl
  }

(* NEIGH2xx *)

let mk_neigh2_actor name h w = 
  let rData s p = s, pcon1 "Data" p in
  let wData s e = s, econ1 "Data" [e] in
  let mk_o i j = "o" ^ string_of_int ((i-1)*w+j) in
  let mk_os f = [ f (mk_o i j) | i <- range 1 h; j <- range 1 w ] in
  let wSoS o = wCon0 o "SoS" in
  let wEoS o = wCon0 o "EoS" in
  let wout k i =
    [wData (qout (mk_o i 1)) (EVar (id "p" (i-1)))]
    @ [wData (qout (mk_o i j)) (arr2_rd "x" (i-1) (j-2)) | j <- range 2 k]
    @ [wData (qout (mk_o i j)) (EVar "v") | j <- range (k+1) w] in
  let wout' k l =
    let v i j =
      if i > k || j > l then EVar "v"
      else begin 
        if j = 1 then EVar (id "p" (i-1))
        else arr2_rd "x" (i-1) (j-2)
      end in
    [wData (qout (mk_o i j)) (v i j) | i <- range 1 h; j <- range 1 w] in
  { a_id = name;
    a_params = List.map mk_param [ "v", t1 ];
    a_ins = mk_io ("i", t2) :: [mk_io (id "z" j, t1) | j <- range 0 (h-2)];
    a_outs = mk_os (function o -> mk_io (o, t2)) @ [mk_io (id "oz" j, t1) | j <- range 0 (h-2)];
    a_vars =
          mk_var ("s",
                  TypeEnum (["S0"]
                          @ [idd "P" i j |  i <- range 1 (h-1); j <- range 0 w]
                          @ [idd "P" h 0]
                          @ [id "P" j |  j <- range 1 w]), Some (mk_exp (econ0 "S0")))
      :: (if w > 1 then [mk_var ("x", mk_arr2_ty t1 h (w-1), None)] else []);
    a_rsch = no_rsch;
    a_rules = 
         [rTrans "S0" (idd "P" 1 0) [rInpC "SoS"] (mk_os wSoS)]
       @ [rTrans (idd "P" 1 0) "S0" [rInpC "EoS"]  (mk_os wEoS)]
      +@ [  [rTrans (idd "P" k 0) (idd "P" k 1) [rInpC "SoS"]  (mk_os wSoS)]
          @ [rTrans
               (idd "P" k l)
               (idd "P" k (if l<w then l+1 else l))
               ((rData (qin "i") (RPatVar "p0")) :: [rFifo j (RPatVar (id "p" (j+1))) | j <- range 0 (k-2)])
               ((wout' k l) @ [wFifo j (EVar (id "p" j))  | j <- range 0 (k-1)] +@ [shiftx (if l<w then l else w-1) j | j <- range 0 (k-1)])
            | l <- range 1 w]
          @ [rTrans (idd "P" k w) (idd "P" (k+1) 0) [rInpC "EoS"]  (mk_os wEoS)]
         | k <- range 1 (h-1)]
       @ [rTrans (idd "P" h 0) "S0" [rInpC "EoS"] (mk_os wEoS)]
       @ [rTrans (idd "P" h 0) (id "P" 1) [rInpC "SoS"] (mk_os wSoS)]
       @ [rTrans
            (id "P" k)
            (id "P" (k+1))
            ((rData (qin "i") (RPatVar "p0")) :: [rFifo j (RPatVar (id "p" (j+1))) | j <- range 0 (h-2)])
            ((List.concat [wout k i | i <- range 1 h]) @ [wFifo j (EVar (id "p" j))  | j <- range 0 (h-2)] +@ [shiftx k j | j <- range 0 (h-1)])
         | k <- range 1 (w-1)]
       @ [rTrans
            (id "P" w)
            (id "P" w)
            ((rData (qin "i") (RPatVar "p0")) :: [rFifo j (RPatVar (id "p" (j+1))) | j <- range 0 (h-2)])
            ([wFifo j (EVar (id "p" j))  | j <- range 0 (h-2)] +@ [wout w i | i <- range 1 h] +@ [shiftx (w-1) j | j <- range 0 (h-1)])
         | k <- range 1 (w-1)]
       @ [rTrans (id "P" w) (idd "P" h 0) [rInpC "EoS"] (mk_os wEoS)];
    a_impl = no_impl
  }

(* Network generators *)

let mk_conv_net mode nname aname h w =
  let args = match mode with
    Shifted | Centered -> ["kernel";"norm";"pad"]
  | Neigh -> ["pad"] in
  let outs, outs' = match mode with
    Shifted | Centered -> "o", "o"
  | Neigh -> 
      let s = Misc.string_of_list (id "o") "," (range 1 (h*w)) in
      s, "(" ^ s ^ ")" in
  let sf = Misc.string_of_list (function s -> s) in
  if h <= 1 then
    [Printf.sprintf "net %s (%s) i = let %s = %s (%s) i in %s" nname (sf "," args) outs' aname (sf "," args) outs']
  else
    let zs = Misc.string_of_list (id "z") "," (range 0 (h-2)) in
    [Printf.sprintf "net %s (%s) i = let rec (%s,%s) = %s (%s) (i,%s) in %s"
       nname (sf "," args) outs zs aname (sf "," args) zs outs]

(* Main *)

exception Invalid_mode of string
let mode_of_string = function
    "shifted" -> Shifted
  | "centered" -> Centered
  | "neigh" -> Neigh
  | m -> raise (Invalid_mode m)

type t_args = {
  mutable ofile: string;
  mutable name: string;
  mutable mode: t_mode;
  mutable dim: int;
  mutable h: int;
  mutable w: int;
}

let args = { ofile="stdout"; name=""; mode=Shifted; dim=0; h=1; w=0 }

let options_spec = [
"-o", Arg.String (function s -> args.ofile <- s), "set output file (default is stdout)";
"-name", Arg.String (function s -> args.name <- s), "set actor name (default is (conv|cconv|neigh)<dim><h><w>a)";
"-mode", Arg.String (function s -> args.mode <- mode_of_string s), "set mode (shifted|centered|neigh) (default: shifted)";
"-dim", Arg.Int (function s -> args.dim <- s), "set signal dimension (1|2) (default: 1)";
"-h", Arg.Int (function s -> args.h <- s), "set kernel height (default: 1)";
"-w", Arg.Int (function s -> args.w <- s), "set kernel width";
]

let usage = "usage: mkconv [-name name] [-o file] -dim d -mode (shifted|centered|neigh) -h nr -w nc"

let anonymous = function _ -> ()

let dump_banner oc = 
  Printf.fprintf oc "-- -------------------------------------------------------------------------------\n";
  Printf.fprintf oc "-- This file has been automatically generated with command :\n";
  Printf.fprintf oc "-- %s\n" (Misc.string_of_list (function i -> i) " " (Array.to_list Sys.argv));
  Printf.fprintf oc "-- -------------------------------------------------------------------------------\n\n"

let main () =
try
  Arg.parse options_spec anonymous usage;
  if args.dim = 0 || args.dim > 1 && args.h = 0 || args.w = 0 then begin
    fprintf stderr "%s\n" usage;
    exit 1
    end;
  let oc = if args.ofile = "stdout" then stdout else open_out args.ofile in
  let mk_names mode = 
    let pfx = match mode with Shifted -> "conv" | Centered -> "cconv" | Neigh -> "neigh" in
    if args.name <> "" then args.name ^ "_act", args.name
    else sprintf "%s%d%d%da" pfx args.dim args.h args.w, sprintf "%s%d%d%d" pfx args.dim args.h args.w in
  dump_banner oc;
  let a_name, n_name = mk_names args.mode in
  let actor, ndefns = match args.dim, args.mode, args.h, args.w with
   | 1, Shifted, h, w when w>1 -> mk_shifted_conv1_actor a_name w, mk_conv_net Shifted n_name a_name h w
   | 1, Centered, h, w when w>1 ->
       if w mod 2 = 1 then mk_centered_conv1_actor a_name w, mk_conv_net Centered n_name a_name h w
      else invalid_arg "mkconv: w must be odd for centered convolutions"
   | 1, Neigh, h, w when w>1 -> mk_neigh1_actor a_name w, mk_conv_net Neigh n_name a_name h w
   | 2, Shifted, h, w when h>0 && w>0 -> mk_shifted_conv2_actor a_name h w, mk_conv_net Shifted n_name a_name h w
   | 2, Centered, h, w when h>0 && w>0 ->
       if w mod 2 = 1 then mk_centered_conv2_actor a_name h w, mk_conv_net Centered n_name a_name h w
      else invalid_arg "mkconv: w must be odd for centered convolutions"
   | 2, Neigh, h, w when h>0 && w>0 -> mk_neigh2_actor a_name h w, mk_conv_net Neigh n_name a_name h w
   | _, _, _, _ -> invalid_arg "mkconv: invalid argument(s)" in
  Printf.fprintf oc "#include \"dc.cph\"\n\n";
  Syntax.dump_actor_decl oc actor;
  List.iter (Printf.fprintf oc "%s;\n") ndefns
with
| Sys.Break -> flush stderr; exit 5
| Sys_error msg ->
    eprintf "Input/output error: %s.\n" msg;
    flush stderr;
    exit 6
| e ->
    eprintf "Internal error: %s.\n" (Printexc.to_string e);
    flush stderr; exit 7

let _ = Printexc.print main ()
