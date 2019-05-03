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

(* Static computation of FIFO sizes for pure SDF DFGs. Since v 2.8.4 *)

type sdf_config = {
  mutable sdf_fifo_size_file_suffix: string;
  }

let cfg = {
  sdf_fifo_size_file_suffix = "_sdf_fifo_sizes.dat";
}

type node_id = Static.bid
type edge_id = Static.wid

type graph = {
  nodes: (node_id * node_desc) list;
  edges: (edge_id * edge_desc) list;
  }

and node_desc = {
  n_ins: edge_id list;
  n_outs: edge_id list;
  mutable icnt: int;  (* used for topological sorting *)
  }

and edge_desc = {
  e_src: node_id;
  e_dst: node_id;
  mutable phase: int;
  mutable cap: int;
  }

let get_node i g = try List.assoc i g.nodes with Not_found -> failwith "Sdf.get_node"
let get_edge i g = try List.assoc i g.edges with Not_found -> failwith "Sdf.get_edge"

let node_succs g n =
  List.map
    (function i -> (get_edge i g).e_dst)
    n.n_outs

exception NonSDF of Static.bid * string

let build_graph ir =
  let open Interm in
  let mk_node (i,b) = match b.ib_tag, b.ib_moc with
    Static.RegularB, Static.Moc_SDF (_,_)
  | Static.InpB _, _
  | Static.OutB _, _ ->
      let inps = List.map (function (_, (wid,_)) -> wid) b.ib_ins in
      i, { n_ins = inps;
           n_outs = Misc.flatmap (function (_, (wids,_)) -> wids) b.ib_outs;
           icnt = List.length inps }
  | _, _ -> raise (NonSDF (i,b.ib_name)) in
  let mk_edge (i,(((src,ssel),(dst,dsel)),_)) =
    i, { e_src = src;
         e_dst = dst;
         phase = -1;
         cap = -1 } in
  try {
    nodes = List.map mk_node ir.ir_boxes;
    edges = List.map mk_edge ir.ir_wires
    }
  with
    NonSDF (bid, name) -> Error.cannot_build_sdf_graph bid name

(* let string_of_node (i,n) = Printf.sprintf "B%d(%d)" i n.icnt *)
(* let string_of_edge (i,e) = Printf.sprintf "W%d(%d->%d,%d,%d)" i e.e_src e.e_dst e.phase e.cap *)

(* let dump_graph g = *)
(*   let open Printf in *)
(*   printf "Nodes: %s\n" (Misc.string_of_list string_of_node ", " g.nodes); *)
(*   printf "Edges: %s\n" (Misc.string_of_list string_of_edge ", " g.edges) *)

exception TopologicalSort

(* let topological_sort g = *)
(*   let q = Queue.create () in *)
(*   let r = ref [] in *)
(*   List.iter (function (i,n) -> if n.icnt = 0 then Queue.add i q) g.nodes; *)
(*   while not (Queue.is_empty q) do *)
(*     let i = Queue.pop q in *)
(*     let n = get_node i g in *)
(*     r := (i,n) :: !r; *)
(*     List.iter *)
(*       (function j -> *)
(*         let n' = get_node j g in *)
(*         n'.icnt <- n'.icnt - 1; *)
(*         if n'.icnt = 0 then Queue.add j q) *)
(*       (node_succs g n) *)
(*   done; *)
(*   if List.length !r = List.length g.nodes then *)
(*     { g with nodes = List.rev !r } *)
(*   else *)
(*     raise TopologicalSort *)

let compute_phases g =
  let edge i = get_edge i g in
  let q = Queue.create () in
  let visited = ref 0 in
  List.iter                    
    (function (i,n) ->
      if n.icnt = 0 then begin                                   (* For all source nodes .. *)
        List.iter (function j -> (edge j).phase <- 0) n.n_outs;  (* .. set phase of output edges to 0 *)
        Queue.add i q                                            (* .. add to visiting queue *)
       end)     
    g.nodes;
  while not (Queue.is_empty q) do
    let i = Queue.pop q in
    let n = get_node i g in
    incr visited;
    let phase_max = List.fold_left (fun m j -> Misc.max m (edge j).phase) 0 n.n_ins in  
    List.iter                                                    (* Adjust FIFO capacity of input edges *)
      (function j -> 
        let e = edge j in
        e.cap <- if e.phase < phase_max then 1 + phase_max - e.phase else 1)
      n.n_ins;
    List.iter                                                    (* Adjust phase of output edges *)
      (function j -> (edge j).phase <- phase_max + 1)
      n.n_outs;
    List.iter                                                    (* Iterate *)
      (function j ->
        let n' = get_node j g in
        n'.icnt <- n'.icnt - 1;
        if n'.icnt = 0 then Queue.add j q)
      (node_succs g n)
  done;
  if !visited <> List.length g.nodes then raise TopologicalSort

let dump_fifo_sizes name ir =
  let g = build_graph ir in
  compute_phases g;
(*   dump_graph g *)
  ir.Interm.ir_wire_annots <- List.fold_left (fun acc (i,e) -> (i, Interm.mk_wire_annot e.phase e.cap)::acc) [] g.edges;
  let fname = Misc.prefix_dir Genmake.target.Genmake.dir name ^ cfg.sdf_fifo_size_file_suffix in
  let oc = open_out fname  in
  List.iter (function (i,e) -> Printf.fprintf oc "w%d fifo_size = %d\n" i e.cap) g.edges;
  Logfile.write fname;
  close_out oc
