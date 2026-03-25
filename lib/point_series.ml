(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

type 'c query_fn = Date.t -> 'c Point_cell.t

type 'c t =
  | Const of { id : int; value : float }
  | Map of { id : int; inner : 'c t Lazy.t; f : float -> float }
  | Convert of { id : int; inner : 'c t Lazy.t; f : Date.t -> float -> float }

(*  Constructors *)
let next_id = Atomic.make 0
let fresh_id () = Atomic.fetch_and_add next_id 1
let const value = Const { id = fresh_id (); value }
let map f s = Map { id = fresh_id (); inner = s; f }
let convert f s = Convert { id = fresh_id (); inner = (Obj.magic s : _ t Lazy.t); f }

(*  Accessors *)
let id = function Const { id; _ } -> id | Map { id; _ } -> id | Convert { id; _ } -> id

(*  Query *)

let rec eval_query cache series date =
  let series_id = id series in
  match Hashtbl.find_opt cache series_id with
  | Some value -> value
  | None ->
      let value =
        match series with
        | Const { value; _ } -> Point_cell.const date value
        | Map { inner; f; _ } -> Point_cell.map (eval_query cache (Lazy.force inner) date) f
        | Convert { inner; f; _ } -> Point_cell.convert (eval_query cache (Lazy.force inner) date) f
      in
      Hashtbl.add cache series_id value;
      value

let query date series =
  let cache = Hashtbl.create 16 in
  Some (eval_query cache series date)

(*  Dependencies *)

let dependencies series =
  let rec aux visited series =
    if List.memq series visited then visited
    else
      let visited = series :: visited in
      match series with
      | Const _ -> visited
      | Map { inner; _ } | Convert { inner; _ } -> aux visited (Lazy.force inner)
  in
  aux [] series
