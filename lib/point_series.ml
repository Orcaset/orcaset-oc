(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

open Series_types

type 'c t = 'c point_series

(*  Constructors *)
let const value = TConst { id = fresh_id (); value }
let map f s = TMap { id = fresh_id (); inner = s; f }
let convert f s = TConvert { id = fresh_id (); inner = (Obj.magic s : _ t Lazy.t); f }

(*  Accessors *)
let id = function TConst { id; _ } -> id | TMap { id; _ } -> id | TConvert { id; _ } -> id

(*  Evaluation *)

let rec eval_query cache series date =
  let series_id = id series in
  match Hashtbl.find_opt cache.point series_id with
  | Some value -> value
  | None ->
      let value =
        match series with
        | TConst { value; _ } -> Point_cell.const date value
        | TMap { inner; f; _ } -> Point_cell.map (eval_query cache (Lazy.force inner) date) f
        | TConvert { inner; f; _ } ->
            Point_cell.convert (eval_query cache (Lazy.force inner) date) f
      in
      Hashtbl.add cache.point series_id value;
      value

