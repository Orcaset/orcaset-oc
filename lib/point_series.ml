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
  | Some value -> Some value
  | None -> (
      let value =
        match series with
        | TConst { value; _ } -> Some (Point_cell.const date value)
        | TMap { inner; f; _ } -> (
            let inner_cell = eval_query cache (Lazy.force inner) date in
            match inner_cell with None -> None | Some cell -> Some (Point_cell.map cell f))
        | TConvert { inner; f; _ } -> (
            let inner_cell = eval_query cache (Lazy.force inner) date in
            match inner_cell with None -> None | Some cell -> Some (Point_cell.convert cell f))
      in
      match value with
      | None -> value
      | Some cell ->
          Hashtbl.add cache.point series_id cell;
          value)
