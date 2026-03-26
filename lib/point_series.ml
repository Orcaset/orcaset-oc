(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

open Series_types

type 'c t = 'c point_series

(*  Constructors *)
let const value = TConst { id = fresh_id (); value }
let map f s = TMap { id = fresh_id (); inner = s; f }
let convert f s = TConvert { id = fresh_id (); inner = (Obj.magic s : _ t Lazy.t); f }

let accum ~start_date ~initial_value changes =
  TAccum { id = fresh_id (); start_date; initial_value; changes }

(*  Accessors *)
let id = function TConst { id; _ } -> id | TMap { id; _ } -> id | TConvert { id; _ } -> id | TAccum { id; _ } -> id

(*  Evaluation *)

let rec eval_query ~eval_period cache series date =
  let series_id = id series in
  match Hashtbl.find_opt cache.point (series_id, date) with
  | Some value -> Some value
  | None -> (
      let value =
        match series with
        | TConst { value; _ } -> Some (Point_cell.const date value)
        | TMap { inner; f; _ } -> (
            let inner_cell = eval_query ~eval_period cache (Lazy.force inner) date in
            match inner_cell with None -> None | Some cell -> Some (Point_cell.map cell f))
        | TConvert { inner; f; _ } -> (
            let inner_cell = eval_query ~eval_period cache (Lazy.force inner) date in
            match inner_cell with None -> None | Some cell -> Some (Point_cell.convert cell f))
        | TAccum { start_date; initial_value; changes; _ } ->
            if Date.compare date start_date <= 0 then
              Some (Point_cell.const date initial_value)
            else
              let change_series = Lazy.force changes in
              let query_period = Period.make start_date date in
              let cells = eval_period cache change_series query_period in
              Some (Point_cell.accum cells date (fun total -> initial_value +. total))
      in
      match value with
      | None -> value
      | Some cell ->
          Hashtbl.add cache.point (series_id, date) cell;
          value)
