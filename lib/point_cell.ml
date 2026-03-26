(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

open Cell_types

type +'c t = Cell_types.point_cell

let const date value = TConst { id = Cell_types.fresh_id (); date; value }
let map inner f = TMap { id = Cell_types.fresh_id (); inner; f }
let convert inner f = TConvert { id = Cell_types.fresh_id (); inner; f }
let dep2 c1 c2 date f = TDep2 { id = Cell_types.fresh_id (); date; c1; c2; f }
let accum changes date f = TAccum { id = Cell_types.fresh_id (); date; changes; f }

(* Accessors *)

let id = function
  | TConst { id; _ } | TMap { id; _ } | TConvert { id; _ } | TDep2 { id; _ } | TAccum { id; _ } ->
      id

let rec date = function
  | TConst { date; _ } | TDep2 { date; _ } | TAccum { date; _ } -> date
  | TMap { inner; _ } | TConvert { inner; _ } -> date inner

(* Priming *)

let prime (prime_tree : Cell_types.prime_fn) cache cell =
  let store status = Cell_cache.store_point cache (id cell) status in
  match cell with
  | TConst { value; _ } -> store (Cell_cache.Resolved value)
  | TMap { inner; _ } | TConvert { inner; _ } ->
      store (Cell_cache.Unresolved (0.0, 0));
      prime_tree cache (PointCell inner)
  | TDep2 { c1; c2; _ } ->
      store (Cell_cache.Unresolved (0.0, 0));
      List.iter (fun dep -> prime_tree cache (PointCell dep)) (List.filter_map Fun.id [ c1; c2 ])
  | TAccum { changes; _ } ->
      store (Cell_cache.Unresolved (0.0, 0));
      Seq.iter (fun dep -> prime_tree cache (PeriodCell dep)) changes

(* Evaluation *)

let compute (eval_cell : Cell_types.eval_fn) cache iteration = function
  | TConst { value; _ } -> (value, 0.0)
  | TMap { inner; f; _ } ->
      let v, d = eval_cell cache iteration (PointCell inner) in
      (f v, d)
  | TConvert { inner; f; _ } ->
      let v, d = eval_cell cache iteration (PointCell inner) in
      (f (date inner) v, d)
  | TDep2 { c1; c2; f; _ } ->
      let v1, d1 =
        match c1 with
        | None -> (None, 0.0)
        | Some c ->
            let v, d = eval_cell cache iteration (PointCell c) in
            (Some v, d)
      in
      let v2, d2 =
        match c2 with
        | None -> (None, 0.0)
        | Some c ->
            let v, d = eval_cell cache iteration (PointCell c) in
            (Some v, d)
      in
      (f v1 v2, Float.max d1 d2)
  | TAccum { changes; f; _ } ->
      let total_accum, max_delta =
        Seq.fold_left
          (fun (acc_v, acc_d) change ->
            let v, d = eval_cell cache iteration (PeriodCell change) in
            (acc_v +. v, Float.max acc_d d))
          (0.0, 0.0) changes
      in
      (f total_accum, max_delta)
