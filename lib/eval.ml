(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

open Cell_types

type cell = Cell_types.cell =
  | PeriodCell : 'c Cell_types.period_cell -> cell
  | PointCell : 'c Cell_types.point_cell -> cell

(* Cache dispatch helpers *)

let find_in_cache cache = function
  | PeriodCell pc ->
      Cell_cache.find_period cache (Period_cell.id pc) (Period_cell.period pc) |> Option.map snd
  | PointCell tc -> Cell_cache.find_point cache (Point_cell.id tc)

let store_in_cache cache status = function
  | PeriodCell pc ->
      Cell_cache.store_period cache (Period_cell.id pc) (Period_cell.period pc) status
  | PointCell tc -> Cell_cache.store_point cache (Point_cell.id tc) status

(* Priming *)

let rec prime_tree cache cell =
  match find_in_cache cache cell with
  | Some _ -> ()
  | None -> (
      match cell with
      | PeriodCell pc -> (
          let store status =
            Cell_cache.store_period cache (Period_cell.id pc) (Period_cell.period pc) status
          in
          match pc with
          | RConst { f; _ } -> store (Cell_cache.Resolved (f ()))
          | RRef { cell = None; _ } ->
              failwith "prime: Ref cell with no resolved dependency reached during priming"
          | RRef { cell = Some inner; _ } ->
              store (Cell_cache.Unresolved (0.0, 0));
              prime_tree cache (PeriodCell inner)
          | RDeps { deps; _ } ->
              store (Cell_cache.Unresolved (0.0, 0));
              List.iter (prime_tree cache) deps
          | RMap { inner; _ } ->
              store (Cell_cache.Unresolved (0.0, 0));
              prime_tree cache (PeriodCell inner)
          | RConvert { inner; _ } ->
              store (Cell_cache.Unresolved (0.0, 0));
              prime_tree cache (PeriodCell inner)
          | RMap2 { c1; c2; _ } ->
              store (Cell_cache.Unresolved (0.0, 0));
              List.iter
                (fun dep -> prime_tree cache (PeriodCell dep))
                (List.filter_map Fun.id [ c1; c2 ]))
      | PointCell tc -> (
          let store status = Cell_cache.store_point cache (Point_cell.id tc) status in
          match tc with
          | TConst { value; _ } -> store (Cell_cache.Resolved value)
          | TMap { inner; _ } ->
              store (Cell_cache.Unresolved (0.0, 0));
              prime_tree cache (PointCell inner)
          | TConvert { inner; _ } ->
              store (Cell_cache.Unresolved (0.0, 0));
              prime_tree cache (PointCell inner)
          | TDep2 { c1; c2; _ } ->
              store (Cell_cache.Unresolved (0.0, 0));
              List.iter
                (fun dep -> prime_tree cache (PointCell dep))
                (List.filter_map Fun.id [ c1; c2 ])
          | TAccum { changes; _ } ->
              store (Cell_cache.Unresolved (0.0, 0));
              Seq.iter (fun dep -> prime_tree cache (PeriodCell dep)) changes))

(* Evaluation *)

let rec eval_cell cache iteration cell =
  match find_in_cache cache cell with
  | Some (Cell_cache.Resolved v) -> (v, 0.0)
  | Some (Cell_cache.Unresolved (v, last_iter)) when last_iter = iteration -> (v, 0.0)
  | Some (Cell_cache.Unresolved (old_v, _)) ->
      store_in_cache cache (Cell_cache.Unresolved (old_v, iteration)) cell;
      let new_v, child_delta = compute_value cache iteration cell in
      let delta = Float.abs (new_v -. old_v) in
      let max_delta = Float.max delta child_delta in
      store_in_cache cache (Cell_cache.Unresolved (new_v, iteration)) cell;
      (new_v, max_delta)
  | None ->
      let new_v, child_delta = compute_value cache iteration cell in
      store_in_cache cache (Cell_cache.Resolved new_v) cell;
      (new_v, child_delta)

and compute_value cache iteration = function
  | PeriodCell pc -> (
      match pc with
      | RConst { f; _ } -> (f (), 0.0)
      | RDeps { deps; f; _ } ->
          let values, max_delta =
            List.fold_left
              (fun (vals, acc_delta) dep ->
                let v, d = eval_cell cache iteration dep in
                (v :: vals, Float.max acc_delta d))
              ([], 0.0) deps
          in
          (f (List.rev values), max_delta)
      | RMap { inner; f; _ } ->
          let v, d = eval_cell cache iteration (PeriodCell inner) in
          (f v, d)
      | RConvert { inner; f; _ } ->
          let v, d = eval_cell cache iteration (PeriodCell inner) in
          (f (Period_cell.period inner) v, d)
      | RMap2 { c1; c2; f; _ } ->
          let v1, d1 =
            match c1 with
            | None -> (None, 0.0)
            | Some c ->
                let v, d = eval_cell cache iteration (PeriodCell c) in
                (Some v, d)
          in
          let v2, d2 =
            match c2 with
            | None -> (None, 0.0)
            | Some c ->
                let v, d = eval_cell cache iteration (PeriodCell c) in
                (Some v, d)
          in
          (f v1 v2, Float.max d1 d2)
      | RRef { cell = Some c; _ } -> eval_cell cache iteration (PeriodCell c)
      | RRef { cell = None; _ } -> (0.0, 0.0))
  | PointCell tc -> (
      match tc with
      | TConst { value; _ } -> (value, 0.0)
      | TMap { inner; f; _ } ->
          let v, d = eval_cell cache iteration (PointCell inner) in
          (f v, d)
      | TConvert { inner; f; _ } ->
          let v, d = eval_cell cache iteration (PointCell inner) in
          (f (Point_cell.date inner) v, d)
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
          (f total_accum, max_delta))

(* Iteration *)

let resolve_pass cache roots iteration =
  List.fold_left
    (fun acc_delta root ->
      let _, d = eval_cell cache iteration root in
      Float.max acc_delta d)
    0.0 roots

let rec iterate cache roots iteration =
  if iteration > Cell_cache.max_iterations then ()
  else
    let delta = resolve_pass cache roots iteration in
    if delta < Cell_cache.convergence_threshold then () else iterate cache roots (iteration + 1)

(* Result extraction *)

let read_result cache cell =
  match find_in_cache cache cell with
  | Some (Cell_cache.Resolved v) -> v
  | Some (Cell_cache.Unresolved (v, _)) -> v
  | None -> failwith "Solver.read_result: cell not found in cache after iteration"

(* Public API *)

let one cell =
  let cache = Cell_cache.create () in
  prime_tree cache cell;
  iterate cache [ cell ] 1;
  read_result cache cell

let many groups =
  let cache = Cell_cache.create () in
  let all_cells = List.concat groups in
  List.iter (prime_tree cache) all_cells;
  iterate cache all_cells 1;
  List.map (List.map (read_result cache)) groups
