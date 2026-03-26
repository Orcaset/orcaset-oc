(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

open Cell_types

type cell = Cell_types.cell = PeriodCell of period_cell | PointCell of point_cell

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
      | PeriodCell pc -> Period_cell.prime prime_tree cache pc
      | PointCell tc -> Point_cell.prime prime_tree cache tc)

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
  | PeriodCell pc -> Period_cell.compute eval_cell cache iteration pc
  | PointCell tc -> Point_cell.compute eval_cell cache iteration tc

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
