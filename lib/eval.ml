(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

open Cell_types

exception Non_convergence of { iterations : int; delta : float; threshold : float }

(* Cache dispatch helpers *)

let find_in_cache cache (cell : Cell_types.cell) =
  match cell with
  | PeriodCell pc ->
      Cell_cache.find_period cache (Period_cell.id pc) (Period_cell.period pc) |> Option.map snd
  | PointCell tc -> Cell_cache.find_point cache (Point_cell.id tc)

let store_in_cache cache status (cell : Cell_types.cell) =
  match cell with
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
          | RRef { state = Unresolved _ | Resolving; _ } ->
              failwith "prime: Ref cell with no resolved dependency reached during priming"
          | RRef { state = Resolved inner; _ } ->
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
                (List.filter_map Fun.id [ c1; c2 ])
          | RClip { inner; period; _ } -> (
              store (Cell_cache.Unresolved (0.0, 0));
              match inner with
              | RRef { state = Unresolved _ | Resolving; _ } -> ()
              | _ -> (
                  match Period_cell.expand_to_period inner period with
                  | Some expanded -> prime_tree cache (PeriodCell expanded)
                  | None -> prime_tree cache (PeriodCell inner))))
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
          | TDeps { deps; _ } ->
              store (Cell_cache.Unresolved (0.0, 0));
              List.iter (prime_tree cache) deps
          | TRef { cell = None; _ } ->
              failwith "prime: Ref cell with no resolved dependency reached during priming"
          | TRef { cell = Some inner; _ } ->
              store (Cell_cache.Unresolved (0.0, 0));
              prime_tree cache (PointCell inner)))

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
      | RClip { inner; period; _ } -> (
          match inner with
          | RRef { state = Unresolved _ | Resolving; _ } -> (0.0, 0.0)
          | _ -> (
              match Period_cell.expand_to_period inner period with
              | Some expanded -> eval_cell cache iteration (PeriodCell expanded)
              | None ->
                  let v, d = eval_cell cache iteration (PeriodCell inner) in
                  let scale =
                    float_of_int (Period.days period)
                    /. float_of_int (Period.days (Period_cell.period inner))
                  in
                  (v *. scale, d)))
      | RRef { state = Resolved c; _ } -> eval_cell cache iteration (PeriodCell c)
      | RRef { state = Unresolved _ | Resolving; _ } -> (0.0, 0.0))
  | PointCell tc -> (
      match tc with
      | TConst { value; _ } -> (value, 0.0)
      | TMap { inner; f; _ } ->
          let v, d = eval_cell cache iteration (PointCell inner) in
          (f v, d)
      | TConvert { inner; f; _ } ->
          let v, d = eval_cell cache iteration (PointCell inner) in
          (f (Point_cell.date inner) v, d)
      | TDeps { deps; f; _ } ->
          let values, max_delta =
            List.fold_left
              (fun (vals, acc_delta) dep ->
                let v, d = eval_cell cache iteration dep in
                (v :: vals, Float.max acc_delta d))
              ([], 0.0) deps
          in
          (f (List.rev values), max_delta)
      | TRef { cell = None; _ } -> (0.0, 0.0)
      | TRef { cell = Some c; _ } -> eval_cell cache iteration (PointCell c))

(* Iteration *)

let resolve_pass cache roots iteration =
  List.fold_left
    (fun acc_delta root ->
      let _, d = eval_cell cache iteration root in
      Float.max acc_delta d)
    0.0 roots

let rec iterate cache roots iteration =
  let delta = resolve_pass cache roots iteration in
  if delta < Cell_cache.convergence_threshold then ()
  else if iteration >= Cell_cache.max_iterations then
    raise
      (Non_convergence
         {
           iterations = Cell_cache.max_iterations;
           delta;
           threshold = Cell_cache.convergence_threshold;
         })
  else iterate cache roots (iteration + 1)

(* Result extraction *)

let read_result cache cell =
  match find_in_cache cache cell with
  | Some (Cell_cache.Resolved v) -> v
  | Some (Cell_cache.Unresolved (v, _)) -> v
  | None -> failwith "Solver.read_result: cell not found in cache after iteration"
