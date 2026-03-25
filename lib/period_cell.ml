(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

open Cell_types

type split_fn = Cell_types.split_fn

type split_result = Cell_types.split_result = {
  period : Period.t;
  f : unit -> float;
  split : split_fn;
}

type +'c t = Cell_types.period_cell

(* Constructors *)

let const period f split = RConst { id = Cell_types.fresh_id (); period; f; split }
let deps period deps f = RDeps { id = Cell_types.fresh_id (); period; deps; f }
let map inner f = RMap { id = Cell_types.fresh_id (); inner; f }
let convert inner f = RConvert { id = Cell_types.fresh_id (); inner; f }
let map2 c1 c2 f = RMap2 { id = Cell_types.fresh_id (); c1; c2; f }
let cell_ref period = RRef { id = Cell_types.fresh_id (); period; cell = None }

let set_ref ref_cell cell =
  match ref_cell with
  | RRef r -> r.cell <- Some cell
  | _ -> invalid_arg "Period_cell.set_ref: not a Ref cell"

(* Accessors *)

let id = function
  | RConst { id; _ }
  | RDeps { id; _ }
  | RMap { id; _ }
  | RConvert { id; _ }
  | RMap2 { id; _ }
  | RRef { id; _ } ->
      id

let rec period = function
  | RConst { period; _ } -> period
  | RDeps { period; _ } -> period
  | RMap { inner; _ } | RConvert { inner; _ } -> period inner
  | RMap2 { c1; c2; _ } -> (
      match (c1, c2) with
      | Some c, None -> period c
      | None, Some c -> period c
      | Some c1, Some c2 ->
          let period1 = period c1 in
          let period2 = period c2 in
          if Period.equal period1 period2 then period1
          else failwith "Map2 cells have mismatched periods"
      | None, None -> failwith "Map2 should have at least one non-None cell")
  | RRef { period; _ } -> period

(* Helpers for splitting cells *)
let rec split_cell cell date =
  let period = period cell in
  if Date.(date <= Period.start_date period || date >= Period.end_date period) then
    failwith "split_cell: split date is on or out of bounds for cell period";
  match cell with
  | RConst { id; split; period; f } ->
      let lr, rr = split period f date in
      ( RConst { id; period = lr.period; f = lr.f; split = lr.split },
        RConst { id; period = rr.period; f = rr.f; split = rr.split } )
  (* TODO: Consider whether just narrowing the period range is the right split behavior *)
  | RDeps { id; period; deps; f } ->
      let left_period = Period.make (Period.start_date period) date in
      let right_period = Period.make date (Period.end_date period) in
      (RDeps { id; period = left_period; deps; f }, RDeps { id; period = right_period; deps; f })
  | RMap { id; inner; f } ->
      let left, right = split_cell inner date in
      (RMap { id; inner = left; f }, RMap { id; inner = right; f })
  | RConvert { id; inner; f } ->
      let left, right = split_cell inner date in
      (RConvert { id; inner = left; f }, RConvert { id; inner = right; f })
  | RMap2 { id; c1; c2; f } ->
      let left1, right1 =
        match c1 with
        | None -> (None, None)
        | Some c ->
            let l, r = split_cell c date in
            (Some l, Some r)
      in
      let left2, right2 =
        match c2 with
        | None -> (None, None)
        | Some c ->
            let l, r = split_cell c date in
            (Some l, Some r)
      in
      ( RMap2 { id; c1 = left1; c2 = left2; f = (fun v1 v2 -> f v1 v2) },
        RMap2 { id; c1 = right1; c2 = right2; f = (fun v1 v2 -> f v1 v2) } )
  | RRef { cell = Some c; _ } -> split_cell c date
  | RRef { cell = None; _ } ->
      failwith "split_cell: cannot split Ref cell with unresolved dependency"

let rec clip cell c_period =
  let cell_period = period cell in
  if Period.start_date cell_period < Period.start_date c_period then
    let _, right = split_cell cell (Period.start_date c_period) in
    clip right c_period
  else if Period.end_date cell_period > Period.end_date c_period then
    let left, _ = split_cell cell (Period.end_date c_period) in
    left
  else cell

let rec proportional_split period value_fn split_date =
  let total_days = Period.days period in
  let left_days = Date.diff split_date (Period.start_date period) in
  let right_days = total_days - left_days in
  let left_value () = value_fn () *. (float_of_int left_days /. float_of_int total_days) in
  let right_value () = value_fn () *. (float_of_int right_days /. float_of_int total_days) in
  let left =
    {
      period = Period.make (Period.start_date period) split_date;
      f = left_value;
      split = proportional_split;
    }
  in
  let right =
    {
      period = Period.make split_date (Period.end_date period);
      f = right_value;
      split = proportional_split;
    }
  in
  (left, right)

(** Return a sequence of pairs of cells with overlapping periods. Pads beginning and ending for the
    shorter sequence with None. When periods partially overlap, cells are split so that each emitted
    pair covers an aligned sub-period.

    The body is wrapped in [fun () -> ...] so that [Seq.uncons] calls and recursive invocations are
    deferred until the consumer pulls the next element — without this, infinite input sequences
    would diverge. *)
let rec iter_period_union a b =
 fun () ->
  let a_cons = Seq.uncons a in
  let b_cons = Seq.uncons b in
  match (a_cons, b_cons) with
  | None, None -> Seq.Nil
  | Some _, None -> Seq.map (fun cell -> (Some cell, None)) a ()
  | None, Some _ -> Seq.map (fun cell -> (None, Some cell)) b ()
  | Some (a_head, a_tail), Some (b_head, b_tail) ->
      let a_start = Period.start_date (period a_head) in
      let a_end = Period.end_date (period a_head) in
      let b_start = Period.start_date (period b_head) in
      let b_end = Period.end_date (period b_head) in
      (* Case 1: Periods are identical *)
      if Date.equal a_start b_start && Date.equal a_end b_end then
        Seq.Cons ((Some a_head, Some b_head), iter_period_union a_tail b_tail)
        (* Case 2: a is entirely before b (no overlap) *)
      else if Date.(a_end <= b_start) then
        Seq.Cons ((Some a_head, None), iter_period_union a_tail b)
        (* Case 3: b is entirely before a (no overlap) *)
      else if Date.(b_end <= a_start) then
        Seq.Cons ((None, Some b_head), iter_period_union a b_tail)
        (* Case 4: a starts before b — split a at b_start, emit a's prefix
         unpaired, then recurse with the remainder (starts now aligned) *)
      else if Date.(a_start < b_start) then
        let a_left, a_right = split_cell a_head b_start in
        Seq.Cons ((Some a_left, None), iter_period_union (Seq.cons a_right a_tail) b)
        (* Case 5: b starts before a — mirror of case 4 *)
      else if Date.(b_start < a_start) then
        let b_left, b_right = split_cell b_head a_start in
        Seq.Cons ((None, Some b_left), iter_period_union a (Seq.cons b_right b_tail))
        (* Cases 6–7: starts are equal but ends differ *)
      else if Date.(a_end < b_end) then
        (* a ends first — split b at a_end, pair a with b's prefix,
           then recurse with b's remainder *)
        let b_left, b_right = split_cell b_head a_end in
        Seq.Cons ((Some a_head, Some b_left), iter_period_union a_tail (Seq.cons b_right b_tail))
      else
        (* b ends first — split a at b_end, pair a's prefix with b,
           then recurse with a's remainder *)
        let a_left, a_right = split_cell a_head b_end in
        Seq.Cons ((Some a_left, Some b_head), iter_period_union (Seq.cons a_right a_tail) b_tail)

(* Priming *)

let prime (prime_tree : Cell_types.prime_fn) cache cell =
  let store status = Cell_cache.store_period cache (id cell) (period cell) status in
  match cell with
  | RConst { f; _ } -> store (Cell_cache.Resolved (f ()))
  | RRef { cell = None; _ } ->
      failwith "prime: Ref cell with no resolved dependency reached during priming"
  | RRef { cell = Some inner; _ } ->
      store (Cell_cache.Unresolved (0.0, 0));
      prime_tree cache (PeriodCell inner)
  | RDeps { deps; _ } ->
      store (Cell_cache.Unresolved (0.0, 0));
      List.iter (fun dep -> prime_tree cache (PeriodCell dep)) deps
  | RMap { inner; _ } | RConvert { inner; _ } ->
      store (Cell_cache.Unresolved (0.0, 0));
      prime_tree cache (PeriodCell inner)
  | RMap2 { c1; c2; _ } ->
      store (Cell_cache.Unresolved (0.0, 0));
      List.iter (fun dep -> prime_tree cache (PeriodCell dep)) (List.filter_map Fun.id [ c1; c2 ])

(* Evaluation *)

let compute (eval_cell : Cell_types.eval_fn) cache iteration = function
  | RConst { f; _ } -> (f (), 0.0)
  | RDeps { deps; f; _ } ->
      let values, max_delta =
        List.fold_left
          (fun (vals, acc_delta) dep ->
            let v, d = eval_cell cache iteration (PeriodCell dep) in
            (v :: vals, Float.max acc_delta d))
          ([], 0.0) deps
      in
      (f (List.rev values), max_delta)
  | RMap { inner; f; _ } ->
      let v, d = eval_cell cache iteration (PeriodCell inner) in
      (f v, d)
  | RConvert { inner; f; _ } ->
      let v, d = eval_cell cache iteration (PeriodCell inner) in
      (f (period inner) v, d)
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
  | RRef { cell = None; _ } -> (0.0, 0.0)

(** A tree representing the dependency structure of a cell. [Cycle] marks a back-edge to a cell that
    was already visited on the current path, preventing infinite recursion when circular
    dependencies exist. *)
type 'c dep_tree = Leaf of 'c t | Node of 'c t * 'c dep_tree list | Cycle of 'c t

(** Return the dependency tree rooted at [cell]. Circular dependencies are detected via physical
    identity ([==]) on a visited set and represented as [Cycle] nodes rather than recursing
    infinitely. *)
let dependency_tree cell =
  let rec go visited c =
    if List.exists (fun v -> v == c) visited then Cycle c
    else
      let visited = c :: visited in
      match c with
      | RConst _ -> Leaf c
      | RDeps { deps; _ } -> Node (c, List.map (go visited) deps)
      | RMap { inner; _ } | RConvert { inner; _ } -> Node (c, [ go visited inner ])
      | RMap2 { c1; c2; _ } ->
          let children = List.filter_map (fun opt -> Option.map (go visited) opt) [ c1; c2 ] in
          Node (c, children)
      | RRef { cell = Some inner; _ } -> Node (c, [ go visited inner ])
      | RRef { cell = None; _ } -> Leaf c
  in
  go [] cell
