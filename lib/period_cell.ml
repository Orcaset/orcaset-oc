(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

open Cell_types

type split_fn = Cell_types.split_fn

type split_result = Cell_types.split_result = {
  period : Period.t;
  f : unit -> float;
  split : split_fn;
}

type 'c t = 'c Cell_types.period_cell

(* Constructors *)

let const period f split = RConst { id = Cell_types.fresh_id (); period; f; split }
let map inner f = RMap { id = Cell_types.fresh_id (); inner; f }
let convert inner f = RConvert { id = Cell_types.fresh_id (); inner; f }
let map2 c1 c2 f = RMap2 { id = Cell_types.fresh_id (); c1; c2; f }
let cell_ref ?resolver period =
  RRef { id = Cell_types.fresh_id (); period; state = Unresolved resolver }

let set_ref ref_cell cell =
  match ref_cell with
  | RRef r -> r.state <- Resolved cell
  | _ -> invalid_arg "Period_cell.set_ref: not a Ref cell"

let ensure_resolved : type c. c t -> unit = function
  | RRef ({ state = Resolved _; _ } | { state = Resolving; _ }) -> ()
  | RRef ({ state = Unresolved None; _ }) -> ()
  | RRef ({ state = Unresolved (Some resolve); _ } as r) ->
      r.state <- Resolving;
      let resolved =
        try resolve ()
        with exn ->
          r.state <- Unresolved (Some resolve);
          raise exn
      in
      r.state <- Resolved resolved
  | _ -> ()

(* Accessors *)

let id = function
  | RConst { id; _ } -> id
  | RDeps { id; _ } -> id
  | RMap { id; _ } -> id
  | RConvert { id; _ } -> id
  | RMap2 { id; _ } -> id
  | RClip { id; _ } -> id
  | RRef { id; _ } -> id

let rec period : type c. c t -> Period.t = function
  | RConst { period; _ } -> period
  | RDeps { period; _ } -> period
  | RMap { inner; _ } -> period inner
  | RConvert { inner; _ } -> period inner
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
  | RClip { period; _ } -> period
  | RRef { period; _ } -> period

(* Helpers for splitting cells *)
let clipped_period cell target =
  let cell_period = period cell in
  let start_date =
    if Date.compare (Period.start_date cell_period) (Period.start_date target) >= 0 then
      Period.start_date cell_period
    else Period.start_date target
  in
  let end_date =
    if Date.compare (Period.end_date cell_period) (Period.end_date target) <= 0 then
      Period.end_date cell_period
    else Period.end_date target
  in
  if Date.compare start_date end_date >= 0 then invalid_arg "clip: periods do not overlap";
  Period.make start_date end_date

let rec split_cell : type c. c t -> Date.t -> c t * c t =
 fun cell date ->
  let period = period cell in
  if Date.(date <= Period.start_date period || date >= Period.end_date period) then
    failwith "split_cell: split date is on or out of bounds for cell period";
  match cell with
  | RConst { id; split; period; f } ->
      let lr, rr = split period f date in
      ( RConst { id; period = lr.period; f = lr.f; split = lr.split },
        RConst { id; period = rr.period; f = rr.f; split = rr.split } )
  | RDeps _ ->
      let left_period = Period.make (Period.start_date period) date in
      let right_period = Period.make date (Period.end_date period) in
      ( RClip { id = fresh_id (); inner = cell; period = left_period },
        RClip { id = fresh_id (); inner = cell; period = right_period } )
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
  | RClip { id; inner; _ } ->
      let left_period = Period.make (Period.start_date period) date in
      let right_period = Period.make date (Period.end_date period) in
      (RClip { id; inner; period = left_period }, RClip { id; inner; period = right_period })
  | RRef _ ->
      let left_period = Period.make (Period.start_date period) date in
      let right_period = Period.make date (Period.end_date period) in
      ( RClip { id = id cell; inner = cell; period = left_period },
        RClip { id = id cell; inner = cell; period = right_period } )

and expand_to_period : type c. c t -> Period.t -> c t option =
 fun cell target_period ->
  let target_period = clipped_period cell target_period in
  if Period.equal (period cell) target_period then Some cell
  else
    match cell with
    | RConst _ ->
        let rec trim_const current =
          let current_period = period current in
          if Period.equal current_period target_period then current
          else if Period.start_date current_period < Period.start_date target_period then
            let _, right = split_cell current (Period.start_date target_period) in
            trim_const right
          else
            let left, _ = split_cell current (Period.end_date target_period) in
            trim_const left
        in
        Some (trim_const cell)
    | RDeps _ -> None
    | RMap { id; inner; f } ->
        expand_to_period inner target_period |> Option.map (fun inner -> RMap { id; inner; f })
    | RConvert { id; inner; f } ->
        expand_to_period inner target_period |> Option.map (fun inner -> RConvert { id; inner; f })
    | RMap2 { id; c1; c2; f } -> (
        let clipped_opt = function
          | None -> Some None
          | Some c -> expand_to_period c target_period |> Option.map Option.some
        in
        match (clipped_opt c1, clipped_opt c2) with
        | Some c1, Some c2 -> Some (RMap2 { id; c1; c2; f })
        | _ -> None)
    | RClip { inner; _ } -> expand_to_period inner target_period
    | RRef { state = Resolved inner; _ } -> expand_to_period inner target_period
    | RRef { state = Unresolved _ | Resolving; _ } -> None

and clip cell c_period =
  let period = clipped_period cell c_period in
  match expand_to_period cell period with
  | Some clipped -> clipped
  | None -> RClip { id = id cell; inner = cell; period }

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
