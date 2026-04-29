(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

type split_part = { period : Period.t; value : float -> float }

type span =
  | Value of { id : int; period : Period.t; value : unit -> float; split : split_strategy }
  | Slice of {
      id : int;
      period : Period.t;
      dep : span;
      value : float -> float;
      split : split_strategy;
    }
  | Map of { id : int; dep : span; f : float -> float }
  | Map2 of {
      id : int;
      period : Period.t;
      a : span option;
      b : span option;
      f : float option -> float option -> float;
    }

and split_strategy = span -> Date.t -> split_part * split_part

let split_part ~period ~value = { period; value }

type point =
  | Const of { id : int; date : Date.t; value : unit -> float }
  | Map of { id : int; dep : point; f : float -> float }
  | Derived of { id : int; date : Date.t; deps : point option list; f : float option list -> float }
  | Accum of {
      id : int;
      date : Date.t;
      init : float;
      base : point option;
      delta : span option list;
    }

type _ cell = Point_cell : point -> point cell | Span_cell : span -> span cell

let next_id = Atomic.make 0
let fresh_id () = Atomic.fetch_and_add next_id 1

(* Point operations *)
let p_const date value : point = Const { id = fresh_id (); date; value }
let p_map point f : point = Map { id = fresh_id (); dep = point; f }
let p_derived date deps f = Derived { id = fresh_id (); date; deps; f }
let p_accum date init base delta = Accum { id = fresh_id (); date; init; base; delta }

let rec point_date = function
  | Const { date; _ } -> date
  | Map { dep; _ } -> point_date dep
  | Derived { date; _ } -> date
  | Accum { date; _ } -> date

let point_id = function
  | Const { id; _ } -> id
  | Map { id; _ } -> id
  | Derived { id; _ } -> id
  | Accum { id; _ } -> id

(* Span operations *)
let f_value period value split = Value { id = fresh_id (); period; value; split }
let f_slice period dep value split = Slice { id = fresh_id (); period; dep; value; split }
let f_map span f : span = Map { id = fresh_id (); dep = span; f }
let f_map2 period a b f = Map2 { id = fresh_id (); period; a; b; f }

let rec span_period = function
  | Value { period; _ } -> period
  | Slice { period; _ } -> period
  | Map { dep; _ } -> span_period dep
  | Map2 { period; _ } -> period

let span_id = function
  | Value { id; _ } -> id
  | Slice { id; _ } -> id
  | Map { id; _ } -> id
  | Map2 { id; _ } -> id

let split_with_strategy date span split =
  let left, right = split span date in
  ( Some (f_slice left.period span left.value split),
    Some (f_slice right.period span right.value split) )

let rec split_span date span =
  let start, end_ = Period.to_tuple (span_period span) in
  if Date.(date <= start) then (None, Some span)
  else if Date.(date >= end_) then (Some span, None)
  else
    match span with
    | Value { split; _ } | Slice { split; _ } -> split_with_strategy date span split
    | Map { dep; f; _ } -> (
        match split_span date dep with
        | Some left, Some right -> (Some (f_map left f), Some (f_map right f))
        | Some left, None -> (Some (f_map left f), None)
        | None, Some right -> (None, Some (f_map right f))
        | None, None -> (None, None))
    | Map2 { a; b; f; _ } -> (
        let left_period = Period.make start date in
        let right_period = Period.make date end_ in
        match (a, b) with
        | None, None -> (None, None)
        | Some a, None ->
            let a_left, a_right = split_span date a in
            (Some (f_map2 left_period a_left None f), Some (f_map2 right_period a_right None f))
        | None, Some b ->
            let b_left, b_right = split_span date b in
            (Some (f_map2 left_period None b_left f), Some (f_map2 right_period None b_right f))
        | Some a, Some b ->
            let a_left, a_right = split_span date a in
            let b_left, b_right = split_span date b in
            (Some (f_map2 left_period a_left b_left f), Some (f_map2 right_period a_right b_right f))
        )

let proportional_split : split_strategy =
 fun span date ->
  let start, end_ = Period.to_tuple (span_period span) in
  let numerator = Float.of_int (Date.diff date start) in
  let denominator = Float.of_int (Date.diff end_ start) in
  let left_ratio = numerator /. denominator in
  let right_ratio = 1.0 -. left_ratio in
  ( split_part ~period:(Period.make start date) ~value:(fun value -> value *. left_ratio),
    split_part ~period:(Period.make date end_) ~value:(fun value -> value *. right_ratio) )

let const_split : split_strategy =
 fun span date ->
  let start, end_ = Period.to_tuple (span_period span) in
  ( split_part ~period:(Period.make start date) ~value:Fun.id,
    split_part ~period:(Period.make date end_) ~value:Fun.id )

let clip_span bounds span =
  let q_start, q_end = Period.to_tuple bounds in
  let f_start, f_end = Period.to_tuple (span_period span) in
  if Date.(f_end <= q_start) || Date.(f_start >= q_end) then None
  else
    let span =
      if Date.(f_start < q_start) then
        match split_span q_start span with _, Some right -> right | _, None -> span
      else span
    in
    let span =
      let f_end = Period.end_ (span_period span) in
      if Date.(f_end > q_end) then
        match split_span q_end span with Some left, _ -> left | None, _ -> span
      else span
    in
    Some span

let rec align_span_seq a b =
 fun () ->
  let a_cons = Seq.uncons a in
  let b_cons = Seq.uncons b in
  match (a_cons, b_cons) with
  | None, None -> Seq.Nil
  | Some _, None -> Seq.map (fun cell -> (Some cell, None)) a ()
  | None, Some _ -> Seq.map (fun cell -> (None, Some cell)) b ()
  | Some (a_head, a_tail), Some (b_head, b_tail) -> (
      let a_start, a_end = Period.to_tuple (span_period a_head) in
      let b_start, b_end = Period.to_tuple (span_period b_head) in
      (* Case 1: Periods are identical *)
      if Date.equal a_start b_start && Date.equal a_end b_end then
        Seq.Cons ((Some a_head, Some b_head), align_span_seq a_tail b_tail)
        (* Case 2: a is entirely before b (no overlap) *)
      else if Date.(a_end <= b_start) then Seq.Cons ((Some a_head, None), align_span_seq a_tail b)
        (* Case 3: b is entirely before a (no overlap) *)
      else if Date.(b_end <= a_start) then Seq.Cons ((None, Some b_head), align_span_seq a b_tail)
        (* Case 4: a starts before b — split a at b_start, emit a's prefix
          unpaired, then recurse with the remainder (starts now aligned) *)
      else if Date.(a_start < b_start) then
        let a_left, a_right = split_span b_start a_head in
        match a_right with
        | None -> Seq.Cons ((a_left, None), align_span_seq a_tail b)
        | Some a_right -> Seq.Cons ((a_left, None), align_span_seq (Seq.cons a_right a_tail) b)
        (* Case 5: b starts before a — mirror of case 4 *)
      else if Date.(b_start < a_start) then
        let b_left, b_right = split_span a_start b_head in
        match b_right with
        | None -> Seq.Cons ((None, b_left), align_span_seq a b_tail)
        | Some b_right -> Seq.Cons ((None, b_left), align_span_seq a (Seq.cons b_right b_tail))
        (* Cases 6–7: starts are equal but ends differ *)
      else if Date.(a_end < b_end) then
        (* a ends first — split b at a_end, pair a with b's prefix,
            then recurse with b's remainder *)
        let b_left, b_right = split_span a_end b_head in
        match b_right with
        | None -> Seq.Cons ((Some a_head, b_left), align_span_seq a_tail b_tail)
        | Some b_right ->
            Seq.Cons ((Some a_head, b_left), align_span_seq a_tail (Seq.cons b_right b_tail))
      else
        (* b ends first — split a at b_end, pair a's prefix with b,
            then recurse with a's remainder *)
        let a_left, a_right = split_span b_end a_head in
        match a_right with
        | None -> Seq.Cons ((a_left, Some b_head), align_span_seq a_tail b_tail)
        | Some a_right ->
            Seq.Cons ((a_left, Some b_head), align_span_seq (Seq.cons a_right a_tail) b_tail))
