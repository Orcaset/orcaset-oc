(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

type split_part = private { period : Period.t; value : float -> float }
(** One side of an interior span split. The function computes the split-side value from the parent
    span's value. *)

type span = private
  | Value of { id : int; period : Period.t; value : float; split : split_strategy }
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
      f : float option -> float option -> float option;
    }
  | MapN of {
      id : int;
      period : Period.t;
      deps : span option list;
      f : float option list -> float option;
    }

and split_strategy = span -> Date.t -> split_part * split_part
(** A split strategy for interior split dates. It returns value projections rather than spans so
    {!split_span} can wrap each side as a cell that explicitly depends on its parent. *)

val split_part : period:Period.t -> value:(float -> float) -> split_part
(** [split_part ~period ~value] creates one side of an interior split. *)

type point = private
  | Const of { id : int; date : Date.t; value : float }
  | Map of { id : int; dep : point; f : float -> float }
  | Derived of {
      id : int;
      date : Date.t;
      deps : point option list;
      f : float option list -> float option;
    }
  | Accum of {
      id : int;
      date : Date.t;
      init : float;
      base : point option;
      delta : span option list;
    }

type _ cell = Point_cell : point -> point cell | Span_cell : span -> span cell

(* Point operations *)
val p_const : Date.t -> float -> point
val p_map : point -> (float -> float) -> point
val p_derived : Date.t -> point option list -> (float option list -> float option) -> point
val p_accum : Date.t -> float -> point option -> span option list -> point
val point_date : point -> Date.t
val point_id : point -> int

(* Span operations *)
val f_value : Period.t -> float -> split_strategy -> span
val f_map : span -> (float -> float) -> span

val f_map2 :
  Period.t -> span option -> span option -> (float option -> float option -> float option) -> span

val f_mapn : Period.t -> span option list -> (float option list -> float option) -> span
val span_period : span -> Period.t
val span_id : span -> int

val proportional_split : split_strategy
(** [proportional_split span date] splits the span's period at [date], assigning value
    proportionally. *)

val const_split : split_strategy
(** [const_split span date] splits the span's period at [date], assigning the same value as the
    original span to each side. *)

val split_span : Date.t -> span -> span option * span option
(** [split_span date span] splits [span] at [date]. Stored values, slices, and unary maps use a
    split strategy to allocate the parent value; multi-input maps split their inputs first and
    rebuild the mapped cell for each side. If [date] is at or outside the span's bounds, one side is
    [None] and the other is the original span. *)

val clip_span : Period.t -> span -> span option
(** [clip_span period span] clips [span] to the overlap with [period]. Returns [None] when there is
    no overlap. *)

val align_span_seq : span Seq.t -> span Seq.t -> (span option * span option) Seq.t
(** [align_span_seq a b] returns a sequence of span option tuples with aligned span periods. Any
    unmatched spans are paired with [None], splitting them as necessary to fill partial period
    overlaps. *)

val align_span_seqs : span Seq.t list -> span option list Seq.t
(** [align_span_seqs seqs] is the n-ary generalization of {!align_span_seq}. It emits a sequence of
    rows, where each row is a [span option list] of length [List.length seqs] aligned to a common
    output period. Streams that do not cover a given output period contribute [None]; partial
    overlaps are handled by splitting at row boundaries. Each emitted row contains at least one
    [Some] entry. *)
