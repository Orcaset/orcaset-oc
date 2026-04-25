(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

type span = private
  | Value of { id : int; period : Period.t; value : unit -> float; split : split_span }
  | Map of { id : int; dep : span; f : float -> float }
  | Clip of { id : int; period : Period.t; dep : span; f : float -> float; split : split_span }
  | Map2 of {
      id : int;
      period : Period.t;
      a : span option;
      b : span option;
      f : float option -> float option -> float;
    }

and split_span = span -> Date.t -> span option * span option
(** [split_span span date] splits the span at [date]. If [date] is at or outside the span's bounds,
    one side should be [None] and the other should be the original span. *)

type point = private
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

(* Point operations *)
val p_const : Date.t -> (unit -> float) -> point
val p_map : point -> (float -> float) -> point
val p_derived : Date.t -> point option list -> (float option list -> float) -> point
val p_accum : Date.t -> float -> point option -> span option list -> point
val point_id : point -> int
val point_date : point -> Date.t
val point_value : point -> unit -> float

(* Span operations *)
val f_value : Period.t -> (unit -> float) -> split_span -> span
val f_map : span -> (float -> float) -> span
val f_clip : Period.t -> span -> (float -> float) -> split_span -> span

val f_map2 :
  Period.t -> span option -> span option -> (float option -> float option -> float) -> span

val span_id : span -> int
val span_period : span -> Period.t
val span_value : span -> unit -> float

val proportional_split : split_span
(** [proportional_split span date] splits the span's period at [date], assigning value
    proportionally. If [date] is at or outside the span's bounds, one side is [None] and the other
    is the original span. *)

val const_split : split_span
(** [const_split span date] splits the span's period at [date], assigning the same value as the
    original span to each side. If [date] is at or outside the span's bounds, one side is [None] and
    the other is the original span. *)

val split_span : Date.t -> span -> span option * span option
(** [split_span date span] splits [span] at [date] using its underlying split function. If [date] is
    at or outside the span's bounds, one side is [None] and the other is the original span. *)

val clip_span : Period.t -> span -> span option
(** [clip_span period span] clips [span] to the overlap with [period]. Returns [None] when there is
    no overlap. *)

val align_span_seq : span Seq.t -> span Seq.t -> (span option * span option) Seq.t
(** [align_span_seq a b] returns a sequence of span option tuples with aligned span periods. Any
    unmatched spans are paired with [None], splitting them as necessary to fill partial period
    overlaps. *)
