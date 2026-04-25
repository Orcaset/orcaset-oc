(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Contiguous time intervals.

    A period is an ordered pair of dates representing a time interval interpreted as
    [\[start, end_)] (start-inclusive, end-exclusive). This ensures contiguous periods produced by
    {!make_seq} tile without overlap. The length in days is [end_ - start], which is zero when both
    dates are equal and negative when [end_] precedes [start] (no validation is performed).

    {1 Periods} *)

type t
(** The type for time periods. *)

val make : Date.t -> Date.t -> t
(** [make start end_] is a period from [start] to [end_].

    {b Note.} No validation is performed on date ordering. *)

(** {1 Accessors} *)

val start : t -> Date.t
(** [start p] is [p]'s start date. *)

val end_ : t -> Date.t
(** [end_ p] is [p]'s end date. *)

val days : t -> int
(** [days p] is the number of calendar days in [p], i.e. {!Date.diff}[ (end_ p) (start p)]. *)

val to_tuple : t -> Date.t * Date.t
(** [to_tuple p] is [(start p, end_ p)]. *)

val contains : Date.t -> t -> bool
(** [contains d p] is [true] iff [start p <= d] and [d < end_ p]. Start-inclusive, end-exclusive. *)

(** {1 Shifting} *)

val next : Offset.t -> t -> t
(** [next offset p] is a new period with the dates [Period.end_ p] and
    [Date.shift offset (Period.end_ p)]. *)

val prev : Offset.t -> t -> t
(** [prev offset p] is a new period with the dates [Date.shift (Period.start p)] and
    [Period.start p]. *)

val shift : Offset.t -> t -> t
(** [shift offset p] is a new period with both start and end dates shifted by [offset]. *)

(** {1 Sequences} *)

val make_seq : start:Date.t -> offset:Offset.t -> t Seq.t
(** [make_seq ~start ~offset] is an infinite sequence of contiguous periods. The first period starts
    at [start] and ends at [Date.shift offset start]. Each subsequent period starts where the
    previous one ended. *)

val seq_to_dates : t Seq.t -> Date.t Seq.t
(** [seq_to_dates periods] is a sequence of dates in [periods] with equal adjacent start/end dates
    deduplicated. *)

(** {1 Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal p0 p1] is [true] iff both endpoints are equal. *)

val hash : t -> int
(** [hash p] is a hash of the period [p], derived from the hashes of its start and end dates. *)

(** {1:fmt Formatting} *)

val to_string : t -> string
(** [to_string p] is [p] formatted as ["YYYY-MM-DD..YYYY-MM-DD"]. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a period with {!to_string}. *)
