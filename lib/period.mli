(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Contiguous time intervals.

    A period is an ordered pair of dates representing a time interval interpreted as
    [\[start_date, end_date)] (start-inclusive, end-exclusive). This ensures contiguous periods
    produced by {!make_seq} tile without overlap. The length in days is [end_date - start_date],
    which is zero when both dates are equal and negative when [end_date] precedes [start_date] (no
    validation is performed).

    {1 Periods} *)

type t
(** The type for time periods. *)

val make : Date.t -> Date.t -> t
(** [make start_date end_date] is a period from [start_date] to [end_date].

    {b Note.} No validation is performed on date ordering. *)

(** {1:accessors Accessors} *)

val start_date : t -> Date.t
(** [start_date p] is [p]'s start date. *)

val end_date : t -> Date.t
(** [end_date p] is [p]'s end date. *)

val days : t -> int
(** [days p] is the number of calendar days in [p], i.e. {!Date.diff}[ (end_date p) (start_date p)].
*)

val to_tuple : t -> Date.t * Date.t
(** [to_tuple p] is [(start_date p, end_date p)]. *)

val contains : Date.t -> t -> bool
(** [contains d p] is [true] iff [start_date p <= d] and [d < end_date p]. Start-inclusive,
    end-exclusive. *)

(** {1:shifting Shifting} *)

val shift : Offset.t -> t -> t
(** [shift offset p] is [p] with both endpoints advanced by [offset]. *)

(** {1:sequences Sequences} *)

val make_seq : start_date:Date.t -> offset:Offset.t -> t Seq.t
(** [make_seq ~start_date ~offset] is an infinite sequence of contiguous periods. The first period
    starts at [start_date] and ends at [Date.shift offset start_date]. Each subsequent period starts
    where the previous one ended. *)

val seq_to_dates : t Seq.t -> Date.t Seq.t
(** [seq_to_dates periods] is a sequence of dates in [periods] with equal adjacent start/end dates deduplicated. *)

(** {1:preds Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal p0 p1] is [true] iff both endpoints are equal. *)

val hash : t -> int
(** [hash p] is a hash of the period [p], derived from the hashes of its start and end dates. *)

(** {1:fmt Formatting} *)

val to_string : t -> string
(** [to_string p] is [p] formatted as ["YYYY-MM-DD..YYYY-MM-DD"]. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a period with {!to_string}. *)
