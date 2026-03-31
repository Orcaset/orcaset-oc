(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Gregorian calendar dates.

    Immutable dates in the proleptic Gregorian calendar represented as year, month, and day triples.
    Arithmetic is performed via {{:https://en.wikipedia.org/wiki/Julian_day}Julian Day Number}
    conversions.

    {1 Dates} *)

type t
(** The type for Gregorian dates. *)

val make : int -> int -> int -> t
(** [make year month day] is a date for the given [year], [month] (1--12), and [day] (1--days in
    month).

    @raise Invalid_argument
      if [month] is outside 1--12 or [day] is outside 1--{!days_in_month} for the given year and
      month. *)

(** {1:accessors Accessors} *)

val year : t -> int
(** [year d] is [d]'s year. *)

val month : t -> int
(** [month d] is [d]'s month (1--12). *)

val day : t -> int
(** [day d] is [d]'s day of the month (1--31). *)

(** {1:preds Predicates and comparisons} *)

val compare : t -> t -> int
(** [compare d0 d1] is a total order on dates, ordered chronologically. *)

val equal : t -> t -> bool
(** [equal d0 d1] is [true] iff [d0] and [d1] represent the same date. *)

val ( < ) : t -> t -> bool
(** [d0 < d1] is [true] iff [d0] is strictly before [d1]. *)

val ( <= ) : t -> t -> bool
(** [d0 <= d1] is [true] iff [d0] is before or equal to [d1]. *)

val ( > ) : t -> t -> bool
(** [d0 > d1] is [true] iff [d0] is strictly after [d1]. *)

val ( >= ) : t -> t -> bool
(** [d0 >= d1] is [true] iff [d0] is after or equal to [d1]. *)

val min : t -> t -> t
(** [min d0 d1] is the earlier of [d0] and [d1]. *)

val max : t -> t -> t
(** [max d0 d1] is the later of [d0] and [d1]. *)

(** {1:arith Arithmetic} *)

val diff : t -> t -> int
(** [diff d0 d1] is the signed number of calendar days from [d1] to [d0], i.e. [d0 - d1]. Positive
    when [d0] is after [d1]. *)

val add_days : int -> t -> t
(** [add_days n d] is the date [n] calendar days after [d]. [n] may be negative. *)

(** {1:weekday Day of week} *)

val weekday : t -> int
(** [weekday d] is the ISO 8601 day of week for [d]: [1] = Monday, ..., [7] = Sunday. Used by
    {!Calendar.weekdays} to identify business days. *)

(** {1:props Properties} *)

val is_leap_year : int -> bool
(** [is_leap_year y] is [true] iff [y] is a Gregorian leap year. *)

val days_in_month : t -> int
(** [days_in_month d] is the number of days in the month of [d] (28, 29, 30, or 31). *)

(** {1:shifting Shifting} *)

val shift : Offset.t -> t -> t
(** [shift offset d] is [d] advanced by [offset]. Month-based components (months, quarters, years)
    are applied before day-based ones (days, weeks). Day clamping from {!add_months} applies to the
    month step. When [Offset.month_end] is [true], the result is snapped to the last day of its
    month. *)

(** {1:hash Hashing} *)

val hash : t -> int
(** [hash d] is a hash of the date [d], derived from its Julian Day Number. *)

(** {1:fmt Formatting} *)

val to_string : t -> string
(** [to_string d] is [d] formatted as ["YYYY-MM-DD"]. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a date with {!to_string}. *)
