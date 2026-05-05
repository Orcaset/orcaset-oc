(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Span split strategies.

    Split strategies allocate a span value across two sub-periods when the span is clipped or
    aligned with other series. *)

type part
(** One side of a span split. It describes how to project the original span value onto that side. *)

val part : value:(float -> float) -> part
(** [part ~value] creates one side of a custom split strategy. [value] receives the original span
    value and returns the split-side value. *)

val value : part -> float -> float
(** [value part original] applies [part]'s value projection to [original]. *)

type t = period:Period.t -> date:Date.t -> part * part
(** Strategy for assigning value when a span over [period] is split at an interior [date]. The first
    returned part is assigned to [Period.make (Period.start period) date]; the second is assigned to
    [Period.make date (Period.end_ period)]. *)

val daily : t
(** Splits a span's value proportionally by calendar days. E.g. clipping a $120/year span to one
    quarter yields roughly $30 for that quarter. *)

val const : t
(** Assigns the original span's value to each clipped side. E.g. splitting a span with value "10"
    will create sub-periods with values of "10" as well. *)

val cmonthly : t
(** Splits a span's value proportionally by {!Yf.cmonthly} year fraction. *)

val act_360 : t
(** Splits a span's value proportionally by {!Yf.act_360} year fraction. *)
