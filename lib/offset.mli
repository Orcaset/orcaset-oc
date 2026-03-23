(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Composite calendar offsets.

    An offset describes a composite duration made of days, weeks, months, quarters, and years.
    Offsets are used to shift dates and periods by calendar-aware increments. *)

type t = { days : int; weeks : int; months : int; quarters : int; years : int; month_end : bool }
(** The type for calendar offsets. When [month_end] is [true], the resulting date is snapped to the
    last day of its month after all other components are applied. *)

val make :
  ?days:int ->
  ?weeks:int ->
  ?months:int ->
  ?quarters:int ->
  ?years:int ->
  ?month_end:bool ->
  unit ->
  t
(** [make ()] is an offset with all components defaulting to [0] and [month_end] to [false]. *)
