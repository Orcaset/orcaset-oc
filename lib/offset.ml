(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

type t = { days : int; weeks : int; months : int; quarters : int; years : int; month_end : bool }

let make ?(days = 0) ?(weeks = 0) ?(months = 0) ?(quarters = 0) ?(years = 0) ?(month_end = false) ()
    =
  { days; weeks; months; quarters; years; month_end }
