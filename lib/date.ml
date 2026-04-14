(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

type t = { year : int; month : int; day : int; jdn : int }

let is_leap_year y = y mod 4 = 0 && (y mod 100 <> 0 || y mod 400 = 0)

let days_in_month_raw ~year ~month =
  match month with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> if is_leap_year year then 29 else 28
  | _ -> invalid_arg "Date: month must be 1-12"

(* Julian Day Number Conversions *)

(* Integer-only Gregorian <-> JDN formulas from the US Naval Observatory /
   Meeus "Astronomical Algorithms". The magic constants come from the length
   of the Gregorian 400-year cycle (146097 days) and the 5-month approximation
   (153 days per 5 months). Single-letter locals mirror the published derivation
   so the correspondence is easy to verify. *)

let compute_jdn ~year ~month ~day =
  let a = (14 - month) / 12 in
  let y = year + 4800 - a in
  let m = month + (12 * a) - 3 in
  day + (((153 * m) + 2) / 5) + (365 * y) + (y / 4) - (y / 100) + (y / 400) - 32045

let of_jdn jdn =
  let a = jdn + 32044 in
  let b = ((4 * a) + 3) / 146097 in
  let c = a - (146097 * b / 4) in
  let d = ((4 * c) + 3) / 1461 in
  let e = c - (1461 * d / 4) in
  let m = ((5 * e) + 2) / 153 in
  let day = e - (((153 * m) + 2) / 5) + 1 in
  let month = m + 3 - (12 * (m / 10)) in
  let year = (100 * b) + d - 4800 + (m / 10) in
  { year; month; day; jdn }

(* Construction *)

let make year month day =
  if month < 1 || month > 12 then invalid_arg (Printf.sprintf "Date.make: invalid month %d" month);
  let max_day = days_in_month_raw ~year ~month in
  if day < 1 || day > max_day then
    invalid_arg (Printf.sprintf "Date.make: invalid day %d for %04d-%02d" day year month);
  { year; month; day; jdn = compute_jdn ~year ~month ~day }

let lower_bound = make 1 1 1

(* Accessors *)

let year d = d.year
let month d = d.month
let day d = d.day

(* Comparison *)

let compare d1 d2 = Int.compare d1.jdn d2.jdn
let equal d1 d2 = compare d1 d2 = 0
let ( < ) d1 d2 = compare d1 d2 < 0
let ( <= ) d1 d2 = compare d1 d2 <= 0
let ( > ) d1 d2 = compare d1 d2 > 0
let ( >= ) d1 d2 = compare d1 d2 >= 0
let min d1 d2 = if d1 <= d2 then d1 else d2
let max d1 d2 = if d1 >= d2 then d1 else d2

(* Arithmetic *)

let diff d1 d2 = d1.jdn - d2.jdn
let add_days n d = of_jdn (d.jdn + n)

let add_months n d =
  if n = 0 then d
  else
    let total = (d.year * 12) + d.month - 1 + n in
    (* OCaml's (/) truncates toward zero, but we need Euclidean (floor)
       division so that month lands in 1..12 for negative totals too.
       Subtracting 11 before dividing simulates floor division for negatives.
       Uses Stdlib.( >= ) because Date.( >= ) is shadowed in this module. *)
    let year = (if Stdlib.( >= ) total 0 then total else total - 11) / 12 in
    let month = total - (year * 12) + 1 in
    let max_day = days_in_month_raw ~year ~month in
    let day = Int.min d.day max_day in
    { year; month; day; jdn = compute_jdn ~year ~month ~day }

(* Weekday *)

(* JDN epoch (0) is a Monday, so jdn mod 7 gives 0=Mon .. 6=Sun.
   We add 1 to match ISO 8601: 1=Monday .. 7=Sunday. *)
let weekday d = (d.jdn mod 7) + 1

(* Properties *)

let days_in_month d = days_in_month_raw ~year:d.year ~month:d.month

(* Shifting *)

(* Application order matters: months first (so day-clamping from
   add_months happens before day offsets), then days, then the
   month_end snap which overrides the day to the last of the month. *)
let shift (offset : Offset.t) date =
  let total_months = offset.months + (offset.quarters * 3) + (offset.years * 12) in
  let total_days = offset.days + (offset.weeks * 7) in
  let d = add_months total_months date in
  let d = if total_days = 0 then d else add_days total_days d in
  if offset.month_end then make (year d) (month d) (days_in_month d) else d

(* Formatting *)

let hash d = Hashtbl.hash d.jdn
let to_string d = Printf.sprintf "%04d-%02d-%02d" d.year d.month d.day
let pp fmt d = Format.pp_print_string fmt (to_string d)
