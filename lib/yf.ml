(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Year fraction calculations for various day count conventions. *)

let is_month_end date = Date.day date = Date.days_in_month date

(** Actual/360 day count: returns the actual number of days divided by 360. *)
let act_360 dt1 dt2 = float_of_int (Date.diff dt2 dt1) /. 360.0

let days_in_year year = if Date.is_leap_year year then 366 else 365

(** Actual/Actual day count: splits actual days by calendar year and divides by that year's day
    count. *)
let act_act dt1 dt2 =
  let flipped, dt1, dt2 = if Date.compare dt1 dt2 > 0 then (-1, dt2, dt1) else (1, dt1, dt2) in
  let rec loop acc start =
    let year = Date.year start in
    if year = Date.year dt2 then
      acc +. (float_of_int (Date.diff dt2 start) /. float_of_int (days_in_year year))
    else
      let next_year_start = Date.make (year + 1) 1 1 in
      let acc =
        acc +. (float_of_int (Date.diff next_year_start start) /. float_of_int (days_in_year year))
      in
      loop acc next_year_start
  in
  loop 0.0 dt1 *. float_of_int flipped

(** 30/360 day count (NASD method): each month is treated as 30 days, each year as 360 days. *)
let thirty_360 dt1 dt2 =
  (* Based on this the Excel implementation discussed here: https://stackoverflow.com/questions/43355292/replicating-yearfrac-function-from-excel-in-python *)
  let flipped, dt1, dt2 = if Date.compare dt1 dt2 > 0 then (-1, dt2, dt1) else (1, dt1, dt2) in
  let y1 = Date.year dt1 in
  let m1 = Date.month dt1 in
  let d1 = Date.day dt1 in
  let y2 = Date.year dt2 in
  let m2 = Date.month dt2 in
  let d2 = Date.day dt2 in
  let start_is_feb_eom = m1 = 2 && is_month_end dt1 in
  let end_is_feb_eom = m2 = 2 && is_month_end dt2 in
  (* Per NASD 30/360 rules, order matters: adjust end day before start day *)
  let d2 = if end_is_feb_eom && start_is_feb_eom then 30 else d2 in
  let d2 = if d2 = 31 && d1 >= 30 then 30 else d2 in
  let d1 = if d1 = 31 then 30 else d1 in
  let d1 = if start_is_feb_eom then 30 else d1 in
  let days = d2 + (m2 * 30) + (y2 * 360) - (d1 + (m1 * 30) + (y1 * 360)) in
  float_of_int days /. 360.0 *. float_of_int flipped

(** Calendar monthly day count: each calendar month is 1/12th of a year. Partial calendar months are
    treated as actual days elapsed over actual days in the month. Year fraction from but excluding
    dt1 to and including dt2. *)
let cmonthly dt1 dt2 =
  let flipped, dt1, dt2 = if Date.compare dt1 dt2 > 0 then (-1, dt2, dt1) else (1, dt1, dt2) in
  let y1 = Date.year dt1 in
  let m1 = Date.month dt1 in
  let d1 = Date.day dt1 in
  let y2 = Date.year dt2 in
  let m2 = Date.month dt2 in
  let d2 = Date.day dt2 in
  let year_month_frac = float_of_int ((y2 * 360) + (m2 * 30) - ((y1 * 360) + (m1 * 30))) /. 360.0 in
  let start_month_last_day = Date.days_in_month dt1 in
  let start_stub = float_of_int (start_month_last_day - d1) /. float_of_int start_month_last_day in
  let end_month_last_day = Date.days_in_month dt2 in
  let end_stub = float_of_int (end_month_last_day - d2) /. float_of_int end_month_last_day in
  (year_month_frac +. ((start_stub -. end_stub) /. 12.0)) *. float_of_int flipped
