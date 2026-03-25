(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Year fraction calculations for various day count conventions. *)

let diff_days d1 d2 = CalendarLib.Date.sub d2 d1 |> CalendarLib.Date.Period.nb_days

let is_month_end date =
  let last_day = CalendarLib.Date.days_in_month date in
  CalendarLib.Date.day_of_month date = last_day

let days_in_month date = CalendarLib.Date.days_in_month date

(** Actual/360 day count: returns the actual number of days divided by 360. *)
let actual_360 dt1 dt2 = float_of_int (diff_days dt1 dt2) /. 360.0

(** 30/360 day count (NASD method): each month is treated as 30 days, each year as 360 days. *)
let thirty_360 dt1 dt2 =
  (* Based on this the Excel implementation discussed here: https://stackoverflow.com/questions/43355292/replicating-yearfrac-function-from-excel-in-python *)
  let flipped, dt1, dt2 =
    if CalendarLib.Date.compare dt1 dt2 > 0 then (-1, dt2, dt1) else (1, dt1, dt2)
  in
  let y1 = CalendarLib.Date.year dt1 in
  let m1 = CalendarLib.Date.int_of_month (CalendarLib.Date.month dt1) in
  let d1 = CalendarLib.Date.day_of_month dt1 in
  let y2 = CalendarLib.Date.year dt2 in
  let m2 = CalendarLib.Date.int_of_month (CalendarLib.Date.month dt2) in
  let d2 = CalendarLib.Date.day_of_month dt2 in
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
  let flipped, dt1, dt2 =
    if CalendarLib.Date.compare dt1 dt2 > 0 then (-1, dt2, dt1) else (1, dt1, dt2)
  in
  let y1 = CalendarLib.Date.year dt1 in
  let m1 = CalendarLib.Date.int_of_month (CalendarLib.Date.month dt1) in
  let d1 = CalendarLib.Date.day_of_month dt1 in
  let y2 = CalendarLib.Date.year dt2 in
  let m2 = CalendarLib.Date.int_of_month (CalendarLib.Date.month dt2) in
  let d2 = CalendarLib.Date.day_of_month dt2 in
  let year_month_frac = float_of_int ((y2 * 360) + (m2 * 30) - ((y1 * 360) + (m1 * 30))) /. 360.0 in
  let start_month_last_day = days_in_month dt1 in
  let start_stub = float_of_int (start_month_last_day - d1) /. float_of_int start_month_last_day in
  let end_month_last_day = days_in_month dt2 in
  let end_stub = float_of_int (end_month_last_day - d2) /. float_of_int end_month_last_day in
  (year_month_frac +. ((start_stub -. end_stub) /. 12.0)) *. float_of_int flipped
