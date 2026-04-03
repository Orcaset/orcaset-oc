(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Year fraction calculations for various day count conventions.

    Each function computes the fraction of a year between two dates according to a specific day
    count convention.

    {1 Day count conventions} *)

val actual_360 : Date.t -> Date.t -> float
(** [actual_360 dt1 dt2] is the year fraction from [dt1] to [dt2] using the Actual/360 convention:
    the actual number of days between the dates divided by 360. *)

val thirty_360 : Date.t -> Date.t -> float
(** [thirty_360 dt1 dt2] is the year fraction from [dt1] to [dt2] using the 30/360 NASD convention:
    each month is treated as 30 days and each year as 360 days. February end-of-month dates are
    adjusted to 30. *)

val cmonthly : Date.t -> Date.t -> float
(** [cmonthly dt1 dt2] calculates the year fraction from [dt1] to [dt2] assuming each calendar month
    is 1/12th of a year using month-end dates. Partial months are based on the number of days
    elapsed over the total number of days in the calendar month. The fraction is calculated on a
    month-end basis,from but excluding [dt1] to and including [dt2]. *)
