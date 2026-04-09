(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

type 'c t = 'c Series_types.point_series
(** An opaque time-series of cells denominated in currency ['c]. *)

type 'c eval_period_fn =
  Series_types.cache -> 'c Series_types.period_series -> Period.t -> 'c Period_cell.t Seq.t
(** Callback type for resolving period series dependencies during point series evaluation. *)

(** {1 Constructors} *)

val const : float -> 'c t
(** Create a series that produces the same value at every date. *)

val map : (float -> float) -> 'c t Lazy.t -> 'c t
(** Transform the output of a series by applying a function to each cell's value. *)

val convert : (Date.t -> float -> float) -> 'c t Lazy.t -> 'd t
(** Convert the output of a series from one unit to another by applying a function to each cell's
    value. The cell date is passed as an argument to the conversion function. *)

val accum : start_date:Date.t -> initial_value:float -> 'c Series_types.period_series Lazy.t -> 'c t
(** [accum ~start_date ~initial_value changes] accumulates period series values from [start_date] to
    the query date, adding them to [initial_value]. *)

val of_list : (Date.t * float) list -> 'c t
(** [of_list cells] creates a series from a list of [(date, value)] pairs. At evaluation, returns
    the value for the queried date if one exists, or [None] otherwise. Dates are compared with
    {!Date.equal}. *)

val extend : 'c t -> (Date.t -> 'c t) -> 'c t
(** [extend base cont] queries [base] first. If [base] produces a value, that value is returned.
    Otherwise [cont] is called with the query date to obtain a continuation series, and the
    continuation is queried at the same date. The continuation constructor is memoized — [cont] is
    called at most once, on the first date where [base] returns no value. *)

val map2 : (float option -> float option -> float) -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
(** Combine two series with a binary function. Each input is passed as [float option] — [None] when
    the underlying series has no value at the query date. *)

val neg : 'c t Lazy.t -> 'c t

val sum : 'c t Lazy.t -> 'c t Lazy.t -> 'c t
(** Elementwise addition. Missing values are filled with [0.0]. *)

val sub : 'c t Lazy.t -> 'c t Lazy.t -> 'c t
(** Elementwise subtraction. Missing values are filled with [0.0]. *)

val mul : 'c t Lazy.t -> 'c t Lazy.t -> 'c t
(** Elementwise multiplication. Missing values are filled with [0.0]. *)

val div : 'c t Lazy.t -> 'c t Lazy.t -> 'c t
(** Elementwise division. Missing values are filled with [0.0]. *)

(** {1 Accessors} *)

val id : 'c t -> int
(** Retrieve the unique identifier of the series. *)

(** {1 Evaluation} *)

val eval_query :
  eval_period:'c eval_period_fn -> Series_types.cache -> 'c t -> Date.t -> 'c Point_cell.t option
(** Materialize a point series at a given date into a point cell. Accepts an [eval_period] callback
    for resolving period series dependencies (e.g. in [TAccum]). The callback is provided by
    {!Series} to tie the knot between point and period evaluation without circular module
    dependencies. Uses the shared cache for memoization. *)
