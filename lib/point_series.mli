(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

type 'c t = 'c Series_types.point_series
(** An opaque time-series of cells denominated in currency ['c]. *)

(** {1 Constructors} *)

val const : float -> 'c t
(** Create a series that produces the same value at every date. *)

val map : (float -> float) -> 'c t Lazy.t -> 'c t
(** Transform the output of a series by applying a function to each cell's value. *)

val convert : (Date.t -> float -> float) -> 'c t Lazy.t -> 'd t
(** Convert the output of a series from one unit to another by applying a function to each cell's
    value. The cell date is passed as an argument to the conversion function. *)

val accum : start_date:Date.t -> initial_value:float -> 'c Series_types.period_series Lazy.t -> 'c t
(** [accum ~start_date ~initial_value changes] accumulates period series values from [start_date]
    to the query date, adding them to [initial_value]. *)

(** {1 Accessors} *)

val id : 'c t -> int
(** Retrieve the unique identifier of the series. *)

(** {1 Evaluation} *)

val eval_query :
  eval_period:(Series_types.cache -> 'c Series_types.period_series -> Period.t -> Cell_types.period_cell Seq.t) ->
  Series_types.cache ->
  'c t ->
  Date.t ->
  'c Point_cell.t option
(** Materialize a point series at a given date into a point cell. Accepts an [eval_period] callback
    for resolving period series dependencies (e.g. in [TAccum]). The callback is provided by
    {!Series} to tie the knot between point and period evaluation without circular module
    dependencies. Uses the shared cache for memoization. *)

