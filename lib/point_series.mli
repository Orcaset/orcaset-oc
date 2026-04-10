(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

type 'c t = 'c Series_types.point_series
(** An opaque time-series of cells denominated in currency ['c]. *)

type 'c eval_period_fn =
  Series_types.cache -> 'c Series_types.period_series -> Period.t -> 'c Period_cell.t Seq.t
(** Callback type for resolving period series dependencies during point series evaluation. *)

(** {1 Constructors} *)

val const : label:string -> float -> 'c t
(** Create a series that produces the same value at every date. *)

val map : label:string -> (float -> float) -> 'c t Lazy.t -> 'c t
(** Transform the output of a series by applying a function to each cell's value. *)

val convert : label:string -> (Date.t -> float -> float) -> 'c t Lazy.t -> 'd t
(** Convert the output of a series from one unit to another. *)

val accum :
  label:string ->
  start_date:Date.t ->
  initial_value:float ->
  'c Series_types.period_series Lazy.t ->
  'c t
(** [accum ~label ~start_date ~initial_value changes] accumulates period series values from
    [start_date] to the query date, adding them to [initial_value]. *)

val of_list : label:string -> (Date.t * float) list -> 'c t
(** [of_list ~label cells] creates a series from a list of [(date, value)] pairs. *)

val extend : label:string -> 'c t -> (Date.t -> 'c t) -> 'c t
(** [extend ~label base cont] queries [base] first. If [base] produces a value, that value is
    returned. Otherwise [cont] is called with the query date to obtain a continuation series. *)

val map2 :
  label:string -> (float option -> float option -> float) -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
(** Combine two series with a binary function. *)

val neg : label:string -> 'c t Lazy.t -> 'c t

val sum : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
(** Elementwise addition. Missing values are filled with [0.0]. *)

val sub : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
(** Elementwise subtraction. Missing values are filled with [0.0]. *)

val mul : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
(** Elementwise multiplication. Missing values are filled with [0.0]. *)

val div : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
(** Elementwise division. Missing values are filled with [0.0]. *)

(** {1 Accessors} *)

val id : 'c t -> int
(** Retrieve the unique identifier of the series. *)

val label : 'c t -> string
(** Return the label assigned to a series at construction time. *)

(** {1 Evaluation} *)

val eval_query :
  eval_period:'c eval_period_fn -> Series_types.cache -> 'c t -> Date.t -> 'c Point_cell.t option
(** Materialize a point series at a given date into a point cell. *)
