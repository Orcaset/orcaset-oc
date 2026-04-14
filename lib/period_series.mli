(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** A lazy, period-indexed stream of {!Period_cell.t} values with explicit dependency tracking.

    The type ['c t] is abstract — construct series using the smart constructors {!const}, {!unfold},
    {!map}, and {!map2}. In particular, {!unfold} ensures that every dependency used inside the
    callback is declared up-front, so that dependency analysis always reflects the true dependency
    graph.

    The phantom type parameter ['c] tracks the currency (or unit) associated with the series,
    matching the currency of the cells it produces. *)

type 'c t = 'c Series_types.period_series
(** A period-indexed series of cells denominated in currency ['c]. *)

type 'c eval_point_fn =
  Series_types.cache -> Date.t -> 'c Series_types.point_series -> 'c Point_cell.t option
(** Callback type for resolving point series dependencies during period series evaluation. *)

(** {1 Constructors} *)

val of_seq : label:string -> 'c Period_cell.t Seq.t -> 'c t
(** A series backed by a pre-built cell sequence. *)

val unfold :
  label:string -> deps:'c Series_types.series_dep list -> 'c Series_types.period_unfold_cell Seq.t -> 'c t
(** [unfold ~label ~deps cells] builds a series from a declarative sequence of
    {!Series_types.period_unfold_cell} values. *)

val extend : label:string -> 'c t -> (Period.t -> 'c t) -> 'c t
(** [extend ~label base cont] evaluates [base] (which must be finite), passes the last period to
    [cont], and returns the concatenation of both series. If [base] is empty, returns an empty
    series. *)

val reduce_sum : Series_types.reduce
(** A reduce function that sums all cell values: [List.fold_left (+.) 0.0]. *)

val map : label:string -> (float -> float) -> 'c t Lazy.t -> 'c t
(** [map ~label f s] applies [f] to each cell's float value. *)

val convert : label:string -> (Period.t -> float -> float) -> 'a t Lazy.t -> 'b t
(** [convert ~label f s] applies [f period v] to each cell's float value, allowing unit/currency
    conversions. *)

val map2 :
  label:string -> (float option -> float option -> float) -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
(** [map2 ~label f s1 s2] combines two series cell-by-cell, aligning periods. *)

val const_ann_growth :
  label:string ->
  start:Date.t ->
  value:float ->
  rate:float ->
  offset:Offset.t ->
  yf:(Date.t -> Date.t -> float) ->
  'c t
(** A convenience constructor for a series that grows from a starting value by a constant annual
    growth rate, compounded according to the given offset. *)

val sum : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
val sub : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
val mul : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
val div : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t

val filter :
  label:string -> ('c Period_cell.t Seq.t -> 'c Period_cell.t Seq.t) -> 'c t Lazy.t -> 'c t
(** [filter ~label f s] applies a cell-sequence transformation [f] to the evaluated cells of [s]. *)

val after : label:string -> Date.t -> 'c t Lazy.t -> 'c t
(** [after ~label date s] returns a series containing only cells that fall after [date]. *)

(** {1 Accessors} *)

val id : 'c t -> int
(** Return the unique integer identifier assigned to a series at construction time. *)

val label : 'c t -> string
(** Return the label assigned to a series at construction time. *)

(** {1 Evaluation} *)

val eval_seq : eval_point:'c eval_point_fn -> Series_types.cache -> 'c t -> 'c Period_cell.t Seq.t
(** Materialize a series into a cell sequence. *)

val eval_query :
  eval_point:'c eval_point_fn -> Series_types.cache -> Period.t -> 'c t -> 'c Period_cell.t Seq.t
(** Get the cells from a series that cover the given period range. *)
