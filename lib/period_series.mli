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

(** {1 Constructors} *)

val const : 'c Period_cell.t Seq.t -> 'c t
(** A series backed by a pre-built cell sequence. *)

val unfold : deps:'c Series_types.series_dep list -> 'c Series_types.unfold_cell Seq.t -> 'c t
(** [unfold ~deps cells] builds a series from a declarative sequence of {!Series_types.unfold_cell}
    values.

    Each element in [cells] is either a {!Series_types.Seed} (constant, no dependencies) or a
    {!Series_types.Step} (declares queries against [deps] or the series' own earlier output). The
    system resolves queries and constructs the underlying {!Period_cell.t} values automatically.

    Dependencies are wrapped in {!Series_types.series_dep} to support both period and point series
    dependencies. Lazy values are only forced during evaluation, not at construction time.

    Because the callback never receives raw query functions or {!Period_cell.t} constructors, the
    only way to express dependencies is through {!Series_types.dep_query} values in
    {!Series_types.Step}, which are derived from the declared [deps]. This makes it impossible to
    silently bypass series-level dependency tracking or erase cell-level dependency connections. *)

val reduce_sum : Series_types.reduce
(** A reduce function that sums all cell values: [List.fold_left (+.) 0.0]. *)

val map : (float -> float) -> 'c t Lazy.t -> 'c t
(** [map f s] applies [f] to each cell's float value. The dependency is wrapped in [Lazy.t] to
    support mutually recursive series definitions via [let rec]. The lazy value is forced during
    evaluation, not at construction. *)

val convert : (Period.t -> float -> float) -> 'a t Lazy.t -> 'b t
(** [convert f s] applies [f period v] to each cell's float value, where [period] is the cell's
    period and [v] is its evaluated result. Unlike {!map}, the phantom type of the result may differ
    from the input, allowing unit/currency conversions. *)

val map2 : (float option -> float option -> float) -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
(** [map2 f s1 s2] combines two series cell-by-cell, aligning periods. *)

val sum : 'c t -> 'c t -> 'c t
val sub : 'c t -> 'c t -> 'c t
val mul : 'c t -> 'c t -> 'c t
val div : 'c t -> 'c t -> 'c t

(** {1 Accessors} *)

val id : 'c t -> int
(** Return the unique integer identifier assigned to a series at construction time. *)

(** {1 Evaluation} *)

val eval_seq :
  eval_point:(Series_types.cache -> Date.t -> 'c Series_types.point_series -> 'c Point_cell.t option) ->
  Series_types.cache ->
  'c t ->
  'c Period_cell.t Seq.t
(** Materialize a series into a lazy cell sequence. Accepts an [eval_point] callback for resolving
    point series dependencies. The callback is provided by {!Series} to tie the knot between period
    and point evaluation without circular module dependencies. *)

val eval_query :
  eval_point:(Series_types.cache -> Date.t -> 'c Series_types.point_series -> 'c Point_cell.t option) ->
  Series_types.cache ->
  'c t ->
  Period.t ->
  'c Period_cell.t Seq.t
(** Get the cells from a series that cover the given period range. *)

