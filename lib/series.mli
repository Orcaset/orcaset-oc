(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

type split = Split.t
(** Strategy for assigning value when a span is split. See {!Split}. *)

module Agg : sig
  type sample = { period : Period.t; value : float }
  (** One defined span sample collected for aggregation. Missing query coverage and undefined cell
      values are represented by [None] in aggregation inputs. *)

  type t
  (** A span aggregation function. *)

  val make : (sample option list -> float option) -> t
  (** [make f] creates an aggregation from [f]. *)

  val reduce : t -> sample option list -> float option
  (** [reduce agg samples] applies [agg] to [samples]. *)

  val sum : t
  val min : t
  val max : t
  val average : t

  val time_weighted_average : (Date.t -> Date.t -> float) -> t
  (** [time_weighted_average year_frac] weights each present sample by [year_frac start end_], where
      [start] and [end_] are the sample period bounds. *)
end

module rec Spans : sig
  type unfold_cell
  (** Describes one yielded step during {!Spans:t} {!unfold} evaluation: the period cover,
      interpolation strategy ({!split}), and lazily-evaluated {!Formula.t}. Produced with {!cell}.
  *)

  type +'tag t
  (** Span series carrying a phantom semantic tag. The tag is compile-time metadata only; it is not
      available for runtime matching. *)

  type packed
  (** Existentially packed span series for APIs that need heterogeneous input tags. *)

  val pack : 'tag t -> packed
  (** [pack series] hides [series]' phantom tag for heterogeneous collections. *)

  val neg : ?label:string -> 'in_tag t -> 'out_tag t
  val scale : ?label:string -> float -> 'in_tag t -> 'out_tag t

  val sum : ?label:string -> agg:Agg.t -> packed list -> 'out_tag t
  (** [sum ?label ~agg series] sums present aligned inputs; missing inputs are ignored, and rows
      with no present inputs are [None]. *)

  val sub : ?label:string -> agg:Agg.t -> 'a_tag t -> 'b_tag t -> 'out_tag t
  (** [sub ?label ~agg a b] subtracts [b] from [a]; a missing side is treated as [0.0] only when the
      other side is present. *)

  val mul : ?label:string -> agg:Agg.t -> packed list -> 'out_tag t
  (** [mul ?label ~agg series] multiplies aligned inputs; any missing input makes the row [None]. *)

  val div : ?label:string -> agg:Agg.t -> 'a_tag t -> 'b_tag t -> 'out_tag t
  (** [div ?label ~agg a b] divides [a] by [b]; either missing side makes the row [None]. *)

  val const : ?label:string -> split:split -> agg:Agg.t -> period:Period.t -> float -> 'tag t
  (** [const ?label ~split ~agg ~period value] is a span series containing one value over [period].
  *)

  val of_list : ?label:string -> split:split -> agg:Agg.t -> (Period.t * float) list -> 'tag t
  (** [of_list ?label ~split ~agg cells] is a span series that yields [cells] in list order. No
      ordering or overlap validation is performed. *)

  val map : ?label:string -> (float -> float) -> 'in_tag t -> 'out_tag t
  (** [map ?label f series] applies [f] to each value in [series]. *)

  val map2 :
    ?label:string ->
    agg:Agg.t ->
    'a_tag t ->
    'b_tag t ->
    (float option -> float option -> float option) ->
    'out_tag t

  (** [map2 ?label ~agg a b f] aligns [a] and [b], then applies [f] to each aligned pair. Missing
      sides are passed to [f] as [None]. *)

  val mapn :
    ?label:string -> agg:Agg.t -> packed list -> (float option list -> float option) -> 'out_tag t
  (** [mapn ?label ~agg series f] aligns [series] at common boundaries, then applies [f] to each
      aligned row of values (one entry per input series, in input order). Missing entries are passed
      as [None]. *)

  val extend : agg:Agg.t -> 'tag t -> 'tag t -> 'tag t
  (** [extend ~agg a b] yields all spans from [a], then continues with [b]. If [a] ends inside a
      span from [b], that first overlapping span from [b] is clipped to start at [a]'s end. Takes
      the first series' label. *)

  val clipped : after:Date.t -> until:Date.t -> 'tag t -> 'tag t
  (** [clipped ~after ~until series] exposes only the portion of [series] from [after] [until].
      Partially overlapping spans are clipped with their existing split strategy. Raises
      [Invalid_argument] if [until] is before [after]. *)

  val after : Date.t -> 'tag t -> 'tag t
  (** [after date series] exposes the portion of [series] from [date] inclusive onward. *)

  val until : Date.t -> 'tag t -> 'tag t
  (** [until date series] exposes the portion of [series] before [date]. *)

  val unfold :
    ?label:string ->
    agg:Agg.t ->
    deps:(unit -> 'readers Deps.t) ->
    init:'state ->
    cells:('readers -> 'state -> (unfold_cell * 'state) option) ->
    unit ->
    'tag t
  (** [unfold ?label ~agg ~deps ~init ~cells ()] builds a span series by repeatedly calling [cells].
  *)

  val unfold_from :
    ?label:string ->
    agg:Agg.t ->
    deps:(unit -> 'readers Deps.t) ->
    cells:('readers -> Period.t -> (unfold_cell * Period.t) option) ->
    'tag t ->
    'tag t
  (** [unfold_from ?label ~deps ~cells base] yields all spans from [base], then continues by calling
      [cells] with the final period emitted by [base]. Each continuation step returns the emitted
      cell and the next period passed to [cells]. If [base] emits no spans, [cells] is never called.
  *)

  val unfold_rec :
    ?label:string ->
    agg:Agg.t ->
    deps:('tag t -> 'readers Deps.t) ->
    init:'state ->
    cells:('readers -> 'state -> (unfold_cell * 'state) option) ->
    unit ->
    'tag t
  (** [unfold_rec ?label ~deps ~init ~cells ()] is like {!unfold}, but [deps] receives the series
      being constructed. Use it for self-recursive span series. *)

  val cell : period:Period.t -> split:split -> float option Formula.t -> Spans.unfold_cell
  (** [cell ~period ~split formula] builds a {!Spans.unfold_cell}: one unfolded step spanning
      [period] with splitting strategy [split]. Dependency readers appearing in [formula] become
      queries resolved when that step's evaluated span cell is forced (never earlier). *)

  val label : 'tag t -> string option
  (** [label series] returns the optional human-readable label attached to [series]. *)

  val agg : 'tag t -> Agg.t
  (** [agg series] returns the series' span aggregation function. *)

  val with_agg : agg:Agg.t -> 'tag t -> 'tag t
  (** [with_agg ~agg series] returns [series] with a replacement aggregation function. *)
end

and Points : sig
  type +'tag t
  (** Point series carrying a phantom semantic tag. The tag is compile-time metadata only. *)

  type packed
  (** Existentially packed point series for APIs that need heterogeneous input tags. *)

  val pack : 'tag t -> packed
  (** [pack series] hides [series]' phantom tag for heterogeneous collections. *)

  val neg : ?label:string -> 'in_tag t -> 'out_tag t
  val scale : ?label:string -> float -> 'in_tag t -> 'out_tag t

  val sum : ?label:string -> packed list -> 'out_tag t
  (** [sum ?label series] sums present inputs; missing inputs are ignored, and dates with no present
      inputs are [None]. *)

  val sub : ?label:string -> 'a_tag t -> 'b_tag t -> 'out_tag t
  (** [sub ?label a b] subtracts [b] from [a]; a missing side is treated as [0.0] only when the
      other side is present. *)

  val mul : ?label:string -> packed list -> 'out_tag t
  (** [mul ?label series] multiplies inputs; any missing input makes the date [None]. *)

  val div : ?label:string -> 'a_tag t -> 'b_tag t -> 'out_tag t
  (** [div ?label a b] divides [a] by [b]; either missing side makes the date [None]. *)

  val const : ?label:string -> period:Period.t -> float -> 'tag t
  (** [const ?label ~period value] is a point series with [value] at dates contained by [period]. *)

  val of_list : ?label:string -> (Date.t * float) list -> 'tag t
  (** [of_list ?label values] is a sparse point series with exact values at the listed dates. No
      ordering or duplicate-date validation is performed. *)

  val map : ?label:string -> (float -> float) -> 'in_tag t -> 'out_tag t
  (** [map ?label f series] applies [f] to each value in [series]. *)

  val map2 :
    ?label:string ->
    'a_tag t ->
    'b_tag t ->
    (float option -> float option -> float option) ->
    'out_tag t

  (** [map2 ?label a b f] applies [f] to the values of [a] and [b] on each queried date. Missing
      sides are passed to [f] as [None]. *)

  val mapn : ?label:string -> packed list -> (float option list -> float option) -> 'out_tag t
  (** [mapn ?label series f] applies [f] on each queried date to the list of values from [series]
      (in input order). Missing entries are passed as [None]. *)

  val accum : ?label:string -> init:float -> 'change_tag Spans.t -> 'out_tag t
  (** [accum ?label ~init changes] is a point series whose value starts at [init] and changes by the
      cumulative span values in [changes]. *)

  val label : 'tag t -> string option
  (** [label series] returns the optional human-readable label attached to [series]. *)
end

and Deps : sig
  (** Applicative for declaring which {!Spans:t} dependency series an {!Spans.unfold} span series
      reads while running its user-defined [cells] function—those readers compose into formulas
      under [deps]. *)

  (* type span_reader *)

  type span_reader = period:Period.t -> float option Formula.t
  (** A reader bound to a declared span dependency. Calling it records a cell-level span query; the
      query is resolved only when the formula is evaluated. *)

  (* type point_reader *)

  type point_reader = date:Date.t -> float option Formula.t
  (** A reader bound to a declared point dependency. Calling it records a cell-level point query;
      the query is resolved only when the formula is evaluated. Missing dates resolve to [None]. *)

  type _ t
  (** Applicative computation. Build with [none], [span_dep], [point_dep], or the [let+]/[and+]
      operators. *)

  val none : unit t
  (** Empty dependency set. Use when an [unfold]'s [cells] function needs no readers. *)

  val span_dep : 'tag Spans.t -> span_reader t
  (** Declare a span dependency; the produced value is the [span_reader] bound to it. *)

  val point_dep : 'tag Points.t -> point_reader t
  (** Declare a point dependency; the produced value is the [point_reader] bound to it. *)

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

and Formula : sig
  (** A formula describes how to compute a cell value from zero or more declared cell-level
      dependency queries. Formula construction records the queries; evaluation later resolves them.
  *)

  type 'a t

  type packed_query =
    | Span_query_item : { series : 'tag Spans.t; period : Period.t } -> packed_query
    | Point_query_item : { series : 'tag Points.t; date : Date.t } -> packed_query
        (** An inspectable cell-level dependency query recorded by a formula. *)

  val pure : 'a -> 'a t
  (** [pure x] is a formula with no dependency queries. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  (** Applicative operators for combining dependency query results. *)

  val queries : 'a t -> packed_query list
  (** [queries formula] returns the cell-level span and point queries needed by [formula]. *)
end

type span_kind
type point_kind

type (_, _) series =
  | Point_series : 'tag Points.t -> (point_kind, 'tag) series
  | Span_series : 'tag Spans.t -> (span_kind, 'tag) series

val label : ('kind, 'tag) series -> string option
(** [label series] returns the optional human-readable label attached to [series]. *)

type packed_series = Series : ('kind, 'tag) series -> packed_series
type dependency = { series : packed_series; dependencies : dependency list; is_back_edge : bool }

val dependencies : ('kind, 'tag) series -> dependency list
(** [dependencies series] returns the direct dependencies of [series] as a nested list. Back-edges
    into the current traversal path are marked with [is_back_edge] and have no further nested
    dependencies. *)

(** {1 Querying} *)

type series_cache

exception Evaluation_did_not_converge of { iterations : int; tolerance : float; max_delta : float }
(** Raised when iterative cell evaluation does not converge within the built-in iteration limit. *)

val make_cache : unit -> series_cache
(** [make_cache ()] creates a fresh memoization cache for queries. A cache should be reused across
    related queries to benefit from memoization; it is not safe to share across threads. *)

val query_span_samples : series_cache -> 'tag Spans.t -> period:Period.t -> Agg.sample option list
(** [query_span_samples cache s ~period] collects defined clipped samples from [s]. Missing query
    coverage and undefined cell values are represented by [None]. *)

val query_span : series_cache -> 'tag Spans.t -> period:Period.t -> float option
(** [query_span cache s ~period] aggregates [s]'s resolved samples over [period] using {!Spans.agg}.
*)

val query_point : series_cache -> 'tag Points.t -> date:Date.t -> float option
(** [query_point cache s ~date] returns [s]'s value at [date], or [None] if the series has no value
    there. *)
