(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

type series_id

val new_id : unit -> series_id
(** [new_id ()] returns a fresh series id. Required when constructing series variants. Reusing one
    id for multiple series is invalid and raises an exception when the affected series are queried
    or inspected for dependencies. *)

type split
(** Strategy for assigning value when a span is clipped to a sub-period. *)

val proportional_split : split
(** Splits a span's value proportionally by time. E.g. clipping a $120/yr span to one quarter yields
    $30 for that quarter. *)

val const_split : split
(** Assigns the original span's value to each clipped side. E.g. clipping a "40 mph" span to any
    sub-period still yields 40 mph. *)

module rec Formula : sig
  (** A formula describes how to compute a cell value from zero or more declared cell-level
      dependency queries. Formula construction records the queries; evaluation later resolves them
      to floats. *)

  type 'a t

  type packed_query =
    | Span_query_item of { series : Spans.t; period : Period.t }
    | Point_query_item of { series : Points.t; date : Date.t }
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

and Spans : sig
  type unfold_cell
  (** Describes one yielded step during {!Spans:t} {!Unfold} evaluation: the period cover,
      interpolation strategy ({!split}), and lazily-evaluated {!Formula.t}. Produced with {!cell}.
  *)

  type t =
    | Const of { id : series_id; label : string option; period : Period.t; value : unit -> float }
    | Map of { id : series_id; label : string option; dep : t; f : float -> float }
    | Map2 of {
        id : series_id;
        label : string option;
        a : t;
        b : t;
        f : float option -> float option -> float;
      }
    | After of { id : series_id; label : string option; date : Date.t; dep : t }
    | Unfold : {
        id : series_id;
        label : string option;
        deps : unit -> 'readers Deps.t;
        init : 'state;
        cells : 'readers -> 'state -> (unfold_cell * 'state) option;
      }
        -> t

  val label : t -> string option
  (** [label series] returns the optional human-readable label attached to [series]. *)

  val neg : ?label:string -> t -> t
  val scale : ?label:string -> float -> t -> t
  val sum : ?label:string -> ?fill:float -> t -> t -> t
  val sub : ?label:string -> ?fill:float -> t -> t -> t
  val mul : ?label:string -> ?fill:float -> t -> t -> t
  val div : ?label:string -> ?fill:float -> t -> t -> t

  val after : Date.t -> t -> t
  (** [after date series] returns a span series containing cells from [series] that start on or
      after [date]. If [date] falls inside a source cell, that cell is split and the portion from
      [date] is included. *)
end

and Points : sig
  type t =
    | Const of { id : series_id; label : string option; period : Period.t; value : unit -> float }
    | Map of { id : series_id; label : string option; dep : t; f : float -> float }
    | Map2 of {
        id : series_id;
        label : string option;
        a : t;
        b : t;
        f : float option -> float option -> float;
      }
    | Accum of { id : series_id; label : string option; init : float; changes : Spans.t }

  val label : t -> string option
  (** [label series] returns the optional human-readable label attached to [series]. *)

  val neg : ?label:string -> t -> t
  val scale : ?label:string -> float -> t -> t
  val sum : ?label:string -> ?fill:float -> t -> t -> t
  val sub : ?label:string -> ?fill:float -> t -> t -> t
  val mul : ?label:string -> ?fill:float -> t -> t -> t
  val div : ?label:string -> ?fill:float -> t -> t -> t
end

and Deps : sig
  (** Applicative for declaring which {!Spans:t} dependency series an {!Unfold} span series reads
      while running its user-defined [cells] function—those readers compose into formulas under
      [deps]. *)

  (* type span_reader *)

  type span_reader = period:Period.t -> reduce:(float option list -> float) -> float Formula.t
  (** A reader bound to a declared span dependency. Calling it records a cell-level span query; the
      query is resolved only when the formula is evaluated. *)

  (* type point_reader *)

  type point_reader = date:Date.t -> default:float -> float Formula.t
  (** A reader bound to a declared point dependency. Calling it records a cell-level point query;
      the query is resolved only when the formula is evaluated. *)

  type _ t
  (** Applicative computation. Build with [none], [span_dep], [point_dep], or the [let+]/[and+]
      operators. *)

  val none : unit t
  (** Empty dependency set. Use when an [Unfold]'s [cells] function needs no readers. *)

  val span_dep : Spans.t -> span_reader t
  (** Declare a span dependency; the produced value is the [span_reader] bound to it. *)

  val point_dep : Points.t -> point_reader t
  (** Declare a point dependency; the produced value is the [point_reader] bound to it. *)

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

val cell : period:Period.t -> split:split -> float Formula.t -> Spans.unfold_cell
(** [cell ~period ~split formula] builds a {!Spans.unfold_cell}: one unfolded step spanning [period]
    with splitting strategy [split]. Dependency readers appearing in [formula] become queries
    resolved when that step's evaluated span cell is forced (never earlier). *)

type _ series =
  | Point_series : Points.t -> [ `Point ] series
  | Span_series : Spans.t -> [ `Span ] series

val label : 'a series -> string option
(** [label series] returns the optional human-readable label attached to [series]. *)

type packed_series = Series : 'a series -> packed_series
type dependency = { series : packed_series; dependencies : dependency list; is_back_edge : bool }

val dependencies : 'a series -> dependency list
(** [dependencies series] returns the direct dependencies of [series] as a nested list. Back-edges
    into the current traversal path are marked with [is_back_edge] and have no further nested
    dependencies. *)

(** {1 Querying} *)

(* TODO: Temp top-level query API for extracting values. Remove once eval mechanism is in place. *)

type series_cache

exception Evaluation_did_not_converge of { iterations : int; tolerance : float; max_delta : float }
(** Raised when iterative cell evaluation does not converge within the built-in iteration limit. *)

val make_cache : unit -> series_cache
(** [make_cache ()] creates a fresh memoization cache for queries. A cache should be reused across
    related queries to benefit from memoization; it is not safe to share across threads. *)

val sum_float_opt : fill:float -> float option list -> float
(** [sum_float_opt ~fill values] sums [values], using [fill] for each [None]. Useful as a
    {!query_span} reduce function. *)

val query_span :
  series_cache -> Spans.t -> period:Period.t -> reduce:(float option list -> 'a) -> 'a
(** [query_span cache s ~period ~reduce] collects [s]'s values clipped to [period] (as
    [float option list], with [None] filling any gaps at the boundaries) and folds them with
    [reduce]. *)

val query_point : series_cache -> Points.t -> date:Date.t -> default:float -> float
(** [query_point cache s ~date ~default] returns [s]'s value at [date], or [default] if the series
    has no value there. *)
