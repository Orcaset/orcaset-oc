(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

type series_id

val new_id : unit -> series_id
(** [new_id ()] returns a fresh series id. Required when constructing series variants manually. *)

type split
(** Strategy for assigning value when a span is clipped to a sub-period. *)

val proportional_split : split
(** Splits a span's value proportionally by time. E.g. clipping a $120/yr span to one quarter yields
    $30 for that quarter. *)

val const_split : split
(** Assigns the original span's value to each clipped side. E.g. clipping a "40 mph" span to any
    sub-period still yields 40 mph. *)

type step
(** A single cell emitted by an [Unfold]. Construct with {!step}. *)

val step : period:Period.t -> split:split -> (unit -> float) -> step
(** [step ~period ~split value] creates a cell for [period] whose value is produced lazily by
    [value], using [split] to apportion the value when the cell is clipped. When [value] references
    other series, those series must be declared in the enclosing [Unfold]'s [deps] applicative so
    that the corresponding readers can be used inside the closure. Recursive reads should happen
    inside [value]; reads performed while constructing the step are scaffold-time reads and can
    observe placeholder values. *)

module rec Span_series : sig
  type t =
    | Const of { id : series_id; period : Period.t; value : unit -> float }
    | Map of { id : series_id; dep : t; f : float -> float }
    | Map2 of { id : series_id; a : t; b : t; f : float option -> float option -> float }
    | Unfold : {
        id : series_id;
        deps : unit -> 'readers Deps.t;
        init : 'b;
        step : 'readers -> 'b -> (step * 'b) option;
      }
        -> t

  val neg : t -> t
  (** Negates each value of the dependency. Implemented with {!Map}; where the dependency has no
      value, behavior matches {!Map} on that dependency (this constructor does not fill missing
      values itself). *)

  val scale : float -> t -> t
  (** [scale k s] multiplies each value of [s] by [k]. Implemented with {!Map}; missing values
      follow [s] as for {!Map}. *)

  val sum : t -> t -> t
  (** Element-wise sum using {!Map2}. Each missing operand ([None]) is treated as [0.0] before
      adding. *)

  val sub : t -> t -> t
  (** Element-wise difference [a -. b] using {!Map2}. Each missing operand ([None]) is treated as
      [0.0] before subtracting. *)
end

and Point_series : sig
  type t =
    | Const of { id : series_id; period : Period.t; value : unit -> float }
    | Map of { id : series_id; dep : t; f : float -> float }
    | Map2 of { id : series_id; a : t; b : t; f : float option -> float option -> float }
    | Accum of { id : series_id; init : float; changes : Span_series.t }

  val neg : t -> t
  (** Negates each value of the dependency. Implemented with {!Map}; where the dependency has no
      value, behavior matches {!Map} on that dependency (this constructor does not fill missing
      values itself). *)

  val scale : float -> t -> t
  (** [scale k s] multiplies each value of [s] by [k]. Implemented with {!Map}; missing values
      follow [s] as for {!Map}. *)

  val sum : t -> t -> t
  (** Element-wise sum using {!Map2}. Each missing operand ([None]) is treated as [0.0] before
      adding. *)

  val sub : t -> t -> t
  (** Element-wise difference [a -. b] using {!Map2}. Each missing operand ([None]) is treated as
      [0.0] before subtracting. *)
end

and Deps : sig
  (** Applicative for declaring an [Unfold]'s dependencies. A value of type ['a t] is a computation
      that declares zero or more span/point dependencies and, given the corresponding readers,
      produces an ['a]. The [Unfold.step] function receives the computation's result (typically a
      labeled tuple of readers). *)

  (* type span_reader *)

  type span_reader = period:Period.t -> reduce:(float option list -> float) -> float
  (** A reader bound to a declared span dependency. Calling it inside the step function collects the
      dep's values over [period] (as [float option list]; [None] for gaps) and folds them with
      [reduce]. *)

  (* type point_reader *)

  type point_reader = date:Date.t -> default:float -> float
  (** A reader bound to a declared point dependency. Calling it inside the step function returns the
      dep's value at [date], or [default] if the series has no value there. *)

  type _ t
  (** Applicative computation. Build with [none], [span_dep], [point_dep], or the [let+]/[and+]
      operators. *)

  val none : unit t
  (** Empty dependency set. Use when an [Unfold]'s [step] needs no readers. *)

  val span_dep : Span_series.t -> span_reader t
  (** Declare a span dependency; the produced value is the [span_reader] bound to it. *)

  val point_dep : Point_series.t -> point_reader t
  (** Declare a point dependency; the produced value is the [point_reader] bound to it. *)

  val reduce : (float -> float -> float) -> float -> float option list -> float
  (** [reduce op fill] folds a list of [float option] with [op], starting from [0.0], using [fill]
      when an element is [None]. *)

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

type _ series =
  | Point_series : Point_series.t -> [ `Point ] series
  | Span_series : Span_series.t -> [ `Span ] series

type packed_series = Series : 'a series -> packed_series
type dependency = { series : packed_series; dependencies : dependency list; is_back_edge : bool }

exception Resolution_failed of { iterations : int; tolerance : float; max_delta : float }
(** Raised by query functions when recursive cell values do not converge. *)

val dependencies : 'a series -> dependency list
(** [dependencies series] returns the direct dependencies of [series] as a nested list. Back-edges
    into the current traversal path are marked with [is_back_edge] and have no further nested
    dependencies. *)

(** {1 Querying} *)

(* TODO: Temp top-level query API for extracting values. Remove once eval mechanism is in place. *)

type series_cache

val make_cache : unit -> series_cache
(** [make_cache ()] creates a fresh memoization cache for queries. A cache should be reused across
    related queries to benefit from memoization; it is not safe to share across threads. *)

val query_span :
  series_cache -> Span_series.t -> period:Period.t -> reduce:(float option list -> 'a) -> 'a
(** [query_span cache s ~period ~reduce] collects [s]'s values clipped to [period] (as
    [float option list], with [None] filling any gaps at the boundaries), resolves recursive cells
    by Gauss-Seidel iteration, and folds them with [reduce]. Raises [Resolution_failed] if the
    induced scaffold does not converge. *)

val query_point : series_cache -> Point_series.t -> date:Date.t -> default:float -> float
(** [query_point cache s ~date ~default] returns [s]'s value at [date], or [default] if the series
    has no value there. Raises [Resolution_failed] if the induced scaffold does not converge. *)
