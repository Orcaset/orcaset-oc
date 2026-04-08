(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Scoped series module providing both period and point-in-time series with compile-time scope
    isolation. Each call to {!Make} produces a fresh module whose series types are incompatible with
    those from other calls, preventing accidental cross-model mixing. *)

(** {1 Shared types} *)

(** A numeric value tagged with phantom type ['c] (typically a currency or unit). The [[@@unboxed]]
    annotation ensures zero runtime overhead — the representation is identical to a bare [float]. *)
type 'c amount = Amount of float [@@unboxed]

exception Duplicate_label of { label : string; existing_series_id : int }

(** {1 Scoped series module} *)

module type S = sig
  type 'c period_t
  type 'c point_t

  (** {1 Query and evaluation types} *)

  type 'c q_result =
    | QRPeriod of { label : string; period : Period.t; cells : 'c Period_cell.t list }
    | QRPoint of { label : string; cell : 'c Point_cell.t option }
        (** The result of querying a series. [QRPeriod] holds the bounding query period and the
            materialized cells that fall within it. [QRPoint] holds the cell for a single date, or
            [None] if the series has no value there. *)

  type 'c eval_result =
    | Period of { label : string; period : Period.t; value : 'c amount }
    | Point of { label : string; point : (Date.t * 'c amount) option }
        (** The result of evaluating a query result. [Period] collapses the cell list to a single
            value via sum reduction. [Point] resolves the cell to a dated value, or [None] if the
            query had no cell. The phantom type ['c] is preserved from the originating series so
            that callers can dispatch on it for currency-aware formatting. *)

  (** {1 Period series} *)

  module Period : sig
    type 'c t = 'c period_t
    type reduce = float list -> float
    type 'c dep
    type 'c point_dep
    type 'c cell

    (** {2 Dependency applicative} *)

    type ('a, 'c) deps
    (** A declarative description of the series dependencies needed by an {!unfold} cell builder.
        Compose with {!dep_period}, {!dep_point}, {!both}, and the binding-operator aliases [let+] /
        [and+]. The resulting value is interpreted internally by {!unfold} — the user never has
        access to a registration context, so dependencies cannot be added outside the declaration.
    *)

    val no_deps : (unit, 'c) deps
    val dep_period : 'c t Lazy.t -> ('c dep, 'c) deps
    val dep_point : 'c point_t Lazy.t -> ('c point_dep, 'c) deps
    val both : ('a, 'c) deps -> ('b, 'c) deps -> ('a * 'b, 'c) deps
    val ( let+ ) : ('a, 'c) deps -> ('a -> 'b) -> ('b, 'c) deps
    val ( and+ ) : ('a, 'c) deps -> ('b, 'c) deps -> ('a * 'b, 'c) deps

    (** {2 Cell query applicative} *)

    module Query : sig
      type ('a, 'c) t
      (** A declarative description of the cell-local data needed to compute one {!step}. Queries
          are combined applicatively and then lowered to explicit cell dependencies internally. *)

      val pure : 'a -> ('a, 'c) t
      val self : period:Period.t -> reduce:reduce -> (float, 'c) t
      val period : 'c dep -> period:Period.t -> reduce:reduce -> (float, 'c) t
      val point : 'c point_dep -> date:Date.t -> (float option, 'c) t
      val point_or : default:float -> 'c point_dep -> date:Date.t -> (float, 'c) t
      val map : ('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
      val both : ('a, 'c) t -> ('b, 'c) t -> ('a * 'b, 'c) t
      val ( let+ ) : ('a, 'c) t -> ('a -> 'b) -> ('b, 'c) t
      val ( and+ ) : ('a, 'c) t -> ('b, 'c) t -> ('a * 'b, 'c) t
    end

    (** {2 Construction} *)

    val of_seq : label:string -> 'c Period_cell.t Seq.t -> 'c t
    val const : period:Period.t -> (unit -> float) -> 'c cell
    val step : period:Period.t -> ('a, 'c) Query.t -> ('a -> float) -> 'c cell

    val unfold : label:string -> deps:('deps, 'c) deps -> cells:('deps -> 'c cell Seq.t) -> 'c t
    (** [unfold ~label ~deps ~cells] constructs a series in two phases. [deps] is an applicative
        description of the series dependencies built with {!dep_period} / {!dep_point} and
        combinators; [unfold] interprets it to extract both the dependency list and a value of type
        ['deps] (for example a tuple or labeled tuple of handles) that is passed to the [cells]
        builder. The builder returns abstract cells constructed with {!const}, {!step}, and the
        {!Query} applicative, so periods remain explicit while cell-local dependency wiring stays
        declarative and inspectable. *)

    val unfold_self : label:string -> cells:(unit -> 'c cell Seq.t) -> 'c t
    (** Convenience wrapper for {!unfold} when the cell builder has no external period or point
        series dependencies. Equivalent to [unfold ~label ~deps:no_deps ~cells]. *)

    val extend : label:string -> 'c t -> (Period.t -> 'c t) -> 'c t
    (** [extend ~label base cont] evaluates [base] (which must be finite), passes the last period to
        [cont], and returns the concatenation of both series. *)

    val reduce_sum : reduce
    val map : label:string -> (float -> float) -> 'c t Lazy.t -> 'c t
    val convert : label:string -> (Period.t -> float -> float) -> 'a t Lazy.t -> 'b t

    val map2 :
      label:string -> (float option -> float option -> float) -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t

    val const_ann_growth :
      label:string ->
      start:Date.t ->
      value:float ->
      rate:float ->
      offset:Offset.t ->
      yf:(Date.t -> Date.t -> float) ->
      'c t

    val sum : label:string -> 'c t Lazy.t list -> 'c t
    val sub : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
    val mul : label:string -> 'c t Lazy.t list -> 'c t
    val div : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
    val id : 'c t -> int
    val label : 'c t -> string

    val query : Period.t list -> 'c t -> 'c q_result list
    (** [query periods series] retrieves the cells corresponding to each period in [periods] for the
        given series, returning one {!q_result} per period. All periods share a single evaluation
        cache. *)

    val to_seq : 'c t list -> 'c Period_cell.t Seq.t list
    (** Materialize a list of series into corresponding lazy cell sequences. All series in the list
        share a single evaluation cache, so common dependencies are computed only once. *)
  end

  (** {1 Point series} *)

  module Point : sig
    type 'c t = 'c point_t

    val const : label:string -> float -> 'c t
    val map : label:string -> (float -> float) -> 'c t Lazy.t -> 'c t
    val convert : label:string -> (Date.t -> float -> float) -> 'c t Lazy.t -> 'd t

    val accum :
      label:string -> start_date:Date.t -> initial_value:float -> 'c period_t Lazy.t -> 'c t

    val map2 :
      label:string -> (float option -> float option -> float) -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
    (** Combine two series with a binary function. Each input is passed as [float option] — [None]
        when the underlying series has no value at the query date. *)

    val neg : label:string -> 'c t Lazy.t -> 'c t

    val sum : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
    (** Elementwise addition. Missing values are filled with [0.0]. *)

    val sub : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
    (** Elementwise subtraction. Missing values are filled with [0.0]. *)

    val mul : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
    (** Elementwise multiplication. Missing values are filled with [0.0]. *)

    val div : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
    (** Elementwise division. Missing values are filled with [0.0]. *)

    val id : 'c t -> int
    val label : 'c t -> string

    val query : Date.t -> 'c t -> 'c q_result
    (** [query date series] retrieves the cell for [date], returning a {!q_result}. *)

    val query_many : Date.t list -> 'c t -> 'c q_result list
    (** [query_many dates series] retrieves one {!q_result} per date, sharing an evaluation cache
        across all queries. *)
  end

  (** {1 Evaluation} *)

  val eval : 'c q_result -> 'c eval_result
  (** [eval qr] runs the fixed-point solver on the cells in [qr] and returns an {!eval_result}. For
      period query results, the cell values are sum-reduced to a single ['c amount]. For point query
      results, the cell is resolved to a dated value (or [None] if absent). The phantom type ['c] is
      preserved so callers can dispatch on the currency or unit tag. *)

  val eval_many : 'c q_result list -> 'c eval_result list
  (** [eval_many qrs] evaluates a list of query results, sharing a single solver cache across all of
      them so that common dependencies are computed only once. Equivalent to mapping {!eval} over
      the list, but more efficient when the query results share underlying cells. *)

  (** {1 Label inspection} *)

  val labels : unit -> string list
  (** Return all registered labels in this scope. *)

  (** {1 Graph bridge} *)

  val period_to_graph : 'c Period.t -> Graph.series
  val point_to_graph : 'c Point.t -> Graph.series
end

module Make () : S
