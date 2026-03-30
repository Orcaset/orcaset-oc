(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Scoped series module providing both period and point-in-time series with compile-time scope
    isolation. Each call to {!Make} produces a fresh module whose series types are incompatible with
    those from other calls, preventing accidental cross-model mixing.

    The dispatch pattern parallels {!Eval} at the cell level: {!Series_types} defines the shared
    types, {!Period_series} and {!Point_series} contain the logic, and each functor instance
    provides the callbacks that route evaluation across the period/point boundary. *)

(** {1 Shared types} *)

type reduce = float list -> float

type dep_query =
  | Self of { period : Period.t; reduce : reduce }
  | Dep of { index : int; period : Period.t; reduce : reduce }
  | Point_dep of { index : int; date : Date.t }

type 'c unfold_cell =
  | Seed of { period : Period.t; f : unit -> float }
  | Step of { period : Period.t; queries : dep_query list; f : float list -> float }

exception Duplicate_label of { label : string; existing_series_id : int }

(** {1 Scoped series module} *)

module type S = sig
  type 'c period_t
  type 'c point_t
  type 'c series_dep = Period_dep of 'c period_t Lazy.t | Point_dep of 'c point_t Lazy.t

  (** {1 Period series} *)

  module Period : sig
    type 'c t = 'c period_t

    type nonrec 'c unfold_cell = 'c unfold_cell =
      | Seed of { period : Period.t; f : unit -> float }
      | Step of { period : Period.t; queries : dep_query list; f : float list -> float }

    val const : label:string -> 'c Period_cell.t Seq.t -> 'c t
    val unfold : label:string -> deps:'c series_dep list -> 'c unfold_cell Seq.t -> 'c t
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

    val sum : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
    val sub : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
    val mul : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
    val div : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
    val id : 'c t -> int

    val query : Period.t -> 'c t list -> 'c Period_cell.t Seq.t list
    (** Retrieve the cells corresponding to a specific period. *)

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

    val id : 'c t -> int

    val query : Date.t -> 'c t -> 'c Point_cell.t option
    (** Retrieve the cell corresponding to a specific date. *)

    val query_many : Date.t list -> 'c t -> 'c Point_cell.t option list
    (** Retrieve cells for multiple dates from a single series, sharing an evaluation cache across
        all queries. *)
  end

  (** {1 Label inspection} *)

  val labels : unit -> string list
  (** Return all registered labels in this scope. *)

  (** {1 Graph bridge} *)

  val period_to_graph : 'c Period.t -> Graph.series
  val point_to_graph : 'c Point.t -> Graph.series
end

module Make () : S
