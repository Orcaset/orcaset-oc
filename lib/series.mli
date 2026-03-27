(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Unified series module providing both period and point-in-time series with a shared evaluation
    cache. This module ties the knot between {!Period_series} and {!Point_series}, enabling
    cross-type dependencies without circular module imports.

    The dispatch pattern parallels {!Eval} at the cell level: {!Series_types} defines the shared
    types, {!Period_series} and {!Point_series} contain the logic, and this module provides the
    callbacks that route evaluation across the period/point boundary. *)

(** {1 Shared types} *)

type reduce = float list -> float

type dep_query =
  | Self of { period : Period.t; reduce : reduce }
  | Dep of { index : int; period : Period.t; reduce : reduce }
  | Point_dep of { index : int; date : Date.t }

type 'c unfold_cell =
  | Seed of { period : Period.t; f : unit -> float }
  | Step of { period : Period.t; queries : dep_query list; f : float list -> float }

type 'c period_t
type 'c point_t
type 'c series_dep = Period_dep of 'c period_t Lazy.t | Point_dep of 'c point_t Lazy.t

(** {1 Period series} *)

module Period : sig
  type 'c t = 'c period_t

  type nonrec 'c unfold_cell = 'c unfold_cell =
    | Seed of { period : Period.t; f : unit -> float }
    | Step of { period : Period.t; queries : dep_query list; f : float list -> float }

  val const : 'c Period_cell.t Seq.t -> 'c t
  val unfold : deps:'c series_dep list -> 'c unfold_cell Seq.t -> 'c t
  val reduce_sum : reduce
  val map : (float -> float) -> 'c t Lazy.t -> 'c t
  val convert : (Period.t -> float -> float) -> 'a t Lazy.t -> 'b t
  val map2 : (float option -> float option -> float) -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
  val sum : 'c t -> 'c t -> 'c t
  val sub : 'c t -> 'c t -> 'c t
  val mul : 'c t -> 'c t -> 'c t
  val div : 'c t -> 'c t -> 'c t
  val id : 'c t -> int

  val query : Period.t -> 'c t list -> 'c Period_cell.t Seq.t list
  (** Retrieve the cell corresponding to a specific period. *)

  val to_seq : 'c t list -> 'c Period_cell.t Seq.t list
  (** Materialize a list of series into corresponding lazy cell sequences. All series in the list
      share a single evaluation cache, so common dependencies are computed only once. *)
end

(** {1 Point series} *)

module Point : sig
  type 'c t = 'c point_t

  val const : float -> 'c t
  val map : (float -> float) -> 'c t Lazy.t -> 'c t
  val convert : (Date.t -> float -> float) -> 'c t Lazy.t -> 'd t
  val accum : start_date:Date.t -> initial_value:float -> 'c period_t Lazy.t -> 'c t
  val id : 'c t -> int

  val query : Date.t -> 'c t -> 'c Point_cell.t option
  (** Retrieve the cell corresponding to a specific date. *)

  val query_many : Date.t list -> 'c t -> 'c Point_cell.t option list
  (** Retrieve cells for multiple dates from a single series, sharing an evaluation cache across all
      queries. *)
end
