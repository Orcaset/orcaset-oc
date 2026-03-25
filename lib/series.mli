(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Unified series module providing both period and point-in-time series with a shared evaluation
    cache. This module ties the knot between {!Period_series} and {!Point_series}, enabling
    cross-type dependencies without circular module imports.

    The dispatch pattern parallels {!Eval} at the cell level: {!Series_types} defines the shared
    types, {!Period_series} and {!Point_series} contain the logic, and this module provides the
    callbacks that route evaluation across the period/point boundary. *)

(** {1 Shared types} *)

type 'c series_dep = 'c Series_types.series_dep =
  | Period_dep of 'c Period_series.t Lazy.t
  | Point_dep of 'c Point_series.t Lazy.t

type reduce = Series_types.reduce

type dep_query = Series_types.dep_query =
  | Self of { period : Period.t; reduce : reduce }
  | Dep of { index : int; period : Period.t; reduce : reduce }
  | Point_dep of { index : int; date : Date.t }

type 'c unfold_cell = 'c Series_types.unfold_cell =
  | Seed of { period : Period.t; f : unit -> float }
  | Step of { period : Period.t; queries : dep_query list; f : float list -> float }

(** {1 Period series} *)

module Period : sig
  type 'c t = 'c Period_series.t

  type 'c unfold_cell = 'c Series_types.unfold_cell =
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

  val to_seq : 'c t list -> 'c Period_cell.t Seq.t list
  (** Materialize a list of series into corresponding lazy cell sequences. All series in the list
      share a single evaluation cache, so common dependencies are computed only once. *)
end

(** {1 Point series} *)

module Point : sig
  type 'c t = 'c Point_series.t

  val const : float -> 'c t
  val map : (float -> float) -> 'c t Lazy.t -> 'c t
  val convert : (Date.t -> float -> float) -> 'c t Lazy.t -> 'd t
  val id : 'c t -> int

  val query : Date.t -> 'c t -> 'c Point_cell.t option
  (** Retrieve the cell corresponding to a specific date. *)
end
