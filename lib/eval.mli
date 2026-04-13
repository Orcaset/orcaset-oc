(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Internal fixed-point solver for cell evaluation.

    Implements iterative evaluation over a dependency graph of period and point-in-time cells. This
    module is used internally by {!Series} — users should not call these functions directly. *)

exception Non_convergence of {
  iterations : int;
  delta : float;
  threshold : float;
}
(** Raised when fixed-point iteration exhausts the configured iteration limit without converging. *)

val prime_tree : Cell_cache.t -> Cell_types.cell -> unit
(** [prime_tree cache cell] walks the dependency tree rooted at [cell], initialising each node in
    [cache] with either a resolved constant or an unresolved placeholder. *)

val iterate : Cell_cache.t -> Cell_types.cell list -> int -> unit
(** [iterate cache roots iteration] runs the fixed-point solver from [iteration] until convergence
    or raises if the maximum iteration count is reached without convergence. *)

val read_result : Cell_cache.t -> Cell_types.cell -> float
(** [read_result cache cell] extracts the final float value for [cell] from [cache]. Raises if the
    cell was never primed. *)
