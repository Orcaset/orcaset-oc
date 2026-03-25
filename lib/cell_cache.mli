(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Evaluation cache for period cells.

    Stores intermediate and final cell values during fixed-point iteration. The cache is keyed by
    cell ID and period, supporting cells that have been split into sub-periods with distinct values.

    {1 Types} *)

type cell_status =
  | Resolved of float  (** Final value that will not change. *)
  | Unresolved of float * int
      (** Current iteration guess and the last iteration number in which the cell was updated. Used
          as a Gauss-Seidel guard to avoid redundant re-evaluation within a single sweep. *)

type t
(** An opaque evaluation cache. *)

(** {1 Construction} *)

val create : unit -> t
(** [create ()] is a fresh, empty cache. *)

(** {1 Operations} *)

val find : t -> int -> Period.t -> (Period.t * cell_status) option
(** [find cache id period] looks up the status of cell [id] for [period]. Returns
    [Some (period, status)] on a hit, [None] on a miss. As a side effect, ensures the per-cell
    sub-table exists so that a subsequent {!store} for the same [id] does not need to re-check. *)

val store : t -> int -> Period.t -> cell_status -> unit
(** [store cache id period status] writes [status] into the cache for cell [id] and [period]. *)

(** {1 Constants} *)

val max_iterations : int
(** Maximum number of fixed-point iterations before giving up. *)

val convergence_threshold : float
(** Absolute delta below which fixed-point iteration is considered converged. *)
