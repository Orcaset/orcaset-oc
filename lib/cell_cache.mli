(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Evaluation cache for period and point-in-time cells.

    Stores intermediate and final cell values during fixed-point iteration. Period cells are keyed
    by cell ID and period; point cells are keyed by cell ID.

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

(** {1 Period cell operations} *)

val find_period : t -> int -> Period.t -> (Period.t * cell_status) option
(** [find_period cache id period] looks up the status of a period cell [id] for [period]. Returns
    [Some (period, status)] on a hit, [None] on a miss. As a side effect, ensures the per-cell
    sub-table exists so that a subsequent {!store_period} for the same [id] does not need to
    re-check. *)

val store_period : t -> int -> Period.t -> cell_status -> unit
(** [store_period cache id period status] writes [status] into the cache for period cell [id] and
    [period]. *)

(** {1 Point cell operations} *)

val find_point : t -> int -> cell_status option
(** [find_point cache id] looks up the status of point cell [id]. Returns [Some status] on a hit,
    [None] on a miss. *)

val store_point : t -> int -> cell_status -> unit
(** [store_point cache id status] writes [status] into the cache for point cell [id]. *)

(** {1 Constants} *)

val max_iterations : int
(** Maximum number of fixed-point iterations before giving up. *)

val convergence_threshold : float
(** Absolute delta below which fixed-point iteration is considered converged. *)
