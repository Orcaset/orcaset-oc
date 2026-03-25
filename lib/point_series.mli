(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

type 'c query_fn = Date.t -> 'c Point_cell.t
(** A function that retrieves the cell at a given date. *)

type 'c t
(** An opaque time-series of cells denominated in currency ['c]. *)

(** {1 Constructors} *)

val const : float -> 'c t
(** Create a series that produces the same value at every date. *)

val map : (float -> float) -> 'c t Lazy.t -> 'c t
(** Transform the output of a series by applying a function to each cell's value. *)

val convert : (Date.t -> float -> float) -> 'c t Lazy.t -> 'd t
(** Convert the output of a series from one unit to another by applying a function to each cell's value.
    The cell date is passed as an argument to the conversion function. *)

(** {1 Accessors} *)

val id : 'c t -> int
(** Retrieve the unique identifier of the series. *)

(** {1 Query} *)

val query : Date.t -> 'c t -> 'c Point_cell.t option
(** Retrieve the cell corresponding to a specific date. If the cell does not exist in the series,
    either because it is before the series starts or after it ends, [None] is returned. *)

(** {1 Dependencies}*)

val dependencies : 'c t -> 'c t list
(** Return all transitive dependencies of a series (including itself). Uses physical identity to
    detect cycles. *)