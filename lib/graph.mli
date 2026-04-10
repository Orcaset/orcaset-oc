(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Dependency analysis and graph output for cells and series.

    Provides unified dependency tree traversal for both period and point cells, transitive
    dependency collection for series, and DOT graph output. *)

(** {1 Cell dependency trees} *)

type cell_dep_tree =
  | Leaf of Cell_types.cell
  | Node of Cell_types.cell * cell_dep_tree list
  | Cycle of Cell_types.cell
      (** A tree representing the dependency structure of a cell. [Cycle] marks a back-edge to a
          cell that was already visited on the current path. *)

val cells : Cell_types.cell -> cell_dep_tree
(** [cells cell] is the dependency tree rooted at [cell]. Handles both period and point cells.
    Circular dependencies are detected via physical identity and represented as [Cycle] nodes. *)

(** {1 Series dependencies} *)

type series =
  | PeriodSeries : _ Series_types.period_series -> series
  | PointSeries : _ Series_types.point_series -> series
      (** A handle to a series for dependency analysis and graph output. The constructors reference
          private types and cannot be used outside the library — use {!Series.period_to_graph} and
          {!Series.point_to_graph} to create values of this type. *)

val series : series -> series list
(** Return all transitive series dependencies (including the root itself). Handles both period and
    point series. Uses physical identity to detect cycles. *)

(** {1 DOT output} *)

val pp_dot : Format.formatter -> series list -> unit
(** Pretty-print a DOT digraph of the dependency structure of one or more series. Handles both
    period and point series dependencies within the graph. Node labels are derived from the series
    labels assigned at construction time. Edges point from dependencies to dependents. *)
