(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Fixed-point solver for cell evaluation.

    Implements iterative evaluation over a dependency graph of period and point-in-time cells.
    Handles both cell types through the {!cell} sum type, computing values for all cell variants. *)

type cell = PeriodCell : 'c Period_cell.t -> cell | PointCell : 'c Point_cell.t -> cell

val one : cell -> float
(** [one cell] forces the computation tree rooted at [cell] and returns its float value. Creates a
    fresh cache, primes the dependency tree, iterates until convergence, and reads the result.

    Note: each call creates a fresh cache. When evaluating many cells that share dependencies,
    prefer {!many} for cache sharing. *)

val many : cell list list -> float list list
(** [many groups] evaluates grouped lists of cells using a single shared cache, so common
    sub-dependencies are computed only once. Returns results in the same shape as the input — one
    float list per group. *)
