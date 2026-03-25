(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Fixed-point solver for cell evaluation.

    Implements Gauss-Seidel iteration over a dependency graph of period and point-in-time cells.
    Handles both cell types through the {!Cell_types.cell} sum type, dispatching to the appropriate
    cache (period or point) and computing values for all cell variants.

    {1 Period cell evaluation} *)

val eval_period : 'c Period_cell.t -> Period.t * float
(** [eval_period cell] forces the computation tree rooted at [cell] and returns its period and float
    value. Creates a fresh cache, primes the dependency tree, iterates until convergence, and reads
    the result.

    Note: each call creates a fresh cache. When evaluating many cells that share dependencies,
    prefer {!eval_period_many} for cache sharing. *)

val eval_period_many : 'c Period_cell.t list -> (Period.t * float) list
(** [eval_period_many cells] evaluates a list of period cells using a single shared cache, so common
    sub-dependencies are computed only once. Returns results in the same order as the input. *)

(** {1 Point cell evaluation} *)

val eval_point : 'c Point_cell.t -> Date.t * float
(** [eval_point cell] forces the computation tree rooted at [cell] and returns its date and float
    value. Creates a fresh cache, primes the dependency tree, iterates until convergence, and reads
    the result.

    Note: each call creates a fresh cache. When evaluating many cells that share dependencies,
    prefer {!eval_point_many} for cache sharing. *)

val eval_point_many : 'c Point_cell.t list -> (Date.t * float) list
(** [eval_point_many cells] evaluates a list of point cells using a single shared cache, so common
    sub-dependencies are computed only once. Returns results in the same order as the input. *)
