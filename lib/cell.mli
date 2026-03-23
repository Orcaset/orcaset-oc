(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Single-period computation cells.

    A cell represents a single value associated with a time period. Cells form a graph structure
    where composite cells ({!deps}, {!map}, {!map2}) depend on other cells. Evaluation via {!eval}
    traverses the graph, caching results by cell identity and period so that shared sub-graphs are
    computed only once. Dependencies may be circular.

    The phantom type parameter ['c] tracks the currency (or unit) associated with a cell, e.g.
    [[\`USD] Cell.t].
*)

(** {1 Types} *)

type split_fn = Period.t -> (unit -> float) -> Date.t -> split_result * split_result
(** A function that splits a constant cell's value at a given date, producing two {!split_result}
    values covering the left and right sub-periods. *)

and split_result = { period : Period.t; f : unit -> float; split : split_fn }
(** The result of splitting a [Const] cell's value at a date. Contains the components needed to
    construct a new [Const] cell covering a sub-period. *)

type +'c t
(** An opaque single-period computation cell. The phantom type parameter ['c] tracks the currency or
    unit associated with the cell's value. *)

(** {1 Constructors} *)

val const : Period.t -> (unit -> float) -> split_fn -> 'c t
(** [const period f split] is a constant cell that evaluates [f ()] over [period]. [split] controls
    how the cell's value is divided when period alignment requires splitting (see
    {!proportional_split}). *)

val deps : Period.t -> 'c t list -> (float list -> float) -> 'c t
(** [deps period cells f] is a cell whose value is [f values] where [values] are the evaluated
    results of the dependency [cells]. *)

val map : 'c t -> (float -> float) -> 'c t
(** [map cell f] is a cell whose value is [f v] where [v] is the evaluated result of [cell]. *)

val map2 : 'c t option -> 'c t option -> (float option -> float option -> float) -> 'c t
(** [map2 c1 c2 f] combines two optional cells. At least one must be [Some]. *)

val cell_ref : Period.t -> 'c t
(** [cell_ref period] is a mutable indirection cell that starts unresolved ([None]). Use {!set_ref}
    to patch it to point at the actual cell once it has been constructed. During evaluation an
    unresolved [Ref] returns [0.0]; a resolved [Ref] delegates to the target cell. When the target
    (transitively) depends on this [Ref], evaluation will infinitely recurse — a fixed-point solver
    can intercept this to detect convergence. *)

val set_ref : 'c t -> 'c t -> unit
(** [set_ref ref_cell target] sets the target of a [Ref] cell.
    @raise Invalid_argument if [ref_cell] is not a [Ref]. *)

(** {1 Accessors} *)

val cell_id : 'c t -> int
(** [cell_id cell] is the unique integer identifier assigned to [cell] at construction time. *)

val cell_period : 'c t -> Period.t
(** [cell_period cell] is the time period covered by [cell]. For [Map] and [Map2] cells this is
    derived recursively from the inner cell(s). *)

(** {1 Evaluation} *)

val eval : 'c t -> Period.t * float
(** [eval cell] forces the computation tree rooted at [cell] and returns its period and float value.
    Results are cached by cell identity and period, so shared sub-trees are computed only once
    within a single [eval] call.

    Note: each call creates a fresh cache. When evaluating many cells that share dependencies,
    prefer {!eval_many} for cache sharing. *)

val eval_many : 'c t list -> (Period.t * float) list
(** [eval_many cells] evaluates a list of cells using a single shared cache, so common
    sub-dependencies are computed only once. Returns results in the same order as the input. *)

(** {1 Splitting} *)

val proportional_split : split_fn
(** A split function that divides a cell's value proportionally by the number of days on each side
    of the split date. *)

val split_cell : 'c t -> Date.t -> 'c t * 'c t
(** [split_cell cell date] splits [cell] at [date], producing two cells covering [\[start, date)]
    and [\[date, end)]. Split cells inherit their parent's identity for caching purposes. *)

val clip : 'c t -> Period.t -> 'c t
(** [clip cell period] returns a new cell with period start and end dates clipped to [period],
    splitting the cell using its interpolation method. *)

(** {1 Period alignment} *)

val iter_period_union : 'c t Seq.t -> 'c t Seq.t -> ('c t option * 'c t option) Seq.t
(** [iter_period_union a b] merges two cell sequences by aligning their periods. Cells are split as
    needed so that each emitted pair covers an identical sub-period. Unmatched cells are paired with
    [None]. *)

(** {1 Dependency analysis} *)

type 'c dep_tree =
  | Leaf of 'c t
  | Node of 'c t * 'c dep_tree list
  | Cycle of 'c t
      (** A tree representing the dependency structure of a cell. [Cycle] marks a back-edge to a
          cell that was already visited on the current path. *)

val dependency_tree : 'c t -> 'c dep_tree
(** [dependency_tree cell] is the dependency tree rooted at [cell]. Circular dependencies are
    detected via physical identity and represented as [Cycle] nodes. *)
