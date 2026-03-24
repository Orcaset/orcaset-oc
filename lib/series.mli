(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** A lazy, period-indexed stream of {!Cell.t} values with explicit dependency tracking.

    The type ['c t] is abstract — construct series using the smart constructors {!const}, {!unfold},
    {!map}, and {!map2}. In particular, {!unfold} ensures that every dependency used inside the
    callback is declared up-front, so that {!dependencies} and {!pp_dot} always reflect the true
    dependency graph.

    The phantom type parameter ['c] tracks the currency (or unit) associated with the series,
    matching the currency of the cells it produces. *)

module Cell = Cell

type 'c query_fn = Period.t -> 'c Cell.t Seq.t
(** A function that retrieves the cells covering a given period. *)

type reduce = float list -> float
(** A function that collapses multiple cell values into a single float. Used by {!dep_query} to
    aggregate the (potentially multiple) cells returned by a period query. *)

type dep_query =
  | Self of { period : Period.t; reduce : reduce }
  | Dep of { index : int; period : Period.t; reduce : reduce }
      (** A declarative query against a dependency series or the series' own earlier output.

          - [Self { period; reduce }] queries the series' own cached output for [period]. Only periods
            that start strictly before the current frontier (the end date of the last produced cell) may
            be queried; querying at or beyond the frontier raises {!Forward_self_query}.
          - [Dep { index; period; reduce }] queries the dependency at position [index] in the [deps]
            list for [period].

          In both cases, [reduce] collapses the returned cells' values into a single float (e.g.
          {!reduce_sum}). *)

type 'c unfold_cell =
  | Seed of { period : Period.t; f : unit -> float }
  | Step of { period : Period.t; queries : dep_query list; f : float list -> float }
      (** A structured cell descriptor returned by the unfold sequence.

          - [Seed] is an initial/constant cell with no dependencies. Backed by {!Cell.const} with
            {!Cell.proportional_split}.
          - [Step] declares its dependency queries and a combining function [f] that receives one
            reduced float per query. The system resolves queries and builds {!Cell.deps}
            automatically, ensuring dependency connections are never erased. *)

type 'c t
(** An opaque time-series of cells denominated in currency ['c]. *)

(** {1 Exceptions} *)

exception
  Forward_self_query of { series_id : int; current_frontier : Date.t; query_period : Period.t }
(** Raised when an [Unfold] series' self-query function is called with a period whose start date is
    at or beyond the frontier (the end date of the last produced cell). The exception carries the
    series identifier, the frontier date, and the offending query period for diagnostics. *)

(** {1 Constructors} *)

val const : 'c Cell.t Seq.t -> 'c t
(** A series backed by a pre-built cell sequence. *)

val unfold : deps:'c t Lazy.t list -> 'c unfold_cell Seq.t -> 'c t
(** [unfold ~deps cells] builds a series from a declarative sequence of {!unfold_cell} values.

    Each element in [cells] is either a {!Seed} (constant, no dependencies) or a {!Step} (declares
    queries against [deps] or the series' own earlier output). The system resolves queries and
    constructs the underlying {!Cell.t} values automatically.

    Dependencies are wrapped in [Lazy.t] to support mutually recursive series definitions via
    [let rec]. The lazy values are only forced during evaluation (i.e. {!to_seq}), not at
    construction time.

    Because the callback never receives raw query functions or {!Cell.t} constructors, the only way
    to express dependencies is through {!dep_query} values in {!Step}, which are derived from the
    declared [deps]. This makes it impossible to silently bypass series-level dependency tracking or
    erase cell-level dependency connections. *)

val reduce_sum : reduce
(** A reduce function that sums all cell values: [List.fold_left (+.) 0.0]. *)

val map : (float -> float) -> 'c t Lazy.t -> 'c t
(** [map s f] applies [f] to each cell's float value. The dependency is wrapped in [Lazy.t] to
    support mutually recursive series definitions via [let rec]. The lazy value is forced during
    evaluation ({!to_seq}), not at construction. *)

val convert : (Period.t -> float -> float) -> 'a t Lazy.t -> 'b t
(** [convert s f] applies [f period v] to each cell's float value, where [period] is the cell's
    period and [v] is its evaluated result. Unlike {!map}, the phantom type of the result may differ
    from the input, allowing unit/currency conversions. The period argument lets conversion functions
    vary over time (e.g. time-varying exchange rates). The dependency is wrapped in [Lazy.t] to
    support mutually recursive series definitions via [let rec]. The lazy value is forced during
    evaluation ({!to_seq}), not at construction. *)

val map2 : (float option -> float option -> float) -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
(** [map2 s1 s2 f] combines two series cell-by-cell, aligning periods. Dependencies are wrapped in
    [Lazy.t] to support mutually recursive series definitions via [let rec]. The lazy values are
    forced during evaluation ({!to_seq}), not at construction. *)

val sum : 'c t -> 'c t -> 'c t
(** [sum s1 s2] adds two series using [map2] after aligning periods, treating missing cells as zero.
*)

val sub : 'c t -> 'c t -> 'c t
(** [sub s1 s2] subtracts two series using [map2] after aligning periods, treating missing cells as
    zero. *)

val mul : 'c t -> 'c t -> 'c t
(** [mul s1 s2] multiplies two series using [map2] after aligning periods, treating missing cells as
    zero. *)

val div : 'c t -> 'c t -> 'c t
(** [div s1 s2] divides two series using [map2] after aligning periods, treating missing cells as
    zero. *)

(** {1 Accessors} *)

val series_id : 'c t -> int
(** Return the unique integer identifier assigned to a series at construction time. Useful for
    debugging, logging, and DOT output. *)

(** {1 Evaluation} *)

val to_seq : 'c t list -> 'c Cell.t Seq.t list
(** Materialize a list of series into corresponding lazy cell sequences. All series in the list
    share a single evaluation cache, so common dependencies are computed only once. *)

(** {1 Dependency analysis} *)

val dependencies : 'c t -> 'c t list
(** Return all transitive dependencies of a series (including itself). Uses physical identity to
    detect cycles. *)

(** {1 DOT output} *)

val pp_dot : Format.formatter -> 'c t list -> unit
(** Pretty-print a DOT digraph of the dependency structure of one or more series. See the source for
    a usage example. 

    Usage:
    {[
      let oc = open_out "model.dot" in
      let ppf = Format.formatter_of_out_channel oc in
      Series.pp_dot ppf [ revenue; costs; profit ];
      Format.pp_print_flush ppf ();
      close_out oc
    ]} *)