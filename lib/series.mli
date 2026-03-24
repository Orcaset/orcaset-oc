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

val unfold : deps:'c t Lazy.t list -> ('c query_fn -> 'c query_fn list -> 'c Cell.t Seq.t) -> 'c t
(** [unfold ~deps f] builds a series whose cells are produced by [f].

    [f] receives two arguments:
    - [self_query : query_fn] — queries the series' own (cached) output for earlier periods. Only
      periods that start strictly before the current frontier (the end date of the last produced
      cell) may be queried. Querying a period whose start date is at or beyond the frontier raises
      {!Forward_self_query}. When no cells have been produced yet, the self-query returns an empty
      sequence.
    - [dep_queries : query_fn list] — one query function per element of [deps], in the same order.

    Dependencies are wrapped in [Lazy.t] to support mutually recursive series definitions via
    [let rec]. The lazy values are only forced during evaluation (i.e. {!to_seq}), not at
    construction time.

    Because [f] never receives a [Series.t] value, the only way to access another series' cells
    inside [f] is through the provided query functions, which are derived from the declared [deps].
    This makes it impossible to silently bypass series-level dependency tracking. *)

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