(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Series module providing both period and point-in-time series with compile-time currency/unit
    safety via phantom types. *)

(** {1 Shared types} *)

(** A numeric value tagged with phantom type ['c] (typically a currency or unit). The [[@@unboxed]]
    annotation ensures zero runtime overhead — the representation is identical to a bare [float]. *)
type 'c amount = Amount of float [@@unboxed]

(** Internal type aliases used to break the mutual reference between {!Period} and {!Point}
    submodules. Users should use {!Period.t} and {!Point.t} instead. *)

type 'c period_t
type 'c point_t

(** {1 Query and evaluation types} *)

type 'c q_result =
  | QRPeriod of { label : string; period : Period.t; cells : 'c Period_cell.t list }
  | QRPoint of { label : string; cell : 'c Point_cell.t option }
      (** The result of querying a series. [QRPeriod] holds the bounding query period and the
          materialized cells that fall within it. [QRPoint] holds the cell for a single date, or
          [None] if the series has no value there. *)

type 'c eval_result =
  | Period of { label : string; period : Period.t; value : 'c amount }
  | Point of { label : string; point : (Date.t * 'c amount) option }
      (** The result of evaluating a query result. [Period] collapses the cell list to a single
          value via sum reduction. [Point] resolves the cell to a dated value, or [None] if the
          query had no cell. The phantom type ['c] is preserved from the originating series so that
          callers can dispatch on it for currency-aware formatting. *)

type period = Period.t
(** The library {!Period.t} type, aliased so it remains accessible after the {!Period} submodule
    shadows the library module name. Used by {!Stmt.pp}. *)

(** {1 Period series} *)

module Period : sig
  type 'c t = 'c period_t
  type reduce = float list -> float
  type 'c dep
  type 'c point_dep
  type 'c cell

  (** {2 Dependency applicative} *)

  type ('a, 'c) deps
  (** A declarative description of the series dependencies needed by an {!unfold} cell builder. *)

  val no_deps : (unit, 'c) deps
  val dep_period : 'c t Lazy.t -> ('c dep, 'c) deps
  val dep_point : 'c point_t Lazy.t -> ('c point_dep, 'c) deps
  val both : ('a, 'c) deps -> ('b, 'c) deps -> ('a * 'b, 'c) deps
  val ( let+ ) : ('a, 'c) deps -> ('a -> 'b) -> ('b, 'c) deps
  val ( and+ ) : ('a, 'c) deps -> ('b, 'c) deps -> ('a * 'b, 'c) deps

  (** {2 Cell query applicative} *)

  module Query : sig
    type ('a, 'c) t

    val pure : 'a -> ('a, 'c) t
    val self : period:Period.t -> reduce:reduce -> (float, 'c) t
    val period : 'c dep -> period:period -> reduce:reduce -> (float, 'c) t
    val point : 'c point_dep -> date:Date.t -> (float option, 'c) t
    val point_or : default:float -> 'c point_dep -> date:Date.t -> (float, 'c) t
    val map : ('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
    val both : ('a, 'c) t -> ('b, 'c) t -> ('a * 'b, 'c) t
    val ( let+ ) : ('a, 'c) t -> ('a -> 'b) -> ('b, 'c) t
    val ( and+ ) : ('a, 'c) t -> ('b, 'c) t -> ('a * 'b, 'c) t
  end

  (** {2 Construction} *)

  val of_seq : label:string -> 'c Period_cell.t Seq.t -> 'c t
  val const : period:Period.t -> (unit -> float) -> 'c cell
  val step : period:Period.t -> ('a, 'c) Query.t -> ('a -> float) -> 'c cell

  val unfold : label:string -> deps:('deps, 'c) deps -> cells:('deps -> 'c cell Seq.t) -> 'c t
  (** [unfold ~label ~deps ~cells] constructs a series in two phases. *)

  val unfold_self : label:string -> cells:(unit -> 'c cell Seq.t) -> 'c t
  (** Convenience wrapper for {!unfold} when the cell builder has no external dependencies. *)

  val extend : label:string -> 'c t -> (Period.t -> 'c t) -> 'c t
  (** [extend ~label base cont] evaluates [base] (which must be finite), passes the last period to
      [cont], and returns the concatenation of both series. *)

  val reduce_sum : reduce
  val map : label:string -> (float -> float) -> 'c t Lazy.t -> 'c t
  val convert : label:string -> (Period.t -> float -> float) -> 'a t Lazy.t -> 'b t

  val map2 :
    label:string -> (float option -> float option -> float) -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t

  val const_ann_growth :
    label:string ->
    start:Date.t ->
    value:float ->
    rate:float ->
    offset:Offset.t ->
    yf:(Date.t -> Date.t -> float) ->
    'c t

  val sum : label:string -> 'c t Lazy.t list -> 'c t
  val sub : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
  val mul : label:string -> 'c t Lazy.t list -> 'c t
  val div : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t

  val filter :
    label:string -> ('c Period_cell.t Seq.t -> 'c Period_cell.t Seq.t) -> 'c t Lazy.t -> 'c t
  (** [filter ~label f s] applies a cell-sequence transformation [f] to the evaluated cells of [s].
  *)

  val after : label:string -> Date.t -> 'c t Lazy.t -> 'c t
  (** [after ~label date s] returns a series containing only cells that fall after [date]. *)

  val id : 'c t -> int
  val label : 'c t -> string

  val query : Period.t list -> 'c t -> 'c q_result list
  (** [query periods series] retrieves the cells corresponding to each period in [periods] for the
      given series. *)

  val to_seq : 'c t list -> 'c Period_cell.t Seq.t list
  (** Materialize a list of series into corresponding lazy cell sequences. *)
end

(** {1 Point series} *)

module Point : sig
  type 'c t = 'c point_t
  type reduce = float list -> float
  type 'c dep = 'c Period.dep
  type 'c point_dep = 'c Period.point_dep
  type 'c cell
  type ('a, 'c) deps = ('a, 'c) Period.deps

  (** {2 Dependency applicative} *)

  val no_deps : (unit, 'c) deps
  val dep_period : 'c period_t Lazy.t -> ('c dep, 'c) deps
  val dep_point : 'c point_t Lazy.t -> ('c point_dep, 'c) deps
  val both : ('a, 'c) deps -> ('b, 'c) deps -> ('a * 'b, 'c) deps
  val ( let+ ) : ('a, 'c) deps -> ('a -> 'b) -> ('b, 'c) deps
  val ( and+ ) : ('a, 'c) deps -> ('b, 'c) deps -> ('a * 'b, 'c) deps
  val const : label:string -> float -> 'c t
  (** [const ~label value] returns the same as-of value at every date. *)

  module Query : sig
    type ('a, 'c) t

    val pure : 'a -> ('a, 'c) t
    val self : date:Date.t -> (float option, 'c) t
    val self_or : default:float -> date:Date.t -> (float, 'c) t
    val period : 'c dep -> period:period -> reduce:reduce -> (float, 'c) t
    val point : 'c point_dep -> date:Date.t -> (float option, 'c) t
    val point_or : default:float -> 'c point_dep -> date:Date.t -> (float, 'c) t
    val map : ('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
    val both : ('a, 'c) t -> ('b, 'c) t -> ('a * 'b, 'c) t
    val ( let+ ) : ('a, 'c) t -> ('a -> 'b) -> ('b, 'c) t
    val ( and+ ) : ('a, 'c) t -> ('b, 'c) t -> ('a * 'b, 'c) t
  end

  val const_cell : period:period -> (unit -> float) -> 'c cell
  val step : period:period -> ('a, 'c) Query.t -> ('a -> float) -> 'c cell

  val unfold : label:string -> deps:('deps, 'c) deps -> cells:('deps -> 'c cell Seq.t) -> 'c t
  (** [unfold ~label ~deps ~cells] builds a point series from period-indexed change cells. Querying
      the resulting series at a date returns the cumulative clipped total through that date. Inside
      the cell builder, {!Query.self} references the constructing point series itself with these
      as-of semantics. *)

  val unfold_self : label:string -> cells:(unit -> 'c cell Seq.t) -> 'c t
  (** Convenience wrapper for {!unfold} when the cell builder has no external dependencies. *)

  val map : label:string -> (float -> float) -> 'c t Lazy.t -> 'c t
  val convert : label:string -> (Date.t -> float -> float) -> 'c t Lazy.t -> 'd t

  val map2 :
    label:string -> (float option -> float option -> float) -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
  (** Combine two series with a binary function. *)

  val neg : label:string -> 'c t Lazy.t -> 'c t

  val sum : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
  (** Elementwise addition. Missing values are filled with [0.0]. *)

  val sub : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
  (** Elementwise subtraction. Missing values are filled with [0.0]. *)

  val mul : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
  (** Elementwise multiplication. Missing values are filled with [0.0]. *)

  val div : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
  (** Elementwise division. Missing values are filled with [0.0]. *)

  val id : 'c t -> int
  val label : 'c t -> string

  val query : Date.t -> 'c t -> 'c q_result
  (** [query date series] retrieves the as-of cell for [date]. *)

  val query_many : Date.t list -> 'c t -> 'c q_result list
  (** [query_many dates series] retrieves one {!q_result} per date. *)
end

(** {1 Evaluation} *)

val eval : 'c q_result -> 'c eval_result
(** [eval qr] runs the fixed-point solver on the cells in [qr] and returns an {!eval_result}. *)

val eval_many : 'c q_result list -> 'c eval_result list
(** [eval_many qrs] evaluates a list of query results, sharing a single solver cache. *)

(** {1 Graph bridge} *)

val period_to_graph : 'c Period.t -> Graph.series
val point_to_graph : 'c Point.t -> Graph.series

(** {1 Statement formatting} *)

module Stmt : sig
  type t
  (** A statement tree of line items and groups. *)

  val period_line : 'c Period.t -> t
  (** [period_line series] is a single row showing the values of the period [series]. *)

  val point_line : 'c Point.t -> t
  (** [point_line series] is a single row showing the values of a point [series]. *)

  val period_total : 'c Period.t -> t list -> t
  (** [period_total series children] is a total whose children are displayed above a separator line
      followed by the total's own sum row. *)

  val point_total : 'c Point.t -> t list -> t
  (** [point_total series children] is a total whose children are displayed above a separator line
      followed by the total's own sum row. *)

  val group : t list -> t
  (** [group children] is a container that holds a list of lines, totals, or nested groups. *)

  type column_role =
    | Start_anchor
    | Period_end
  (** Describes whether a rendered column is the leading start-date anchor or a period-end column. *)

  type slot_kind =
    | Slot_empty
    | Slot_period of period
    | Slot_point of Date.t
  (** Distinguishes empty leading period slots from period and point values. *)

  type row_kind =
    | Row_line
    | Row_total
  (** Indicates whether a row is a simple line item or a total row. *)

  type series_kind =
    | Series_period
    | Series_point
  (** The kind of series backing a rendered row. *)

  type column = {
    id : string;
    label : string;
    date : Date.t;
    role : column_role;
  }
  (** Metadata for one rendered column. [id] is stable within a snapshot and suitable for external
      references. *)

  type slot = {
    column_id : string;
    kind : slot_kind;
    value : float option;
    cell_ids : string list;
  }
  (** One visible value in the rendered statement. [cell_ids] points into the exported [cell] list so
      callers can drill into dependencies. *)

  type row = {
    id : string;
    parent_id : string option;
    depth : int;
    kind : row_kind;
    label : string;
    series_kind : series_kind;
    series_runtime_id : int;
    slots : slot list;
  }
  (** A rendered statement row. [id] is deterministic for a given statement tree shape. *)

  type cell_kind =
    | Cell_period
    | Cell_point
  (** Indicates whether an exported cell belongs to a period or point series. *)

  type cell_op =
    | Cell_const
    | Cell_deps
    | Cell_map
    | Cell_convert
    | Cell_map2
    | Cell_clip
    | Cell_ref
  (** The underlying Orcaset cell constructor used to build an exported cell. *)

  type cell = {
    id : string;
    runtime_id : int;
    kind : cell_kind;
    op : cell_op;
    value : float;
    period : period option;
    date : Date.t option;
    dep_ids : string list;
  }
  (** A normalized exported cell. [id] is unique within the snapshot and includes span metadata so
      split cells do not collide. *)

  type snapshot = {
    version : int;
    columns : column list;
    rows : row list;
    cells : cell list;
  }
  (** A single rendered statement plus the normalized cell registry needed for drill-down. *)

  type statement_snapshot = {
    id : string;
    rows : row list;
  }
  (** One statement inside a multi-statement snapshot. *)

  type model_snapshot = {
    version : int;
    columns : column list;
    statements : statement_snapshot list;
    cells : cell list;
  }
  (** Multiple rendered statements sharing one normalized cell registry. *)

  val snapshot : t -> period list -> snapshot
  (** [snapshot stmt periods] renders [stmt] into structured rows, columns, slots, and reachable
      cells without formatting numbers as strings. *)

  val snapshot_many : (string * t) list -> period list -> model_snapshot
  (** [snapshot_many statements periods] snapshots multiple statements against a shared cell cache and
      deduplicated cell registry. Each statement id is used to namespace row ids in the result. *)

  val snapshot_to_json : snapshot -> string
  (** [snapshot_to_json snapshot] serializes a structured statement snapshot to compact JSON. *)

  val model_snapshot_to_json : model_snapshot -> string
  (** [model_snapshot_to_json snapshot] serializes a multi-statement snapshot to compact JSON. *)

  val pp : t -> period list -> string
  (** [pp stmt periods] pretty-prints the statement as a fixed-width table. *)
end
