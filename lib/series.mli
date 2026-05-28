(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

type split = Split.t
(** Strategy for assigning value when a span is split. See {!Split}. *)

type span_cell_id
type point_cell_id
type span_cell
type point_cell

type span_segment =
  | Present of span_cell
  | Missing of Period.t
      (** Materialized span coverage for a query period. [Present cell] is a concrete formula-backed
          cell; [Missing period] is uncovered query time. *)

type 'key key_ops = {
  equal : 'key -> 'key -> bool;
  hash : 'key -> int;
  compare : 'key -> 'key -> int;
  to_string : 'key -> string;
}
(** Stable identity operations for keyed dynamic families. *)

module Agg : sig
  type sample = { period : Period.t; value : float }
  (** One defined span sample collected for aggregation. Missing query coverage and undefined cell
      values are represented by [None] in aggregation inputs. *)

  type t

  val make : (sample option list -> float option) -> t
  val reduce : t -> sample option list -> float option
  val sum : t
  val min : t
  val max : t
  val average : t
  val time_weighted_average : (Date.t -> Date.t -> float) -> t
end

module Family : sig
  type ('key, 'series) t
  (** A keyed dynamic collection of series. Membership can depend on query periods, but not on
      solved values. *)

  val make :
    id:string ->
    key:'key key_ops ->
    active_keys:(Period.t -> 'key list) ->
    member:('key -> 'series) ->
    ('key, 'series) t

  val members : ('key, 'series) t -> Period.t -> ('key * 'series) list
  val series : ('key, 'series) t -> Period.t -> 'series list
  val id : ('key, 'series) t -> string
  val key_equal : ('key, 'series) t -> 'key -> 'key -> bool
  val key_compare : ('key, 'series) t -> 'key -> 'key -> int
  val key_to_string : ('key, 'series) t -> 'key -> string
end

module Trace : sig
  type span_cell_info = { id : span_cell_id; period : Period.t; series_label : string option }
  type point_cell_info = { id : point_cell_id; date : Date.t; series_label : string option }
  type cell = Span_cell of span_cell_info | Point_cell of point_cell_info

  type query =
    | Span_query of { period : Period.t; label : string option }
    | Point_query of { date : Date.t; label : string option }
    | Span_cell_value
    | Point_cell_value

  type edge = { from : cell; to_ : cell; query : query; is_back_edge : bool }
  type t

  val roots : t -> cell list
  val edges : t -> edge list
end

module rec Spans : sig
  type unfold_cell

  type +'tag t
  (** Span series carrying a phantom semantic tag. The tag is compile-time metadata only. *)

  type packed
  (** Existentially packed span series for APIs that need heterogeneous input tags. *)

  val pack : 'tag t -> packed
  val neg : ?label:string -> 'in_tag t -> 'out_tag t
  val scale : ?label:string -> float -> 'in_tag t -> 'out_tag t
  val sum : ?label:string -> agg:Agg.t -> packed list -> 'out_tag t
  val sub : ?label:string -> agg:Agg.t -> 'a_tag t -> 'b_tag t -> 'out_tag t
  val mul : ?label:string -> agg:Agg.t -> packed list -> 'out_tag t
  val div : ?label:string -> agg:Agg.t -> 'a_tag t -> 'b_tag t -> 'out_tag t
  val sum_family : ?label:string -> agg:Agg.t -> ('key, 'tag t) Family.t -> 'out_tag t
  val const : ?label:string -> split:split -> agg:Agg.t -> period:Period.t -> float -> 'tag t
  val of_list : ?label:string -> split:split -> agg:Agg.t -> (Period.t * float) list -> 'tag t
  val map : ?label:string -> (float -> float) -> 'in_tag t -> 'out_tag t

  val map2 :
    ?label:string ->
    agg:Agg.t ->
    'a_tag t ->
    'b_tag t ->
    (float option -> float option -> float option) ->
    'out_tag t

  val mapn :
    ?label:string -> agg:Agg.t -> packed list -> (float option list -> float option) -> 'out_tag t

  val extend : agg:Agg.t -> 'tag t -> 'tag t -> 'tag t
  val clipped : after:Date.t -> until:Date.t -> 'tag t -> 'tag t
  val after : Date.t -> 'tag t -> 'tag t
  val until : Date.t -> 'tag t -> 'tag t

  val unfold :
    ?label:string ->
    agg:Agg.t ->
    init:'state ->
    cells:('state -> (unfold_cell * 'state) option) ->
    unit ->
    'tag t

  val unfold_from :
    ?label:string ->
    agg:Agg.t ->
    cells:(Period.t -> (unfold_cell * Period.t) option) ->
    'tag t ->
    'tag t

  val unfold_rec :
    ?label:string ->
    agg:Agg.t ->
    init:'state ->
    cells:(self:'tag t -> 'state -> (unfold_cell * 'state) option) ->
    unit ->
    'tag t

  val cell : period:Period.t -> split:split -> float option Formula.t -> unfold_cell
  val label : 'tag t -> string option
  val agg : 'tag t -> Agg.t
  val with_agg : agg:Agg.t -> 'tag t -> 'tag t
end

and Points : sig
  type +'tag t
  type packed

  val pack : 'tag t -> packed
  val neg : ?label:string -> 'in_tag t -> 'out_tag t
  val scale : ?label:string -> float -> 'in_tag t -> 'out_tag t
  val sum : ?label:string -> packed list -> 'out_tag t
  val sub : ?label:string -> 'a_tag t -> 'b_tag t -> 'out_tag t
  val mul : ?label:string -> packed list -> 'out_tag t
  val div : ?label:string -> 'a_tag t -> 'b_tag t -> 'out_tag t
  val const : ?label:string -> period:Period.t -> float -> 'tag t
  val of_list : ?label:string -> (Date.t * float) list -> 'tag t
  val map : ?label:string -> (float -> float) -> 'in_tag t -> 'out_tag t

  val map2 :
    ?label:string ->
    'a_tag t ->
    'b_tag t ->
    (float option -> float option -> float option) ->
    'out_tag t

  val mapn : ?label:string -> packed list -> (float option list -> float option) -> 'out_tag t
  val accum : ?label:string -> init:float -> 'change_tag Spans.t -> 'out_tag t
  val label : 'tag t -> string option
end

and Formula : sig
  type 'a t

  type packed_query =
    | Span_query_item : { series : 'tag Spans.t; period : Period.t } -> packed_query
    | Point_query_item : { series : 'tag Points.t; date : Date.t } -> packed_query
    | Span_cell_value_item : span_cell_id -> packed_query
    | Point_cell_value_item : point_cell_id -> packed_query

  val pure : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val span_query : 'tag Spans.t -> period:Period.t -> float option t
  val point_query : 'tag Points.t -> date:Date.t -> float option t
  val span_cell_value : span_cell_id -> float option t
  val point_cell_value : point_cell_id -> float option t
  val queries : 'a t -> packed_query list
end

type span_kind
type point_kind

type (_, _) series =
  | Point_series : 'tag Points.t -> (point_kind, 'tag) series
  | Span_series : 'tag Spans.t -> (span_kind, 'tag) series

val label : ('kind, 'tag) series -> string option

type packed_series = Series : ('kind, 'tag) series -> packed_series

(** {1 Querying} *)

type series_cache

exception Evaluation_did_not_converge of { iterations : int; tolerance : float; max_delta : float }

val make_cache : unit -> series_cache
val materialize_span : series_cache -> 'tag Spans.t -> period:Period.t -> span_segment list
val query_span_samples : series_cache -> 'tag Spans.t -> period:Period.t -> Agg.sample option list
val query_span : series_cache -> 'tag Spans.t -> period:Period.t -> float option
val query_point : series_cache -> 'tag Points.t -> date:Date.t -> float option
val trace_span : series_cache -> 'tag Spans.t -> period:Period.t -> Trace.t
val trace_point : series_cache -> 'tag Points.t -> date:Date.t -> Trace.t
