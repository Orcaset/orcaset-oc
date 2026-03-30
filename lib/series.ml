(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Scoped series functor that ties the knot between period and point series evaluation.

    Parallels {!Eval} at the cell level: creates the shared cache and provides the callbacks that
    route evaluation across the period/point boundary. Each call to {!Make} produces a fresh module
    with abstract types, preventing accidental cross-scope mixing at compile time. *)

type reduce = Series_types.reduce

type dep_query = Series_types.dep_query =
  | Self of { period : Period.t; reduce : reduce }
  | Dep of { index : int; period : Period.t; reduce : reduce }
  | Point_dep of { index : int; date : Date.t }

type 'c unfold_cell = 'c Series_types.unfold_cell =
  | Seed of { period : Period.t; f : unit -> float }
  | Step of { period : Period.t; queries : dep_query list; f : float list -> float }

exception Duplicate_label = Series_types.Duplicate_label

module type S = sig
  type 'c period_t
  type 'c point_t
  type 'c series_dep = Period_dep of 'c period_t Lazy.t | Point_dep of 'c point_t Lazy.t

  module Period : sig
    type 'c t = 'c period_t

    type nonrec 'c unfold_cell = 'c unfold_cell =
      | Seed of { period : Period.t; f : unit -> float }
      | Step of { period : Period.t; queries : dep_query list; f : float list -> float }

    val const : label:string -> 'c Period_cell.t Seq.t -> 'c t
    val unfold : label:string -> deps:'c series_dep list -> 'c unfold_cell Seq.t -> 'c t
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

    val sum : label:string -> 'c t -> 'c t -> 'c t
    val sub : label:string -> 'c t -> 'c t -> 'c t
    val mul : label:string -> 'c t -> 'c t -> 'c t
    val div : label:string -> 'c t -> 'c t -> 'c t
    val id : 'c t -> int
    val query : Period.t -> 'c t list -> 'c Period_cell.t Seq.t list
    val to_seq : 'c t list -> 'c Period_cell.t Seq.t list
  end

  module Point : sig
    type 'c t = 'c point_t

    val const : label:string -> float -> 'c t
    val map : label:string -> (float -> float) -> 'c t Lazy.t -> 'c t
    val convert : label:string -> (Date.t -> float -> float) -> 'c t Lazy.t -> 'd t

    val accum :
      label:string -> start_date:Date.t -> initial_value:float -> 'c period_t Lazy.t -> 'c t

    val id : 'c t -> int
    val query : Date.t -> 'c t -> 'c Point_cell.t option
    val query_many : Date.t list -> 'c t -> 'c Point_cell.t option list
  end

  val labels : unit -> string list
  val period_to_graph : 'c Period.t -> Graph.series
  val point_to_graph : 'c Point.t -> Graph.series
end

module Make () = struct
  (* Label store — maps label string to series id for diagnostics *)
  let label_store : (string, int) Hashtbl.t = Hashtbl.create 16

  let register_label id label =
    match Hashtbl.find_opt label_store label with
    | Some existing_id ->
        raise (Series_types.Duplicate_label { label; existing_series_id = existing_id })
    | None -> Hashtbl.add label_store label id

  let labels () = Hashtbl.fold (fun label _ acc -> label :: acc) label_store []

  (* Types — representationally equal to Series_types *)
  type 'c period_t = 'c Period_series.t
  type 'c point_t = 'c Point_series.t

  type 'c series_dep = 'c Series_types.series_dep =
    | Period_dep of 'c Period_series.t Lazy.t
    | Point_dep of 'c Point_series.t Lazy.t

  (* Mutually recursive eval callbacks — same as before *)
  let rec eval_point cache date point_series =
    Point_series.eval_query ~eval_period cache point_series date

  and eval_period cache period_series period =
    Period_series.eval_query ~eval_point cache period period_series

  module Period = struct
    type 'c t = 'c period_t

    type 'c unfold_cell = 'c Series_types.unfold_cell =
      | Seed of { period : Period.t; f : unit -> float }
      | Step of { period : Period.t; queries : dep_query list; f : float list -> float }

    let const ~label cells =
      let s = Period_series.const cells in
      register_label (Period_series.id s) label;
      s

    let unfold ~label ~deps cells =
      let s = Period_series.unfold ~deps cells in
      register_label (Period_series.id s) label;
      s

    let reduce_sum = Period_series.reduce_sum

    let map ~label f inner =
      let s = Period_series.map f inner in
      register_label (Period_series.id s) label;
      s

    let convert ~label f inner =
      let s = Period_series.convert f inner in
      register_label (Period_series.id s) label;
      s

    let map2 ~label f s1 s2 =
      let s = Period_series.map2 f s1 s2 in
      register_label (Period_series.id s) label;
      s

    let const_ann_growth ~label ~start ~value ~rate ~offset ~yf =
      let s = Period_series.const_ann_growth ~start ~value ~rate ~offset ~yf in
      register_label (Period_series.id s) label;
      s

    let sum ~label s1 s2 =
      let s = Period_series.sum s1 s2 in
      register_label (Period_series.id s) label;
      s

    let sub ~label s1 s2 =
      let s = Period_series.sub s1 s2 in
      register_label (Period_series.id s) label;
      s

    let mul ~label s1 s2 =
      let s = Period_series.mul s1 s2 in
      register_label (Period_series.id s) label;
      s

    let div ~label s1 s2 =
      let s = Period_series.div s1 s2 in
      register_label (Period_series.id s) label;
      s

    let id = Period_series.id

    let query period series_list =
      let cache = Series_types.create_cache () in
      List.map (Period_series.eval_query ~eval_point cache period) series_list

    let to_seq series_list =
      let cache = Series_types.create_cache () in
      List.map (Period_series.eval_seq ~eval_point cache) series_list
  end

  module Point = struct
    type 'c t = 'c point_t

    let const ~label value =
      let s = Point_series.const value in
      register_label (Point_series.id s) label;
      s

    let map ~label f inner =
      let s = Point_series.map f inner in
      register_label (Point_series.id s) label;
      s

    let convert ~label f inner =
      let s = Point_series.convert f inner in
      register_label (Point_series.id s) label;
      s

    let accum ~label ~start_date ~initial_value changes =
      let s = Point_series.accum ~start_date ~initial_value changes in
      register_label (Point_series.id s) label;
      s

    let id = Point_series.id

    let query date series =
      let cache = Series_types.create_cache () in
      Point_series.eval_query ~eval_period cache series date

    let query_many dates series =
      let cache = Series_types.create_cache () in
      List.map (fun date -> Point_series.eval_query ~eval_period cache series date) dates
  end

  (* Graph bridge functions *)
  let period_to_graph (s : 'c Period.t) : Graph.series = Graph.pack_period s
  let point_to_graph (s : 'c Point.t) : Graph.series = Graph.pack_point s
end
