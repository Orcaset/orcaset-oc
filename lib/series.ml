(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Dispatch module that ties the knot between period and point series evaluation.

    Parallels {!Eval} at the cell level: creates the shared cache and provides the callbacks that
    route evaluation across the period/point boundary. *)

type reduce = Series_types.reduce
type 'c period_t = 'c Period_series.t
type 'c point_t = 'c Point_series.t

type 'c series_dep = 'c Series_types.series_dep =
  | Period_dep of 'c Period_series.t Lazy.t
  | Point_dep of 'c Point_series.t Lazy.t

type dep_query = Series_types.dep_query =
  | Self of { period : Period.t; reduce : reduce }
  | Dep of { index : int; period : Period.t; reduce : reduce }
  | Point_dep of { index : int; date : Date.t }

type 'c unfold_cell = 'c Series_types.unfold_cell =
  | Seed of { period : Period.t; f : unit -> float }
  | Step of { period : Period.t; queries : dep_query list; f : float list -> float }

(* Mutually recursive callbacks that tie the knot between period and point evaluation *)
let rec eval_point cache date point_series =
  Point_series.eval_query ~eval_period cache point_series date

and eval_period cache period_series period =
  Period_series.eval_query ~eval_point cache period period_series

module Period = struct
  let const = Period_series.const
  let unfold = Period_series.unfold
  let reduce_sum = Period_series.reduce_sum
  let map = Period_series.map
  let convert = Period_series.convert
  let map2 = Period_series.map2
  let const_ann_growth = Period_series.const_ann_growth
  let sum = Period_series.sum
  let sub = Period_series.sub
  let mul = Period_series.mul
  let div = Period_series.div
  let id = Period_series.id

  type nonrec 'c t = 'c period_t

  type 'c unfold_cell = 'c Series_types.unfold_cell =
    | Seed of { period : Period.t; f : unit -> float }
    | Step of { period : Period.t; queries : dep_query list; f : float list -> float }

  let query period series_list =
    let cache = Series_types.create_cache () in
    List.map (Period_series.eval_query ~eval_point cache period) series_list

  let to_seq series_list =
    let cache = Series_types.create_cache () in
    List.map (Period_series.eval_seq ~eval_point cache) series_list
end

module Point = struct
  let const = Point_series.const
  let map = Point_series.map
  let convert = Point_series.convert
  let accum = Point_series.accum
  let id = Point_series.id

  type nonrec 'c t = 'c point_t

  let query date series =
    let cache = Series_types.create_cache () in
    Point_series.eval_query ~eval_period cache series date

  let query_many dates series =
    let cache = Series_types.create_cache () in
    List.map (fun date -> Point_series.eval_query ~eval_period cache series date) dates
end
