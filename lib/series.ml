(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Dispatch module that ties the knot between period and point series evaluation.

    Parallels {!Eval} at the cell level: creates the shared cache and provides the callbacks that
    route evaluation across the period/point boundary. *)

type 'c series_dep = 'c Series_types.series_dep =
  | Period_dep of 'c Period_series.t Lazy.t
  | Point_dep of 'c Point_series.t Lazy.t

type reduce = Series_types.reduce

type dep_query = Series_types.dep_query =
  | Self of { period : Period.t; reduce : reduce }
  | Dep of { index : int; period : Period.t; reduce : reduce }
  | Point_dep of { index : int; date : Date.t }

type 'c unfold_cell = 'c Series_types.unfold_cell =
  | Seed of { period : Period.t; f : unit -> float }
  | Step of { period : Period.t; queries : dep_query list; f : float list -> float }

(* The callback that period_series uses to resolve point dependencies *)
let eval_point cache date point_series = Point_series.eval_query cache point_series date

module Period = struct
  include Period_series

  type 'c unfold_cell = 'c Series_types.unfold_cell =
    | Seed of { period : Period.t; f : unit -> float }
    | Step of { period : Period.t; queries : dep_query list; f : float list -> float }

  let to_seq series_list =
    let cache = Series_types.create_cache () in
    List.map (Period_series.eval_seq ~eval_point cache) series_list
end

module Point = struct
  include Point_series

  let query date series =
    let cache = Series_types.create_cache () in
    Point_series.eval_query cache series date
end
