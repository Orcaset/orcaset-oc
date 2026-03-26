(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

let next_id = Atomic.make 0
let fresh_id () = Atomic.fetch_and_add next_id 1

type reduce = float list -> float

type dep_query =
  | Self of { period : Period.t; reduce : reduce }
  | Dep of { index : int; period : Period.t; reduce : reduce }
  | Point_dep of { index : int; date : Date.t }

type 'c unfold_cell =
  | Seed of { period : Period.t; f : unit -> float }
  | Step of { period : Period.t; queries : dep_query list; f : float list -> float }

type 'c period_series =
  | PConst of { id : int; cells : 'c Period_cell.t Seq.t }
  | PUnfold of { id : int; deps : 'c series_dep list; cells : 'c unfold_cell Seq.t }
  | PMap of { id : int; inner : 'c period_series Lazy.t; f : float -> float }
  | PConvert of { id : int; inner : 'c period_series Lazy.t; f : Period.t -> float -> float }
  | PMap2 of {
      id : int;
      s1 : 'c period_series Lazy.t;
      s2 : 'c period_series Lazy.t;
      f : float option -> float option -> float;
    }

and 'c point_series =
  | TConst of { id : int; value : float }
  | TMap of { id : int; inner : 'c point_series Lazy.t; f : float -> float }
  | TConvert of { id : int; inner : 'c point_series Lazy.t; f : Date.t -> float -> float }
  | TAccum of {
      id : int;
      start_date : Date.t;
      initial_value : float;
      changes : 'c period_series Lazy.t;
    }

and 'c series_dep = Period_dep of 'c period_series Lazy.t | Point_dep of 'c point_series Lazy.t

type 'c series = PeriodSeries of 'c period_series | PointSeries of 'c point_series

type cache = {
  period : (int, Cell_types.period_cell Seq.t) Hashtbl.t;
  point : (int * Date.t, Cell_types.point_cell) Hashtbl.t;
}

let create_cache () = { period = Hashtbl.create 16; point = Hashtbl.create 16 }

exception
  Forward_self_query of { series_id : int; current_frontier : Date.t; query_period : Period.t }
