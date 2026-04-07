(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Shared type definitions for period and point-in-time series.

    Types are concrete (not abstract) so that downstream modules can pattern match on series
    variants. This module parallels {!Cell_types} at the series level. *)

val fresh_id : unit -> int

type reduce = float list -> float

type 'c dep_query =
  | Self_query of { period : Period.t; reduce : reduce }
  | Period_query of { dep : 'c period_series Lazy.t; period : Period.t; reduce : reduce }
  | Point_query of { dep : 'c point_series Lazy.t; date : Date.t }
  | Point_present_query of { dep : 'c point_series Lazy.t; date : Date.t }

and 'c unfold_cell =
  | Const of { period : Period.t; f : unit -> float }
  | Step of { period : Period.t; queries : 'c dep_query list; f : float list -> float }

and 'c period_series =
  | POfSeq of { id : int; cells : 'c Period_cell.t Seq.t }
  | PUnfold of { id : int; deps : 'c series_dep list; cells : 'c unfold_cell Seq.t }
  | PMap of { id : int; inner : 'c period_series Lazy.t; f : float -> float }
  | PConvert : {
      id : int;
      inner : 'a period_series Lazy.t;
      f : Period.t -> float -> float;
    }
      -> 'b period_series
  | PMap2 of {
      id : int;
      s1 : 'c period_series Lazy.t;
      s2 : 'c period_series Lazy.t;
      f : float option -> float option -> float;
    }
  | PExtend of { id : int; base : 'c period_series; cont : Period.t -> 'c period_series }

and 'c point_series =
  | TConst of { id : int; value : float }
  | TMap of { id : int; inner : 'c point_series Lazy.t; f : float -> float }
  | TConvert : {
      id : int;
      inner : 'a point_series Lazy.t;
      f : Date.t -> float -> float;
    }
      -> 'b point_series
  | TDep2 of {
      id : int;
      s1 : 'c point_series Lazy.t;
      s2 : 'c point_series Lazy.t;
      f : float option -> float option -> float;
    }
  | TAccum of {
      id : int;
      start_date : Date.t;
      initial_value : float;
      changes : 'c period_series Lazy.t;
    }

and 'c series_dep = Period_dep of 'c period_series Lazy.t | Point_dep of 'c point_series Lazy.t

type 'c series = PeriodSeries of 'c period_series | PointSeries of 'c point_series
type packed_period_seq = Pack_period_seq : 'c Period_cell.t Seq.t -> packed_period_seq
type packed_point_cell = Pack_point_cell : 'c Point_cell.t -> packed_point_cell

type cache = {
  period : (int, packed_period_seq) Hashtbl.t;
  point : (int * Date.t, packed_point_cell) Hashtbl.t;
  prefix : (int, packed_period_seq) Hashtbl.t;
}

val create_cache : unit -> cache

exception Duplicate_label of { label : string; existing_series_id : int }
