(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

type split_fn = Period.t -> (unit -> float) -> Date.t -> split_result * split_result
and split_result = { period : Period.t; f : unit -> float; split : split_fn }

type 'c period_cell =
  | RConst of { id : int; period : Period.t; f : unit -> float; split : split_fn }
  | RDeps of { id : int; period : Period.t; deps : cell list; f : float list -> float }
  | RMap of { id : int; inner : 'c period_cell; f : float -> float }
  | RConvert : {
      id : int;
      inner : 'a period_cell;
      f : Period.t -> float -> float;
    }
      -> 'b period_cell
  | RMap2 of {
      id : int;
      c1 : 'c period_cell option;
      c2 : 'c period_cell option;
      f : float option -> float option -> float;
    }
  | RClip of { id : int; inner : 'c period_cell; period : Period.t }
  | RRef of { id : int; period : Period.t; mutable cell : 'c period_cell option }

and 'c point_cell =
  | TConst of { id : int; date : Date.t; value : float }
  | TMap of { id : int; inner : 'c point_cell; f : float -> float }
  | TConvert : { id : int; inner : 'a point_cell; f : Date.t -> float -> float } -> 'b point_cell
  | TDep2 of {
      id : int;
      date : Date.t;
      c1 : 'c point_cell option;
      c2 : 'c point_cell option;
      f : float option -> float option -> float;
    }
  | TAccum of {
      id : int;
      date : Date.t;
      prev : 'c point_cell option;
      changes : 'c period_cell Seq.t;
      f : float -> float;
    }

and cell = PeriodCell : 'c period_cell -> cell | PointCell : 'c point_cell -> cell

type eval_fn = Cell_cache.t -> int -> cell -> float * float
type prime_fn = Cell_cache.t -> cell -> unit

let next_id = Atomic.make 0
let fresh_id () = Atomic.fetch_and_add next_id 1
