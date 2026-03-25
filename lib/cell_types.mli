(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Shared type definitions for period and point-in-time cells.

    Types are concrete (not abstract) so that downstream modules can pattern match on cell variants.
    Period cell constructors are prefixed with [R], point cell constructors with [T]. *)

type split_fn = Period.t -> (unit -> float) -> Date.t -> split_result * split_result
and split_result = { period : Period.t; f : unit -> float; split : split_fn }

type period_cell =
  | RConst of { id : int; period : Period.t; f : unit -> float; split : split_fn }
  | RDeps of { id : int; period : Period.t; deps : period_cell list; f : float list -> float }
  | RMap of { id : int; inner : period_cell; f : float -> float }
  | RConvert of { id : int; inner : period_cell; f : Period.t -> float -> float }
  | RMap2 of {
      id : int;
      c1 : period_cell option;
      c2 : period_cell option;
      f : float option -> float option -> float;
    }
  | RRef of { id : int; period : Period.t; mutable cell : period_cell option }

and point_cell =
  | TConst of { id : int; date : Date.t; value : float }
  | TMap of { id : int; inner : point_cell; f : float -> float }
  | TConvert of { id : int; inner : point_cell; f : Date.t -> float -> float }
  | TDep2 of {
      id : int;
      date : Date.t;
      c1 : point_cell option;
      c2 : point_cell option;
      f : float option -> float option -> float;
    }

type cell = PeriodCell of period_cell | PointCell of point_cell
type eval_fn = Cell_cache.t -> int -> cell -> float * float
type prime_fn = Cell_cache.t -> cell -> unit

val fresh_id : unit -> int
