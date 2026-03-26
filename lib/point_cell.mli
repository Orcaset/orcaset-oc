(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

type +'c t = Cell_types.point_cell

(** {1 Constructors} *)

val const : Date.t -> float -> 'c t
val map : 'c t -> (float -> float) -> 'c t
val convert : 'a t -> (Date.t -> float -> float) -> 'b t
val dep2 : 'c t option -> 'c t option -> Date.t -> (float option -> float option -> float) -> 'c t
val accum : Cell_types.period_cell Seq.t -> Date.t -> (float -> float) -> 'c t

(** {1 Accessors} *)

val id : 'c t -> int
val date : 'c t -> Date.t

(** {1 Priming} *)

val prime : Cell_types.prime_fn -> Cell_cache.t -> 'c t -> unit
(** [prime prime_tree cache cell] initialises the cache entry for [cell] and recursively primes its
    dependencies via the [prime_tree] callback. *)

(** {1 Evaluation} *)

val compute : Cell_types.eval_fn -> Cell_cache.t -> int -> 'c t -> float * float
(** [compute eval_cell cache iteration cell] computes the value of [cell] for a single evaluation
    step. [eval_cell] is used to recursively evaluate dependencies. *)
