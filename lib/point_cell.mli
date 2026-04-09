(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

type 'c t = 'c Cell_types.point_cell

(** {1 Constructors} *)

val const : Date.t -> float -> 'c t
val map : 'c t -> (float -> float) -> 'c t
val convert : 'a t -> (Date.t -> float -> float) -> 'b t
val dep2 : 'c t option -> 'c t option -> Date.t -> (float option -> float option -> float) -> 'c t
val accum : ?prev:'c t -> 'c Period_cell.t Seq.t -> Date.t -> (float -> float) -> 'c t

(** {1 Accessors} *)

val id : 'c t -> int
val date : 'c t -> Date.t
