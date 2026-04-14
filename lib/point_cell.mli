(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

type 'c t = 'c Cell_types.point_cell

(** {1 Constructors} *)

val const : Date.t -> float -> 'c t
val map : 'c t -> (float -> float) -> 'c t
val convert : 'a t -> (Date.t -> float -> float) -> 'b t
val deps : Date.t -> Cell_types.cell list -> (float list -> float) -> 'c t

(** {1 Accessors} *)

val id : 'c t -> int
val date : 'c t -> Date.t
