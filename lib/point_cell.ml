(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

open Cell_types

type 'c t = 'c Cell_types.point_cell

let const date value = TConst { id = Cell_types.fresh_id (); date; value }
let map inner f = TMap { id = Cell_types.fresh_id (); inner; f }
let convert inner f = TConvert { id = Cell_types.fresh_id (); inner; f }
let dep2 c1 c2 date f = TDep2 { id = Cell_types.fresh_id (); date; c1; c2; f }

(* Accessors *)

let id = function
  | TConst { id; _ } | TMap { id; _ } | TConvert { id; _ } | TDep2 { id; _ } | TRef { id; _ } -> id

let rec date : type c. c t -> Date.t = function
  | TConst { date; _ } | TDep2 { date; _ } | TRef { date; _ } -> date
  | TMap { inner; _ } -> date inner
  | TConvert { inner; _ } -> date inner
