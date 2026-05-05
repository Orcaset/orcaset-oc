(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

type part = Part of { value : float -> float }
type t = period:Period.t -> date:Date.t -> part * part

let part ~value = Part { value }
let value (Part { value }) original = value original

let weighted year_frac : t =
 fun ~period ~date ->
  let start, end_ = Period.to_tuple period in
  let denominator = year_frac start end_ in
  let left_ratio = year_frac start date /. denominator in
  let right_ratio = year_frac date end_ /. denominator in
  (part ~value:(fun value -> value *. left_ratio), part ~value:(fun value -> value *. right_ratio))

let daily = weighted (fun start end_ -> Float.of_int (Date.diff end_ start))
let const : t = fun ~period:_ ~date:_ -> (part ~value:Fun.id, part ~value:Fun.id)
let cmonthly = weighted Yf.cmonthly
let act_360 = weighted Yf.act_360
