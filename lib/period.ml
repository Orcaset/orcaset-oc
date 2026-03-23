(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

type t = { start_date : Date.t; end_date : Date.t }

let make start_date end_date = { start_date; end_date }
let to_tuple p = (p.start_date, p.end_date)

(* Accessors *)

let start_date p = p.start_date
let end_date p = p.end_date
let days p = Date.diff p.end_date p.start_date
let contains date period = Date.(date >= period.start_date && date < period.end_date)

let shift offset period =
  { start_date = Date.shift offset period.start_date; end_date = Date.shift offset period.end_date }

let compare p0 p1 =
  let c = Date.compare p0.start_date p1.start_date in
  if c <> 0 then c else Date.compare p0.end_date p1.end_date

let equal p0 p1 = compare p0 p1 = 0
let hash p = Hashtbl.hash (Date.hash p.start_date, Date.hash p.end_date)
let to_string p = Date.to_string p.start_date ^ ".." ^ Date.to_string p.end_date
let pp fmt p = Format.pp_print_string fmt (to_string p)

let make_seq ~start_date ~offset =
  let end_date = Date.shift offset start_date in
  let initial_period = { start_date; end_date } in
  Seq.unfold
    (fun period ->
      let next_period = shift offset period in
      Some (period, next_period))
    initial_period
