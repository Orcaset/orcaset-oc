(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

type t = { start : Date.t; end_ : Date.t }

let make start end_ = { start; end_ }
let to_tuple p = (p.start, p.end_)

(* Accessors *)

let start p = p.start
let end_ p = p.end_
let days p = Date.diff p.end_ p.start
let contains date period = Date.(date >= period.start && date < period.end_)

(* Shifting *)

let next offset period =
  let derived_date = Date.shift offset (end_ period) in
  if Date.(derived_date < end_ period) then { start = derived_date; end_ = end_ period }
  else { start = end_ period; end_ = derived_date }

let prev offset period =
  let derived_date = Date.shift offset (start period) in
  if Date.(derived_date > start period) then { start = start period; end_ = derived_date }
  else { start = derived_date; end_ = start period }

let shift offset period =
  { start = Date.shift offset period.start; end_ = Date.shift offset period.end_ }

(* Sequences *)

let make_seq ~start ~offset =
  let end_ = Date.shift offset start in
  let initial_period = { start; end_ } in
  Seq.unfold
    (fun period ->
      let next_period = next offset period in
      Some (period, next_period))
    initial_period

let seq_to_dates periods =
  let rec aux last remaining () =
    match remaining () with
    | Seq.Nil -> (
        match last with
        | None -> Seq.Nil
        | Some last_period -> Seq.Cons (end_ last_period, Seq.empty))
    | Seq.Cons (period, next) -> (
        match last with
        | None -> Seq.Cons (start period, aux (Some period) next)
        | Some last_period ->
            if Date.equal (end_ last_period) (start period) then
              Seq.Cons (end_ last_period, aux (Some period) next)
            else
              Seq.Cons (end_ last_period, fun () -> Seq.Cons (start period, aux (Some period) next))
        )
  in
  aux None periods

let list_to_dates periods =
  let rec aux last remaining =
    match remaining with
    | [] -> ( match last with None -> [] | Some last_period -> [ end_ last_period ])
    | period :: next -> (
        match last with
        | None -> start period :: aux (Some period) next
        | Some last_period ->
            if Date.equal (end_ last_period) (start period) then
              end_ last_period :: aux (Some period) next
            else end_ last_period :: start period :: aux (Some period) next)
  in
  aux None periods
(* Predicates and comparisons *)

let equal p0 p1 = compare p0 p1 = 0
let hash p = Hashtbl.hash (Date.hash p.start, Date.hash p.end_)
let to_string p = Date.to_string p.start ^ ".." ^ Date.to_string p.end_
let pp fmt p = Format.pp_print_string fmt (to_string p)
