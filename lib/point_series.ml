(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

open Series_types

type 'c t = 'c point_series

(* SAFETY: These casts only change the phantom type parameter 'c, which has no
   runtime representation — all point_cell and point_series variants carry only
   int, float, Date.t, functions, and nested values of the same GADT family.
   Used to store/retrieve values from the existentially-typed cache
   (Pack_point_cell), which erases 'c on insertion and needs it restored on lookup. *)
let cast_point_cell : type a b. a Point_cell.t -> b Point_cell.t = Obj.magic
let cast_point_series : type a b. a t -> b t = Obj.magic

type 'c eval_period_fn =
  Series_types.cache -> 'c Series_types.period_series -> Period.t -> 'c Period_cell.t Seq.t

(*  Constructors *)
let const ~label value = TConst { id = fresh_id (); label; value }
let map ~label f s = TMap { id = fresh_id (); label; inner = s; f }
let convert ~label f s = TConvert { id = fresh_id (); label; inner = s; f }
let map2 ~label f s1 s2 = TMap2 { id = fresh_id (); label; s1; s2; f }
let fill_zero = Option.value ~default:0.0
let neg ~label s = TMap { id = fresh_id (); label; inner = s; f = (fun x -> -.x) }
let sum ~label s1 s2 = map2 ~label (fun a b -> fill_zero a +. fill_zero b) s1 s2
let sub ~label s1 s2 = map2 ~label (fun a b -> fill_zero a -. fill_zero b) s1 s2
let mul ~label s1 s2 = map2 ~label (fun a b -> fill_zero a *. fill_zero b) s1 s2
let div ~label s1 s2 = map2 ~label (fun a b -> fill_zero a /. fill_zero b) s1 s2

(*  Accessors *)
let id = function
  | TConst { id; _ } -> id
  | TMap { id; _ } -> id
  | TConvert { id; _ } -> id
  | TMap2 { id; _ } -> id
  | TUnfold { id; _ } -> id

let label = function
  | TConst { label; _ } -> label
  | TMap { label; _ } -> label
  | TConvert { label; _ } -> label
  | TMap2 { label; _ } -> label
  | TUnfold { label; _ } -> label

(*  Evaluation *)

let rec eval_query : type c.
    eval_period:c eval_period_fn -> Series_types.cache -> c t -> Date.t -> c Point_cell.t option =
 fun ~eval_period cache series date ->
  let series_id = id series in
  match Hashtbl.find_opt cache.point (series_id, date) with
  | Some (Pack_point_cell value) -> Some (cast_point_cell value)
  | None -> (
      let value =
        match series with
        | TConst { value; _ } -> Some (Point_cell.const date value)
        | TMap { inner; f; _ } -> (
            let inner_cell = eval_query ~eval_period cache (Lazy.force inner) date in
            match inner_cell with None -> None | Some cell -> Some (Point_cell.map cell f))
        | TConvert { inner; f; _ } -> (
            let inner_cell =
              eval_query ~eval_period cache (cast_point_series (Lazy.force inner)) date
            in
            match inner_cell with None -> None | Some cell -> Some (Point_cell.convert cell f))
        | TMap2 { s1; s2; f; _ } ->
            let c1 = eval_query ~eval_period cache (Lazy.force s1) date in
            let c2 = eval_query ~eval_period cache (Lazy.force s2) date in
            Some (Point_cell.dep2 c1 c2 date f)
        | TUnfold _ ->
          (* Placeholder *)
          Some (Point_cell.const (Date.make 0 0 0) 0.0)
      in
      match value with
      | None -> value
      | Some cell ->
          Hashtbl.add cache.point (series_id, date) (Pack_point_cell cell);
          value)
