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

let unfold ~label ~deps cells = TUnfold { id = fresh_id (); label; deps; cells = Seq.memoize cells }

let map ~label f s = TMap { id = fresh_id (); label; inner = s; f }
let convert ~label f s = TConvert { id = fresh_id (); label; inner = s; f }
let map2 ~label f s1 s2 = TMap2 { id = fresh_id (); label; s1; s2; f }
let fill_zero = Option.value ~default:0.0
let neg ~label s = TMap { id = fresh_id (); label; inner = s; f = (fun x -> -.x) }
let sum ~label s1 s2 = map2 ~label (fun a b -> fill_zero a +. fill_zero b) s1 s2
let sub ~label s1 s2 = map2 ~label (fun a b -> fill_zero a -. fill_zero b) s1 s2
let mul ~label s1 s2 = map2 ~label (fun a b -> fill_zero a *. fill_zero b) s1 s2
let div ~label s1 s2 = map2 ~label (fun a b -> fill_zero a /. fill_zero b) s1 s2

let unfold_cell_date = function
  | Point_const_cell { date; _ } | Point_step_cell { date; _ } -> date

let rec find_unfold_cell target_date cells =
  match cells () with
  | Seq.Nil -> None
  | Seq.Cons (cell, rest) ->
      let cmp = Date.compare (unfold_cell_date cell) target_date in
      if cmp = 0 then Some cell else if cmp > 0 then None else find_unfold_cell target_date rest

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
            let point_deps =
              List.filter_map
                (fun cell_opt -> Option.map (fun cell -> Cell_types.PointCell cell) cell_opt)
                [ c1; c2 ]
            in
            let decode values =
              match (c1, c2, values) with
              | None, None, [] -> f None None
              | Some _, None, [ v1 ] -> f (Some v1) None
              | None, Some _, [ v2 ] -> f None (Some v2)
              | Some _, Some _, [ v1; v2 ] -> f (Some v1) (Some v2)
              | _ -> invalid_arg "Point_series.eval_query: internal decode mismatch"
            in
            Some (Point_cell.deps date point_deps decode)
        | TUnfold { cells; _ } -> (
            match find_unfold_cell date cells with
            | None -> None
            | Some unfold_cell ->
                let placeholder = Cell_types.TRef { id = Cell_types.fresh_id (); date; cell = None } in
                let resolve_query = function
                  | Point_self_query { date } ->
                      let cells =
                        match eval_query ~eval_period cache series date with
                        | Some cell -> [ Cell_types.PointCell cell ]
                        | None -> []
                      in
                      (cells, List.fold_left ( +. ) 0.0)
                  | Point_self_present_query { date } ->
                      let cells, reduce =
                        match eval_query ~eval_period cache series date with
                        | Some cell -> ([ Cell_types.PointCell cell ], fun _ -> 1.0)
                        | None -> ([], fun _ -> 0.0)
                      in
                      (cells, reduce)
                  | Point_period_query { dep; period; reduce } ->
                      let cells = eval_period cache (Lazy.force dep) period |> List.of_seq in
                      (List.map (fun cell -> Cell_types.PeriodCell cell) cells, reduce)
                  | Point_point_query { dep; date } ->
                      let cells =
                        match eval_query ~eval_period cache (Lazy.force dep) date with
                        | Some cell -> [ Cell_types.PointCell cell ]
                        | None -> []
                      in
                      (cells, List.fold_left ( +. ) 0.0)
                  | Point_point_present_query { dep; date } ->
                      let cells, reduce =
                        match eval_query ~eval_period cache (Lazy.force dep) date with
                        | Some cell -> ([ Cell_types.PointCell cell ], fun _ -> 1.0)
                        | None -> ([], fun _ -> 0.0)
                      in
                      (cells, reduce)
                in
                Hashtbl.replace cache.point (series_id, date) (Pack_point_cell placeholder);
                let resolved =
                  match unfold_cell with
                  | Point_const_cell { f; _ } -> Point_cell.const date (f ())
                  | Point_step_cell { queries; f; _ } ->
                      let inner_cells =
                        List.map
                          (fun q ->
                            let dep_cells, reduce = resolve_query q in
                            Point_cell.deps date dep_cells reduce)
                          queries
                      in
                      Point_cell.deps date
                        (List.map (fun c -> Cell_types.PointCell c) inner_cells)
                        f
                in
                (match placeholder with
                | Cell_types.TRef state -> state.cell <- Some resolved
                | _ -> assert false);
                Some placeholder)
      in
      match value with
      | None -> value
      | Some cell ->
          Hashtbl.replace cache.point (series_id, date) (Pack_point_cell cell);
          value)
