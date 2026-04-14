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

let reduce_sum = List.fold_left ( +. ) 0.0

let unfold_cell_period = function
  | Point_const_cell { period; _ } | Point_step_cell { period; _ } -> period

let rec take_unfold_cells_through_date cells date () =
  match cells () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons (cell, rest) ->
      let period = unfold_cell_period cell in
      let start_date = Period.start_date period in
      let end_date = Period.end_date period in
      if Date.compare end_date date <= 0 then
        Seq.Cons ((cell, None), take_unfold_cells_through_date rest date)
      else if Date.compare start_date date < 0 then
        Seq.Cons ((cell, Some (Period.make start_date date)), Seq.empty)
      else Seq.Nil

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
        | TUnfold { cells; _ } ->
            let unfold_cells = take_unfold_cells_through_date cells date |> List.of_seq in
            if unfold_cells = [] then None
            else
              let placeholder = Cell_types.TRef { id = Cell_types.fresh_id (); date; cell = None } in
              let resolve_query = function
                | Point_self_query { date } ->
                    let cells =
                      match eval_query ~eval_period cache series date with
                      | Some cell -> [ Cell_types.PointCell cell ]
                      | None -> []
                    in
                    (cells, reduce_sum)
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
                    (cells, reduce_sum)
                | Point_point_present_query { dep; date } ->
                    let cells, reduce =
                      match eval_query ~eval_period cache (Lazy.force dep) date with
                      | Some cell -> ([ Cell_types.PointCell cell ], fun _ -> 1.0)
                      | None -> ([], fun _ -> 0.0)
                    in
                    (cells, reduce)
              in
              let resolve_unfold_cell = function
                | Point_const_cell { period; f } ->
                    Period_cell.const period f Period_cell.proportional_split
                | Point_step_cell { period; queries; f } ->
                    let inner_cells =
                      List.map
                        (fun q ->
                          let dep_cells, reduce = resolve_query q in
                          Cell_types.RDeps
                            { id = Cell_types.fresh_id (); period; deps = dep_cells; f = reduce })
                        queries
                    in
                    Cell_types.RDeps
                      {
                        id = Cell_types.fresh_id ();
                        period;
                        deps = List.map (fun c -> Cell_types.PeriodCell c) inner_cells;
                        f;
                      }
              in
              Hashtbl.replace cache.point (series_id, date) (Pack_point_cell placeholder);
              let resolved_period_cells =
                List.map
                  (fun (unfold_cell, clipped_period) ->
                    let cell = resolve_unfold_cell unfold_cell in
                    match clipped_period with
                    | None -> cell
                    | Some period -> Period_cell.clip cell period)
                  unfold_cells
              in
              let resolved =
                Point_cell.deps date
                  (List.map (fun cell -> Cell_types.PeriodCell cell) resolved_period_cells)
                  reduce_sum
              in
              (match placeholder with
              | Cell_types.TRef state -> state.cell <- Some resolved
              | _ -> assert false);
              Some placeholder
      in
      match value with
      | None -> value
      | Some cell ->
          Hashtbl.replace cache.point (series_id, date) (Pack_point_cell cell);
          value)
