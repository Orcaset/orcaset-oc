(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

open Series_types

type 'c t = 'c period_series

let reduce_sum = List.fold_left ( +. ) 0.0

(* CONSTRUCTORS *)

let const cells = PConst { id = fresh_id (); cells }
let unfold ~deps cells = PUnfold { id = fresh_id (); deps; cells }
let map f s = PMap { id = fresh_id (); inner = s; f }
let convert f s = PConvert { id = fresh_id (); inner = (Obj.magic s : _ t Lazy.t); f }
let map2 f s1 s2 = PMap2 { id = fresh_id (); s1; s2; f }
let fill_zero = Option.value ~default:0.0
let sum s1 s2 = map2 (fun a b -> fill_zero a +. fill_zero b) (lazy s1) (lazy s2)
let sub s1 s2 = map2 (fun a b -> fill_zero a -. fill_zero b) (lazy s1) (lazy s2)
let mul s1 s2 = map2 (fun a b -> fill_zero a *. fill_zero b) (lazy s1) (lazy s2)
let div s1 s2 = map2 (fun a b -> fill_zero a /. fill_zero b) (lazy s1) (lazy s2)

(* ACCESSORS *)

let id = function
  | PConst { id; _ } | PUnfold { id; _ } | PMap { id; _ } | PConvert { id; _ } | PMap2 { id; _ } ->
      id

(* QUERY HELPERS *)

(** Take cells from a sequence while a condition holds. Returns early when period end date equals or
    exceeds the given date. Does not consume periods after [date] when the date falls on a period
    end date. *)
let rec take_cells_while s date =
  match s () with
  | Seq.Nil -> Seq.empty
  | Seq.Cons (cell, rest) ->
      let cell_period = Period_cell.period cell in
      let period_end_comparison = Date.compare (Period.end_date cell_period) date in
      let straddles_date =
        Date.compare (Period.start_date cell_period) date <= 0
        && Date.compare (Period.end_date cell_period) date > 0
      in
      if period_end_comparison < 0 then Seq.cons cell (take_cells_while rest date)
      else if period_end_comparison = 0 then Seq.cons cell Seq.empty
      else if straddles_date then Seq.cons cell Seq.empty
      else Seq.empty

let overlapping_cells period seq =
  let max_cells = take_cells_while seq (Period.end_date period) in
  Seq.drop_while
    (fun cell ->
      Date.compare (Period.end_date (Period_cell.period cell)) (Period.start_date period) <= 0)
    max_cells

let clipped_cells period seq =
  let overlapped_cells = overlapping_cells period seq in
  Seq.map (fun cell -> Period_cell.clip cell period) overlapped_cells

(* EVALUATION *)

let rec eval_seq ~eval_point cache series =
  let series_id = id series in
  match Hashtbl.find_opt cache.period series_id with
  | Some seq -> seq
  | None ->
      let seq =
        match series with
        | PConst { cells; _ } -> cells
        | PUnfold { deps; cells; _ } ->
            (* Cache cells as they are produced to enable circular references within
               the series. Query periods must not extend past the historical range
               (raises Forward_self_query). *)
            let cell_cache = ref [] in
            let self_query period =
              (match !cell_cache with
              | latest :: _ ->
                  let frontier = Period.end_date (Period_cell.period latest) in
                  if Date.compare (Period.end_date period) frontier > 0 then
                    raise
                      (Forward_self_query
                         { series_id; current_frontier = frontier; query_period = period })
              | [] -> ());
              let self_seq () = List.to_seq (List.rev !cell_cache) () in
              clipped_cells period self_seq
            in
            let dep_queries =
              List.map
                (fun dep ->
                  match dep with
                  | Period_dep ps ->
                      fun period ->
                        eval_query ~eval_point cache (Lazy.force ps) period
                  | Point_dep _ ->
                      fun _period ->
                        failwith
                          "Period dep query on a point series dep: use Point_dep in dep_query \
                           instead")
                deps
            in
            let resolve_query = function
              | Self { period; reduce } -> (self_query period |> List.of_seq, reduce)
              | Dep { index; period; reduce } ->
                  ((List.nth dep_queries index) period |> List.of_seq, reduce)
              | Series_types.Point_dep { index = _; date = _ } ->
                  (* Point_dep resolution requires cell-layer support for mixed
                     period/point dependencies. This will be implemented when
                     the cell types are extended to support cross-type deps. *)
                  failwith "Point_dep resolution in period series unfold is not yet implemented"
            in
            let resolve_unfold_cell = function
              | Seed { period; f } -> Period_cell.const period f Period_cell.proportional_split
              | Step { period; queries; f } ->
                  let inner_cells =
                    List.map
                      (fun q ->
                        let dep_cells, reduce = resolve_query q in
                        Period_cell.deps period
                          (List.map (fun c -> Cell_types.PeriodCell c) dep_cells)
                          reduce)
                      queries
                  in
                  Period_cell.deps period
                    (List.map (fun c -> Cell_types.PeriodCell c) inner_cells)
                    f
            in
            let cell_seq = Seq.map resolve_unfold_cell cells in
            let rec caching_seq s () =
              match s () with
              | Seq.Nil -> Seq.Nil
              | Seq.Cons (cell, rest) ->
                  cell_cache := cell :: !cell_cache;
                  Seq.Cons (cell, caching_seq rest)
            in
            caching_seq cell_seq
        | PMap { inner; f; _ } ->
            Seq.map
              (fun cell -> Period_cell.map cell f)
              (eval_seq ~eval_point cache (Lazy.force inner))
        | PConvert { inner; f; _ } ->
            Seq.map
              (fun cell -> Period_cell.convert cell f)
              (eval_seq ~eval_point cache (Lazy.force inner))
        | PMap2 { s1; s2; f; _ } ->
            let seq1 = eval_seq ~eval_point cache (Lazy.force s1) in
            let seq2 = eval_seq ~eval_point cache (Lazy.force s2) in
            let aligned_seq = Period_cell.iter_period_union seq1 seq2 in
            Seq.map
              (fun (cell_opt1, cell_opt2) -> Period_cell.map2 cell_opt1 cell_opt2 f)
              aligned_seq
      in
      let memo_seq = Seq.memoize seq in
      Hashtbl.replace cache.period series_id memo_seq;
      memo_seq

and eval_query ~eval_point cache series period =
  let seq = eval_seq ~eval_point cache series in
  clipped_cells period seq

