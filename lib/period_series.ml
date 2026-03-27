(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

open Series_types

type 'c t = 'c period_series

(* SAFETY: These casts only change the phantom type parameter 'c, which has no
   runtime representation — all period_cell and period_series variants carry only
   int, float, Period.t, Date.t, functions, and nested values of the same GADT
   family. Used to store/retrieve values from the existentially-typed cache
   (Pack_period_seq), which erases 'c on insertion and needs it restored on lookup. *)
let cast_period_seq : type a b. a Period_cell.t Seq.t -> b Period_cell.t Seq.t = Obj.magic
let cast_period_series : type a b. a t -> b t = Obj.magic

type 'c eval_point_fn =
  Series_types.cache -> Date.t -> 'c Series_types.point_series -> 'c Point_cell.t option

let reduce_sum = List.fold_left ( +. ) 0.0

(* CONSTRUCTORS *)

let const cells = PConst { id = fresh_id (); cells }
let unfold ~deps cells = PUnfold { id = fresh_id (); deps; cells }
let map f s = PMap { id = fresh_id (); inner = s; f }
let convert f s = PConvert { id = fresh_id (); inner = s; f }
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

let rec eval_seq : type c.
    eval_point:c eval_point_fn -> Series_types.cache -> c t -> c Period_cell.t Seq.t =
 fun ~eval_point cache series ->
  let seq =
    match series with
    | PConst { cells; _ } ->
        let memo_seq = Seq.memoize cells in
        Hashtbl.replace cache.period (id series) (Pack_period_seq memo_seq);
        memo_seq
    | PUnfold { deps; cells; _ } ->
        let series_id = id series in
        let memo_cells = Seq.memoize cells in

        (* Build a memoized sequence of (RRef placeholder, unfold_cell) pairs.
           Pulling creates RRef cells with the correct periods (deterministic from
           the unfold cell) but no resolved inner cell yet. *)
        let rref_pairs =
          Seq.memoize
            (Seq.map
               (fun uc ->
                 let period = match uc with Seed { period; _ } | Step { period; _ } -> period in
                 (Period_cell.cell_ref period, uc))
               memo_cells)
        in

        (* Cache the placeholder sequence immediately so that recursive
           eval_query calls (Self / dep queries) find RRef cells instead
           of re-entering eval_seq and diverging. *)
        let placeholder_seq = Seq.memoize (Seq.map (fun (rref, _) -> rref) rref_pairs) in
        Hashtbl.replace cache.period series_id (Pack_period_seq placeholder_seq);

        (* Query resolution helpers — all recursion goes through eval_query,
           which will hit the cached placeholder sequence. *)
        let self_query period = eval_query ~eval_point cache period series in
        let dep_queries =
          List.map
            (fun dep ->
              match dep with
              | Period_dep ps -> fun period -> eval_query ~eval_point cache period (Lazy.force ps)
              | Point_dep _ ->
                  fun _period ->
                    failwith
                      "Period dep query on a point series dep: use Point_dep in dep_query instead")
            deps
        in
        let resolve_query = function
          | Self { period; reduce } ->
              let cells = self_query period |> List.of_seq in
              (List.map (fun c -> Cell_types.PeriodCell c) cells, reduce)
          | Dep { index; period; reduce } ->
              let cells = (List.nth dep_queries index) period |> List.of_seq in
              (List.map (fun c -> Cell_types.PeriodCell c) cells, reduce)
          | Point_dep { index; date } ->
              let point_series =
                match List.nth deps index with
                | Point_dep ps -> Lazy.force ps
                | Period_dep _ ->
                    failwith "Point_dep query references a period series dep: use Dep instead"
              in
              let cells =
                match eval_point cache date point_series with
                | Some cell -> [ Cell_types.PointCell cell ]
                | None -> []
              in
              (* TODO: This effectively fills N/A's for query dates outside the series range with zeros. *)
              (cells, reduce_sum)
        in
        let resolve_unfold_cell = function
          | Seed { period; f } -> Period_cell.const period f Period_cell.proportional_split
          | Step { period; queries; f } ->
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

        (* Resolve each unfold cell and patch its RRef placeholder.
           Resolution is triggered lazily as consumers pull the sequence.
           Because rref_pairs is memoized and shared with placeholder_seq,
           the same RRef objects appear in both sequences — patching here
           makes the cached placeholders resolve correctly at eval time. *)
        Seq.memoize
          (Seq.map
             (fun (rref, uc) ->
               let resolved = resolve_unfold_cell uc in
               Period_cell.set_ref rref resolved;
               rref)
             rref_pairs)
    | PMap { inner; f; _ } ->
        let seq =
          Seq.map
            (fun cell -> Period_cell.map cell f)
            (eval_seq ~eval_point cache (Lazy.force inner))
        in
        let memo_seq = Seq.memoize seq in
        Hashtbl.replace cache.period (id series) (Pack_period_seq memo_seq);
        memo_seq
    | PConvert { inner; f; _ } ->
        let seq =
          Seq.map
            (fun cell -> Period_cell.convert cell f)
            (eval_seq ~eval_point cache (cast_period_series (Lazy.force inner)))
        in
        let memo_seq = Seq.memoize seq in
        Hashtbl.replace cache.period (id series) (Pack_period_seq memo_seq);
        memo_seq
    | PMap2 { s1; s2; f; _ } ->
        let seq1 = eval_seq ~eval_point cache (Lazy.force s1) in
        let seq2 = eval_seq ~eval_point cache (Lazy.force s2) in
        let aligned_seq = Period_cell.iter_period_union seq1 seq2 in
        let seq =
          Seq.map (fun (cell_opt1, cell_opt2) -> Period_cell.map2 cell_opt1 cell_opt2 f) aligned_seq
        in
        let memo_seq = Seq.memoize seq in
        Hashtbl.replace cache.period (id series) (Pack_period_seq memo_seq);
        memo_seq
  in
  seq

and eval_query : type c.
    eval_point:c eval_point_fn -> Series_types.cache -> Period.t -> c t -> c Period_cell.t Seq.t =
 fun ~eval_point cache period series ->
  let series_id = id series in
  let seq =
    match Hashtbl.find_opt cache.period series_id with
    | Some (Pack_period_seq seq) -> cast_period_seq seq
    | None -> eval_seq ~eval_point cache series
  in
  clipped_cells period seq
