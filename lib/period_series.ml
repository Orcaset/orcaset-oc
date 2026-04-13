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

let of_seq ~label cells = POfSeq { id = fresh_id (); label; cells }
let unfold ~label ~deps cells = PUnfold { id = fresh_id (); label; deps; cells }

let extend ~label base cont =
  let memo = ref None in
  let cont p =
    match !memo with
    | Some s -> s
    | None ->
        let s = cont p in
        memo := Some s;
        s
  in
  PExtend { id = fresh_id (); label; base; cont }

let map ~label f s = PMap { id = fresh_id (); label; inner = s; f }
let convert ~label f s = PConvert { id = fresh_id (); label; inner = s; f }
let map2 ~label f s1 s2 = PMap2 { id = fresh_id (); label; s1; s2; f }
let fill_zero = Option.value ~default:0.0
let sum ~label s1 s2 = map2 ~label (fun a b -> fill_zero a +. fill_zero b) s1 s2
let sub ~label s1 s2 = map2 ~label (fun a b -> fill_zero a -. fill_zero b) s1 s2
let mul ~label s1 s2 = map2 ~label (fun a b -> fill_zero a *. fill_zero b) s1 s2
let div ~label s1 s2 = map2 ~label (fun a b -> fill_zero a /. fill_zero b) s1 s2
let filter ~label f s = PFilter { id = fresh_id (); label; inner = s; f }

let after ~label date s =
  filter ~label
    (fun seq ->
      Seq.filter_map
        (fun cell ->
          let p = Period_cell.period cell in
          if Date.(Period.end_date p <= date) then None
          else if Date.(Period.start_date p < date) then
            let _, right = Period_cell.split_cell cell date in
            Some right
          else Some cell)
        seq)
    s

let const_ann_growth ~label ~start ~value ~rate ~offset ~yf =
  (* TODO: Create a split function that divides value based on the relative proportion of 
     the period covered by each sub-period, as determined by the year fraction function [yf].
     This ensures the total value always sums to the parent, even if the sum of the sub-periods'
     year fraction does not equal the year fraction over the entire period (e.g. in 30/360
     case). Re-evaluate when looking at split function implementation. This doesn't work
     quite right for re-dividing split periods for YF functions where the original period 
     start date matters (e.g. 30/360). *)
  let rec split_fn period value_thunk split_date =
    let first_period = Period.make (Period.start_date period) split_date in
    let second_period = Period.make split_date (Period.end_date period) in
    let first_period_yf = yf (Period.start_date period) split_date in
    let second_period_yf = yf split_date (Period.end_date period) in
    let total_yf = first_period_yf +. second_period_yf in
    if total_yf = 0.0 then
      let zero_thunk = fun () -> 0.0 in
      ( { Period_cell.period = first_period; f = zero_thunk; split = split_fn },
        { Period_cell.period = second_period; f = zero_thunk; split = split_fn } )
    else
      let first_value = fun () -> value_thunk () *. (first_period_yf /. total_yf) in
      let second_value = fun () -> value_thunk () *. (second_period_yf /. total_yf) in
      ( { Period_cell.period = first_period; f = first_value; split = split_fn },
        { Period_cell.period = second_period; f = second_value; split = split_fn } )
  in
  let rec generate_cells last_period last_value () =
    let current_period = Period.next offset last_period in
    let current_value =
      last_value
      *. ((1.0 +. rate) ** yf (Period.start_date current_period) (Period.end_date current_period))
    in
    Seq.Cons
      ( Period_cell.const current_period (fun () -> current_value) split_fn,
        generate_cells current_period current_value )
  in
  let initial_period = Period.make start (Date.shift offset start) in
  let initial_cell = Period_cell.const initial_period (fun () -> value) split_fn in
  POfSeq
    { id = fresh_id (); label; cells = Seq.cons initial_cell (generate_cells initial_period value) }

(* ACCESSORS *)

let id = function
  | POfSeq { id; _ }
  | PUnfold { id; _ }
  | PMap { id; _ }
  | PConvert { id; _ }
  | PMap2 { id; _ }
  | PExtend { id; _ }
  | PFilter { id; _ } ->
      id

let label = function
  | POfSeq { label; _ }
  | PUnfold { label; _ }
  | PMap { label; _ }
  | PConvert { label; _ }
  | PMap2 { label; _ }
  | PExtend { label; _ }
  | PFilter { label; _ } ->
      label

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
        Date.compare (Period.start_date cell_period) date < 0
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
    | POfSeq { cells; _ } ->
        let memo_seq = Seq.memoize cells in
        Hashtbl.replace cache.period (id series) (Pack_period_seq memo_seq);
        memo_seq
    | PUnfold { cells; _ } ->
        let series_id = id series in
        let memo_cells = Seq.memoize cells in
        let resolve_unfold_cell_ref : (c unfold_cell -> c Period_cell.t) ref =
          ref (fun _ -> failwith "PUnfold: unresolved self ref resolver")
        in

        (* Build a memoized sequence of (RRef placeholder, unfold_cell) pairs.
           Pulling creates RRef cells with the correct periods (deterministic from
           the unfold cell) but no resolved inner cell yet. *)
        let rref_pairs =
          Seq.memoize
            (Seq.map
               (fun uc ->
                 let period = match uc with Const { period; _ } | Step { period; _ } -> period in
                 (Period_cell.cell_ref ~resolver:(fun () -> !resolve_unfold_cell_ref uc) period, uc))
               memo_cells)
        in

        (* Cache the placeholder sequence immediately so that recursive
           eval_query calls (Self / dep queries) find RRef cells instead
           of re-entering eval_seq and diverging. *)
        let placeholder_seq = Seq.memoize (Seq.map (fun (rref, _) -> rref) rref_pairs) in
        Hashtbl.replace cache.period series_id (Pack_period_seq placeholder_seq);

        (* Query resolution helpers. Self queries read directly from the placeholder sequence so
           that same-series forward or simultaneous references can discover cells without forcing
           their resolution order. When this series is the continuation of a PExtend, prefix cells
           from the base are included so that Self queries can look back into the base. *)
        let self_query_scope =
          match Hashtbl.find_opt cache.prefix series_id with
          | Some (Pack_period_seq seq) ->
              Seq.memoize (Seq.append (cast_period_seq seq) placeholder_seq)
          | None -> placeholder_seq
        in
        let self_query period = clipped_cells period self_query_scope in
        let resolve_query = function
          | Self_query { period; reduce } ->
              let cells = self_query period |> List.of_seq in
              List.iter Period_cell.ensure_resolved cells;
              (List.map (fun c -> Cell_types.PeriodCell c) cells, reduce)
          | Period_query { dep; period; reduce } ->
              let cells = eval_query ~eval_point cache period (Lazy.force dep) |> List.of_seq in
              (List.map (fun c -> Cell_types.PeriodCell c) cells, reduce)
          | Point_query { dep; date } ->
              let cells =
                match eval_point cache date (Lazy.force dep) with
                | Some cell -> [ Cell_types.PointCell cell ]
                | None -> []
              in
              (cells, reduce_sum)
          | Point_present_query { dep; date } ->
              let cells, reduce =
                match eval_point cache date (Lazy.force dep) with
                | Some cell -> ([ Cell_types.PointCell cell ], fun _ -> 1.0)
                | None -> ([], fun _ -> 0.0)
              in
              (cells, reduce)
        in
        let resolve_unfold_cell = function
          | Const { period; f } -> Period_cell.const period f Period_cell.proportional_split
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
        resolve_unfold_cell_ref := resolve_unfold_cell;

        (* Resolve each unfold cell and patch its RRef placeholder.
           Resolution is triggered lazily as consumers pull the sequence.
           Because rref_pairs is memoized and shared with placeholder_seq,
           the same RRef objects appear in both sequences — patching here
           makes the cached placeholders resolve correctly at eval time. *)
        let resolving_seq =
          Seq.memoize
            (Seq.map
               (fun (rref, _) ->
                 Period_cell.ensure_resolved rref;
                 rref)
               rref_pairs)
        in
        Hashtbl.replace cache.period series_id (Pack_period_seq resolving_seq);
        resolving_seq
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
    | PExtend { base; cont; _ } ->
        let base_seq = eval_seq ~eval_point cache base in
        let last_period = ref None in
        let base_list =
          List.of_seq
            (Seq.map
               (fun cell ->
                 last_period := Some (Period_cell.period cell);
                 cell)
               base_seq)
        in
        let memo_seq =
          match !last_period with
          | None -> Seq.memoize Seq.empty
          | Some p ->
              let cont_series = cont p in
              (* Combine any prefix from an enclosing extend with our base cells,
                 so the continuation's Self queries can see the full history. *)
              let own_prefix =
                match Hashtbl.find_opt cache.prefix (id series) with
                | Some (Pack_period_seq seq) -> List.of_seq (cast_period_seq seq)
                | None -> []
              in
              Hashtbl.replace cache.prefix (id cont_series)
                (Pack_period_seq (List.to_seq (own_prefix @ base_list)));
              let cont_seq = eval_seq ~eval_point cache cont_series in
              Seq.memoize (Seq.append (List.to_seq base_list) cont_seq)
        in
        Hashtbl.replace cache.period (id series) (Pack_period_seq memo_seq);
        memo_seq
    | PFilter { inner; f; _ } ->
        let inner_seq = eval_seq ~eval_point cache (Lazy.force inner) in
        let filtered_seq = f inner_seq in
        let memo_seq = Seq.memoize filtered_seq in
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
