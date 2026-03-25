(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

module Cell = Period_cell

type 'c query_fn = Period.t -> 'c Cell.t Seq.t
type reduce = float list -> float

type dep_query =
  | Self of { period : Period.t; reduce : reduce }
  | Dep of { index : int; period : Period.t; reduce : reduce }

type 'c unfold_cell =
  | Seed of { period : Period.t; f : unit -> float }
  | Step of { period : Period.t; queries : dep_query list; f : float list -> float }

let next_id = Atomic.make 0
let fresh_id () = Atomic.fetch_and_add next_id 1
let reduce_sum = List.fold_left ( +. ) 0.0

exception
  Forward_self_query of { series_id : int; current_frontier : Date.t; query_period : Period.t }

type 'c t =
  | Const of { id : int; cells : 'c Cell.t Seq.t }
  | Unfold of { id : int; deps : 'c t Lazy.t list; cells : 'c unfold_cell Seq.t }
  | Map of { id : int; inner : 'c t Lazy.t; f : float -> float }
  | Convert of { id : int; inner : 'c t Lazy.t; f : Period.t -> float -> float }
  | Map2 of {
      id : int;
      s1 : 'c t Lazy.t;
      s2 : 'c t Lazy.t;
      f : float option -> float option -> float;
    }

let series_id = function
  | Const { id; _ } | Unfold { id; _ } | Map { id; _ } | Convert { id; _ } | Map2 { id; _ } -> id

(* CONSTRUCTORS *)

let const cells = Const { id = fresh_id (); cells }
let unfold ~deps cells = Unfold { id = fresh_id (); deps; cells }
let map f s = Map { id = fresh_id (); inner = s; f }
let convert f s = Convert { id = fresh_id (); inner = (Obj.magic s : _ t Lazy.t); f }
let map2 f s1 s2 = Map2 { id = fresh_id (); s1; s2; f }
let fill_zero = Option.value ~default:0.0
let sum s1 s2 = map2 (fun a b -> fill_zero a +. fill_zero b) (lazy s1) (lazy s2)
let sub s1 s2 = map2 (fun a b -> fill_zero a -. fill_zero b) (lazy s1) (lazy s2)
let mul s1 s2 = map2 (fun a b -> fill_zero a *. fill_zero b) (lazy s1) (lazy s2)
let div s1 s2 = map2 (fun a b -> fill_zero a /. fill_zero b) (lazy s1) (lazy s2)

(* QUERY HELPERS *)

(** Take cells from a sequence while a condition holds. Returns early when period end date equals or
    exceeds the given date. Does not consume periods after [date] when the date falls on a period
    end date. *)
let rec take_cells_while s date =
  match s () with
  | Seq.Nil -> Seq.empty
  | Seq.Cons (cell, rest) ->
      let cell_period = Cell.cell_period cell in
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
      Date.compare (Period.end_date (Cell.cell_period cell)) (Period.start_date period) <= 0)
    max_cells

let clipped_cells period seq =
  let overlapped_cells = overlapping_cells period seq in
  Seq.map (fun cell -> Cell.clip cell period) overlapped_cells

let rec eval_seq cache series =
  let id = series_id series in
  match Hashtbl.find_opt cache id with
  | Some seq -> seq
  | None ->
      let seq =
        match series with
        | Const { cells; _ } -> cells
        | Unfold { deps; cells; _ } ->
            (* Cache cells as they are produced to enable circular references within
               the series. Query periods must not extend past the historical range
               (raises Forward_self_query). *)
            let cell_cache = ref [] in
            let self_query period =
              (match !cell_cache with
              | latest :: _ ->
                  let frontier = Period.end_date (Cell.cell_period latest) in
                  if Date.compare (Period.end_date period) frontier > 0 then
                    raise
                      (Forward_self_query
                         { series_id = id; current_frontier = frontier; query_period = period })
              | [] -> ());
              let self_seq () = List.to_seq (List.rev !cell_cache) () in
              clipped_cells period self_seq
            in
            let dep_queries =
              List.map (fun dep -> fun period -> eval_query cache (Lazy.force dep) period) deps
            in
            let resolve_query = function
              | Self { period; reduce } -> (self_query period |> List.of_seq, reduce)
              | Dep { index; period; reduce } ->
                  ((List.nth dep_queries index) period |> List.of_seq, reduce)
            in
            let resolve_unfold_cell = function
              | Seed { period; f } -> Cell.const period f Cell.proportional_split
              | Step { period; queries; f } ->
                  let inner_cells =
                    List.map
                      (fun q ->
                        let dep_cells, reduce = resolve_query q in
                        Cell.deps period dep_cells reduce)
                      queries
                  in
                  Cell.deps period inner_cells f
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
        | Map { inner; f; _ } ->
            Seq.map (fun cell -> Cell.map cell f) (eval_seq cache (Lazy.force inner))
        | Convert { inner; f; _ } ->
            Seq.map (fun cell -> Cell.convert cell f) (eval_seq cache (Lazy.force inner))
        | Map2 { s1; s2; f; _ } ->
            let seq1 = eval_seq cache (Lazy.force s1) in
            let seq2 = eval_seq cache (Lazy.force s2) in
            let aligned_seq = Cell.iter_period_union seq1 seq2 in
            Seq.map (fun (cell_opt1, cell_opt2) -> Cell.map2 cell_opt1 cell_opt2 f) aligned_seq
      in
      let memo_seq = Seq.memoize seq in
      Hashtbl.replace cache id memo_seq;
      memo_seq

(* Get the cells from a series that cover the period range. *)
and eval_query cache series period =
  let seq = eval_seq cache series in
  clipped_cells period seq

let to_seq series_list =
  let cache = Hashtbl.create 16 in
  List.map (eval_seq cache) series_list

(* DEPENDENCY ANALYSIS *)

(** Return all transitive dependencies of a series (including itself). Each [Unfold] node is
    self-referential via its [self] parameter, so the node itself is included as a dependency and
    cycle detection uses physical identity ([==]) to avoid infinite recursion. *)
let dependencies series =
  let visited = ref [] in
  let is_visited s = List.exists (fun v -> v == s) !visited in
  let rec go acc s =
    if is_visited s then acc
    else begin
      visited := s :: !visited;
      let acc = s :: acc in
      match s with
      | Const _ -> acc
      | Unfold { deps; _ } ->
          (* The self parameter refers back to this Unfold node, which is
             already marked visited above, so recursing into deps is safe. *)
          List.fold_left (fun acc dep -> go acc (Lazy.force dep)) acc deps
      | Map { inner; _ } | Convert { inner; _ } -> go acc (Lazy.force inner)
      | Map2 { s1; s2; _ } -> go (go acc (Lazy.force s1)) (Lazy.force s2)
    end
  in
  go [] series |> List.rev

(* DOT OUTPUT *)

(** Pretty-print a DOT digraph of the dependency structure of one or more series. Each
    physically-distinct [Series.t] node becomes a DOT node; edges point from a node to its
    dependencies. Circular dependencies (e.g. self-referential [Unfold] nodes) are handled via
    physical identity and will appear as back-edges in the graph rather than causing infinite
    recursion. *)
let pp_dot ppf roots =
  (* Map from physical identity to (id, label). Uses a list + linear scan
     with [==] because Series.t is not hashable/comparable by structure. *)
  let next_dot_id = ref 0 in
  let nodes : (_ t * int) list ref = ref [] in
  let find_dot_id s = List.find_opt (fun (v, _) -> v == s) !nodes in
  let alloc_dot_id s =
    let id = !next_dot_id in
    incr next_dot_id;
    nodes := (s, id) :: !nodes;
    id
  in
  let label_of s =
    let sid = series_id s in
    match s with
    | Const _ -> Printf.sprintf "Const(%d)" sid
    | Unfold _ -> Printf.sprintf "Unfold(%d)" sid
    | Map _ -> Printf.sprintf "Map(%d)" sid
    | Convert _ -> Printf.sprintf "Convert(%d)" sid
    | Map2 _ -> Printf.sprintf "Map2(%d)" sid
  in

  let edges : (int * int) list ref = ref [] in
  let rec visit s =
    match find_dot_id s with
    | Some (_, id) -> id
    | None ->
        let id = alloc_dot_id s in
        let children =
          match s with
          | Const _ -> []
          | Unfold { deps; _ } -> List.map Lazy.force deps
          | Map { inner; _ } | Convert { inner; _ } -> [ Lazy.force inner ]
          | Map2 { s1; s2; _ } -> [ Lazy.force s1; Lazy.force s2 ]
        in
        List.iter
          (fun child ->
            let child_id = visit child in
            edges := (id, child_id) :: !edges)
          children;
        id
  in
  List.iter (fun root -> ignore (visit root)) roots;

  Format.fprintf ppf "@[<v>digraph deps {@ ";
  Format.fprintf ppf "  rankdir=TB;@ ";
  Format.fprintf ppf "  node [shape=box, style=filled, fillcolor=lightyellow];@ ";
  List.iter
    (fun (s, id) -> Format.fprintf ppf "  n%d [label=%S];@ " id (label_of s))
    (List.rev !nodes);
  List.iter (fun (src, dst) -> Format.fprintf ppf "  n%d -> n%d;@ " src dst) (List.rev !edges);
  Format.fprintf ppf "}@]"
