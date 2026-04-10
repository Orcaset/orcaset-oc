(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

open Cell_types

(* CELL DEPENDENCY TREE *)

(** A tree representing the dependency structure of a cell. [Cycle] marks a back-edge to a cell that
    was already visited on the current path, preventing infinite recursion when circular
    dependencies exist. *)
type cell_dep_tree =
  | Leaf of Cell_types.cell
  | Node of Cell_types.cell * cell_dep_tree list
  | Cycle of Cell_types.cell

type series =
  | PeriodSeries : _ Series_types.period_series -> series
  | PointSeries : _ Series_types.point_series -> series

(** Return the dependency tree rooted at [cell]. Circular dependencies are detected via physical
    identity ([==]) on a visited set and represented as [Cycle] nodes rather than recursing
    infinitely. *)
let cells (cell : Cell_types.cell) =
  let pack_cell (c : Cell_types.cell) = c in
  let rec go_period : type c. Cell_types.cell list -> c period_cell -> cell_dep_tree =
   fun visited c ->
    let wrapped = PeriodCell c in
    if List.exists (fun v -> v == wrapped) visited then Cycle wrapped
    else
      let visited = wrapped :: visited in
      match c with
      | RConst _ -> Leaf wrapped
      | RDeps { deps; _ } -> Node (wrapped, List.map (fun dep -> go visited (pack_cell dep)) deps)
      | RMap { inner; _ } -> Node (wrapped, [ go_period visited inner ])
      | RConvert { inner; _ } -> Node (wrapped, [ go_period visited inner ])
      | RMap2 { c1; c2; _ } ->
          let children =
            List.filter_map (fun opt -> Option.map (go_period visited) opt) [ c1; c2 ]
          in
          Node (wrapped, children)
      | RClip { inner; _ } -> Node (wrapped, [ go_period visited inner ])
      | RRef { cell = Some inner; _ } -> Node (wrapped, [ go_period visited inner ])
      | RRef { cell = None; _ } -> Leaf wrapped
  and go_point : type c. Cell_types.cell list -> c point_cell -> cell_dep_tree =
   fun visited c ->
    let wrapped = PointCell c in
    if List.exists (fun v -> v == wrapped) visited then Cycle wrapped
    else
      let visited = wrapped :: visited in
      match c with
      | TConst _ -> Leaf wrapped
      | TMap { inner; _ } -> Node (wrapped, [ go_point visited inner ])
      | TConvert { inner; _ } -> Node (wrapped, [ go_point visited inner ])
      | TDep2 { c1; c2; _ } ->
          let children =
            List.filter_map (fun opt -> Option.map (go_point visited) opt) [ c1; c2 ]
          in
          Node (wrapped, children)
      | TAccum { prev; changes; _ } ->
          let prev_children = match prev with Some p -> [ go_point visited p ] | None -> [] in
          let change_children = List.of_seq (Seq.map (go_period visited) changes) in
          Node (wrapped, prev_children @ change_children)
  and go visited = function
    | PeriodCell c -> go_period visited c
    | PointCell c -> go_point visited c
  in
  go [] cell

(* SERIES DEPENDENCIES *)

(** Return all transitive series dependencies (including the root itself). Handles both period and
    point series via the [series] sum type. Each [PUnfold] node is self-referential via its [self]
    parameter, so cycle detection uses physical identity ([==]) to avoid infinite recursion. *)
let series s =
  (* SAFETY: The phantom-type casts (cast_period_series, cast_point_series) only change 'c,
     which has no runtime representation. *)
  let cast_period_series : type a b. a Series_types.period_series -> b Series_types.period_series =
    Obj.magic
  in
  let cast_point_series : type a b. a Series_types.point_series -> b Series_types.point_series =
    Obj.magic
  in
  let period_visited = ref [] in
  let point_visited = ref [] in
  let is_period_visited s = List.exists (fun v -> v == s) !period_visited in
  let is_point_visited s = List.exists (fun v -> v == s) !point_visited in
  let rec go_period acc s =
    if is_period_visited s then acc
    else begin
      period_visited := s :: !period_visited;
      let acc = PeriodSeries s :: acc in
      match s with
      | Series_types.POfSeq _ -> acc
      | Series_types.PUnfold { deps; _ } ->
          List.fold_left
            (fun acc dep ->
              match dep with
              | Series_types.Period_dep ps -> go_period acc (Lazy.force ps)
              | Series_types.Point_dep ps -> go_point acc (Lazy.force ps))
            acc deps
      | Series_types.PMap { inner; _ } -> go_period acc (Lazy.force inner)
      | Series_types.PConvert { inner; _ } -> go_period acc (cast_period_series (Lazy.force inner))
      | Series_types.PMap2 { s1; s2; _ } ->
          go_period (go_period acc (Lazy.force s1)) (Lazy.force s2)
      | Series_types.PExtend { base; _ } -> go_period acc base
      | Series_types.PFilter { inner; _ } -> go_period acc (Lazy.force inner)
    end
  and go_point acc s =
    if is_point_visited s then acc
    else begin
      point_visited := s :: !point_visited;
      let acc = PointSeries s :: acc in
      match s with
      | Series_types.TConst _ -> acc
      | Series_types.TMap { inner; _ } -> go_point acc (Lazy.force inner)
      | Series_types.TConvert { inner; _ } -> go_point acc (cast_point_series (Lazy.force inner))
      | Series_types.TDep2 { s1; s2; _ } -> go_point (go_point acc (Lazy.force s1)) (Lazy.force s2)
      | Series_types.TAccum { changes; _ } -> go_period acc (Lazy.force changes)
      | Series_types.TExtend { base; _ } -> go_point acc base
      | Series_types.TOfList _ -> acc
    end
  in
  (* SAFETY: The existential type from PeriodSeries/PointSeries constructors is erased
     at runtime. We cast to a monomorphic phantom type to satisfy the type checker. *)
  let cast_ps : type c. c Series_types.period_series -> _ Series_types.period_series = Obj.magic in
  let cast_ts : type c. c Series_types.point_series -> _ Series_types.point_series = Obj.magic in
  (match s with
    | PeriodSeries ps -> go_period [] (cast_ps ps)
    | PointSeries ps -> go_point [] (cast_ts ps))
  |> List.rev

(* DOT OUTPUT *)

(** Pretty-print a DOT digraph of the dependency structure of one or more series. Each
    physically-distinct series node becomes a DOT node; edges point from dependencies to dependents.
    Node labels include the series label from construction. Circular dependencies are handled via
    physical identity and will appear as back-edges in the graph. *)
let pp_dot ppf roots =
  (* SAFETY: Only changes the phantom type parameter 'c, which has no runtime
     representation. Needed so that PConvert/TConvert children (which have a
     different phantom type) can be compared by physical identity (==) against
     the visited set for cycle detection. *)
  let cast_period_series : type a b. a Series_types.period_series -> b Series_types.period_series =
    Obj.magic
  in
  let cast_point_series : type a b. a Series_types.point_series -> b Series_types.point_series =
    Obj.magic
  in
  let next_dot_id = ref 0 in
  let nodes : (int * string) list ref = ref [] in
  (* Use separate tables for period and point series to handle physical identity *)
  let period_ids : (_ Series_types.period_series * int) list ref = ref [] in
  let point_ids : (_ Series_types.point_series * int) list ref = ref [] in
  let alloc_id () =
    let did = !next_dot_id in
    incr next_dot_id;
    did
  in
  let format_node_label lbl kind = Printf.sprintf "%s (%s)" lbl kind in
  let period_label s =
    let lbl = Period_series.label s in
    match s with
    | Series_types.POfSeq _ -> format_node_label lbl "OfSeq"
    | Series_types.PUnfold _ -> format_node_label lbl "Unfold"
    | Series_types.PMap _ -> format_node_label lbl "Map"
    | Series_types.PConvert _ -> format_node_label lbl "Convert"
    | Series_types.PMap2 _ -> format_node_label lbl "Map2"
    | Series_types.PExtend _ -> format_node_label lbl "Extend"
    | Series_types.PFilter _ -> format_node_label lbl "Filter"
  in
  let point_label s =
    let lbl = Point_series.label s in
    match s with
    | Series_types.TConst _ -> format_node_label lbl "PtConst"
    | Series_types.TMap _ -> format_node_label lbl "PtMap"
    | Series_types.TConvert _ -> format_node_label lbl "PtConvert"
    | Series_types.TDep2 _ -> format_node_label lbl "PtDep2"
    | Series_types.TAccum _ -> format_node_label lbl "PtAccum"
    | Series_types.TExtend _ -> format_node_label lbl "PtExtend"
    | Series_types.TOfList _ -> format_node_label lbl "PtOfList"
  in

  let edges : (int * int) list ref = ref [] in
  let rec visit_period s =
    match List.find_opt (fun (v, _) -> v == s) !period_ids with
    | Some (_, did) -> did
    | None ->
        let did = alloc_id () in
        period_ids := (s, did) :: !period_ids;
        nodes := (did, period_label s) :: !nodes;
        let children =
          match s with
          | Series_types.POfSeq _ -> []
          | Series_types.PUnfold { deps; _ } ->
              List.iter
                (fun dep ->
                  match dep with
                  | Series_types.Period_dep ps ->
                      let child_id = visit_period (Lazy.force ps) in
                      edges := (did, child_id) :: !edges
                  | Series_types.Point_dep ps ->
                      let child_id = visit_point (Lazy.force ps) in
                      edges := (did, child_id) :: !edges)
                deps;
              []
          | Series_types.PMap { inner; _ } -> [ Lazy.force inner ]
          | Series_types.PConvert { inner; _ } -> [ cast_period_series (Lazy.force inner) ]
          | Series_types.PMap2 { s1; s2; _ } -> [ Lazy.force s1; Lazy.force s2 ]
          | Series_types.PExtend { base; _ } -> [ base ]
          | Series_types.PFilter { inner; _ } -> [ Lazy.force inner ]
        in
        List.iter
          (fun child ->
            let child_id = visit_period child in
            edges := (did, child_id) :: !edges)
          children;
        did
  and visit_point s =
    match List.find_opt (fun (v, _) -> v == s) !point_ids with
    | Some (_, did) -> did
    | None ->
        let did = alloc_id () in
        point_ids := (s, did) :: !point_ids;
        nodes := (did, point_label s) :: !nodes;
        let children =
          match s with
          | Series_types.TConst _ -> []
          | Series_types.TMap { inner; _ } -> [ Lazy.force inner ]
          | Series_types.TConvert { inner; _ } -> [ cast_point_series (Lazy.force inner) ]
          | Series_types.TDep2 { s1; s2; _ } -> [ Lazy.force s1; Lazy.force s2 ]
          | Series_types.TAccum { changes; _ } ->
              let child_id = visit_period (Lazy.force changes) in
              edges := (did, child_id) :: !edges;
              []
          | Series_types.TExtend { base; _ } -> [ base ]
          | Series_types.TOfList _ -> []
        in
        List.iter
          (fun child ->
            let child_id = visit_point child in
            edges := (did, child_id) :: !edges)
          children;
        did
  in
  (* SAFETY: Same existential escape as in [series] above. *)
  let cast_ps : type c. c Series_types.period_series -> _ Series_types.period_series = Obj.magic in
  let cast_ts : type c. c Series_types.point_series -> _ Series_types.point_series = Obj.magic in
  List.iter
    (function
      | PeriodSeries root -> ignore (visit_period (cast_ps root))
      | PointSeries root -> ignore (visit_point (cast_ts root)))
    roots;

  Format.fprintf ppf "@[<v>digraph deps {@ ";
  Format.fprintf ppf "  rankdir=TB;@ ";
  Format.fprintf ppf "  node [shape=box, style=filled, fillcolor=lightyellow];@ ";
  List.iter
    (fun (did, label) -> Format.fprintf ppf "  n%d [label=%S];@ " did label)
    (List.rev !nodes);
  List.iter (fun (src, dst) -> Format.fprintf ppf "  n%d -> n%d;@ " dst src) (List.rev !edges);
  Format.fprintf ppf "}@]"
