(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

open Cell_type

(* ----- Series ids ----- *)
type series_id = Id of int [@@unboxed]

let next_id = Atomic.make 0
let new_id () : series_id = Id (Atomic.fetch_and_add next_id 1)

(* ----- Step ----- *)
(* A [step] is a user-facing handle for a single cell emitted by [Unfold]. It is
   represented internally as a [span], but is exposed abstractly in the .mli so
   users cannot construct or inspect spans directly. *)
type split = split_span

let proportional_split : split = Cell_type.proportional_split
let const_split : split = Cell_type.const_split

type step = span

let step ~period ~split value : step = f_value period value split

(* ----- Series constructors ----- *)
module rec Span_series : sig
  type t =
    | Const of { id : series_id; period : Period.t; value : unit -> float }
    | Map of { id : series_id; dep : t; f : float -> float }
    | Map2 of { id : series_id; a : t; b : t; f : float option -> float option -> float }
    | Unfold : {
        id : series_id;
        deps : unit -> 'readers Deps.t;
        init : 'b;
        step : 'readers -> 'b -> (step * 'b) option;
      }
        -> t

  val id : t -> series_id
  val neg : t -> t
  val scale : float -> t -> t
  val sum : t -> t -> t
  val sub : t -> t -> t
end = struct
  type t =
    | Const of { id : series_id; period : Period.t; value : unit -> float }
    | Map of { id : series_id; dep : t; f : float -> float }
    | Map2 of { id : series_id; a : t; b : t; f : float option -> float option -> float }
    | Unfold : {
        id : series_id;
        deps : unit -> 'readers Deps.t;
        init : 'b;
        step : 'readers -> 'b -> (step * 'b) option;
      }
        -> t

  let id = function
    | Const { id; _ } -> id
    | Map { id; _ } -> id
    | Map2 { id; _ } -> id
    | Unfold { id; _ } -> id

  let neg dep = Map { id = new_id (); dep; f = (fun x -> -.x) }
  let scale k dep = Map { id = new_id (); dep; f = (fun x -> k *. x) }

  let sum a b =
    Map2
      {
        id = new_id ();
        a;
        b;
        f = (fun x y -> Option.value ~default:0.0 x +. Option.value ~default:0.0 y);
      }

  let sub a b =
    Map2
      {
        id = new_id ();
        a;
        b;
        f = (fun x y -> Option.value ~default:0.0 x -. Option.value ~default:0.0 y);
      }
end

and Point_series : sig
  type t =
    | Const of { id : series_id; period : Period.t; value : unit -> float }
    | Map of { id : series_id; dep : t; f : float -> float }
    | Map2 of { id : series_id; a : t; b : t; f : float option -> float option -> float }
    | Accum of { id : series_id; init : float; changes : Span_series.t }

  val id : t -> series_id
  val neg : t -> t
  val scale : float -> t -> t
  val sum : t -> t -> t
  val sub : t -> t -> t
end = struct
  type t =
    | Const of { id : series_id; period : Period.t; value : unit -> float }
    | Map of { id : series_id; dep : t; f : float -> float }
    | Map2 of { id : series_id; a : t; b : t; f : float option -> float option -> float }
    | Accum of { id : series_id; init : float; changes : Span_series.t }

  let id = function
    | Const { id; _ } -> id
    | Map { id; _ } -> id
    | Map2 { id; _ } -> id
    | Accum { id; _ } -> id

  let neg dep = Map { id = new_id (); dep; f = (fun x -> -.x) }
  let scale k dep = Map { id = new_id (); dep; f = (fun x -> k *. x) }

  let sum a b =
    Map2
      {
        id = new_id ();
        a;
        b;
        f = (fun x y -> Option.value ~default:0.0 x +. Option.value ~default:0.0 y);
      }

  let sub a b =
    Map2
      {
        id = new_id ();
        a;
        b;
        f = (fun x y -> Option.value ~default:0.0 x -. Option.value ~default:0.0 y);
      }
end

and Deps : sig
  type span_reader = period:Period.t -> reduce:(float option list -> float) -> float
  type point_reader = date:Date.t -> default:float -> float
  type _ t

  val none : unit t
  val span_dep : Span_series.t -> span_reader t
  val point_dep : Point_series.t -> point_reader t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val reduce : (float -> float -> float) -> float -> float option list -> float

  type packed_dep = Span_item of Span_series.t | Point_item of Point_series.t

  val dependencies : 'a t -> packed_dep list

  val run :
    query_span_values:(Span_series.t -> Period.t -> float option list) ->
    query_point_value:(Point_series.t -> Date.t -> float option) ->
    'a t ->
    'a
end = struct
  type span_reader = period:Period.t -> reduce:(float option list -> float) -> float
  type point_reader = date:Date.t -> default:float -> float

  type _ dep =
    | Span_dep : Span_series.t -> span_reader dep
    | Point_dep : Point_series.t -> point_reader dep

  type _ t = Pure : 'a -> 'a t | Ap : 'x dep * ('x -> 'a) t -> 'a t

  let none = Pure ()

  let rec map : type a b. (a -> b) -> a t -> b t =
   fun f -> function Pure x -> Pure (f x) | Ap (d, k) -> Ap (d, map (fun g x -> f (g x)) k)

  (* Standard free-applicative [ap]: ap (f : ('a -> 'b) t) (x : 'a t) : 'b t. *)
  let rec ap : type a b. (a -> b) t -> a t -> b t =
   fun f x ->
    match f with Pure g -> map g x | Ap (d, k) -> Ap (d, ap (map (fun g y a -> g a y) k) x)

  let span_dep s = Ap (Span_dep s, Pure (fun r -> r))
  let point_dep s = Ap (Point_dep s, Pure (fun r -> r))

  let reduce (op : float -> float -> float) (fill : float) =
    List.fold_left (fun acc -> function Some v -> op acc v | None -> op acc fill) 0.0

  let ( let+ ) x f = map f x
  let ( and+ ) a b = ap (map (fun x y -> (x, y)) a) b

  type packed_dep = Span_item of Span_series.t | Point_item of Point_series.t

  let rec dependencies : type a. a t -> packed_dep list = function
    | Pure _ -> []
    | Ap (Span_dep s, rest) -> Span_item s :: dependencies rest
    | Ap (Point_dep s, rest) -> Point_item s :: dependencies rest

  let run (type a) ~query_span_values ~query_point_value (d : a t) : a =
    let rec go : type a. a t -> a = function
      | Pure x -> x
      | Ap (Span_dep s, rest) ->
          let reader ~period ~reduce = reduce (query_span_values s period) in
          go rest reader
      | Ap (Point_dep s, rest) ->
          let reader ~date ~default =
            match query_point_value s date with Some v -> v | None -> default
          in
          go rest reader
    in
    go d
end

(* ----- Existentially-packed series ----- *)

type _ series =
  | Point_series : Point_series.t -> [ `Point ] series
  | Span_series : Span_series.t -> [ `Span ] series

type packed_series = Series : 'a series -> packed_series
type dependency = { series : packed_series; dependencies : dependency list; is_back_edge : bool }

exception Resolution_failed = Cell_eval.Resolution_failed

let id : type a. a series -> series_id = function
  | Point_series series -> Point_series.id series
  | Span_series series -> Span_series.id series

(* ----- Series cache ----- *)
type point_slot = { eval : Cell_eval.slot; mutable cell : point option }
type span_slot = { eval : Cell_eval.slot; mutable cell : span option }
type cached_point = Cached_point of point_slot

module PointCellCache = Hashtbl.Make (struct
  type t = Date.t

  let equal = Date.equal
  let hash = Date.hash
end)

module SpanCellCache = Hashtbl.Make (struct
  type t = Period.t

  let equal = Period.equal
  let hash = Period.hash
end)

type span_cache_entry = { cells : span_slot option list SpanCellCache.t; sequence : span Seq.t }

(* ----- Accum checkpoint cache ----- *)

module DateMap = Map.Make (struct
  type t = Date.t

  let compare = Date.compare
end)

type accum_checkpoint = { point : point_slot; tail : span Seq.t }
type accum_cache_entry = { mutable checkpoints : accum_checkpoint DateMap.t; sequence : span Seq.t }

type series_cache = {
  point : (series_id, cached_point PointCellCache.t) Hashtbl.t;
  span : (series_id, span_cache_entry) Hashtbl.t;
  accum : (series_id, accum_cache_entry) Hashtbl.t;
  point_cells : (int, point_slot) Hashtbl.t;
  span_cells : (int, span_slot) Hashtbl.t;
  mutable active_context : Cell_eval.context option;
}

let make_cache () : series_cache =
  {
    point = Hashtbl.create 20;
    span = Hashtbl.create 20;
    accum = Hashtbl.create 20;
    point_cells = Hashtbl.create 64;
    span_cells = Hashtbl.create 64;
    active_context = None;
  }

let with_context cache ctx f =
  let previous_context = cache.active_context in
  cache.active_context <- Some ctx;
  Fun.protect ~finally:(fun () -> cache.active_context <- previous_context) f

let active_context cache =
  match cache.active_context with
  | Some ctx -> ctx
  | None -> invalid_arg "Series reader used outside an active query"

let make_point_slot () : point_slot = { eval = Cell_eval.create_slot (); cell = None }
let make_span_slot () : span_slot = { eval = Cell_eval.create_slot (); cell = None }

let point_cell_cache cache series_id =
  match Hashtbl.find_opt cache.point series_id with
  | Some cell_cache -> cell_cache
  | None ->
      let cell_cache = PointCellCache.create 16 in
      Hashtbl.add cache.point series_id cell_cache;
      cell_cache

let span_cell_cache cache series_id make_sequence =
  match Hashtbl.find_opt cache.span series_id with
  | Some entry -> entry
  | None ->
      let entry = { cells = SpanCellCache.create 16; sequence = make_sequence () } in
      Hashtbl.add cache.span series_id entry;
      entry

(* ----- Series query functions ----- *)

(** Collect spans in sequence over period, padding with None at start/end if query period
    starts/ends before/after seq period and clipping to period boundaries.

    Iteration stops as soon as (a) a span starts at or after [period_end], or (b) a collected span
    already ends at or after [period_end] — in case (b) we avoid forcing the next element of [seq]
    at all, which is essential for safe re-entrance into an in-progress unfold producer. *)
let collect_spans seq period =
  let period_end = Period.end_ period in
  let rec go seq acc =
    match seq () with
    | Seq.Nil -> List.rev acc
    | Seq.Cons (sp, rest) ->
        let sp_start, sp_end = Period.to_tuple (span_period sp) in
        if Date.(sp_start >= period_end) then List.rev acc
        else
          let acc = match clip_span period sp with Some clipped -> clipped :: acc | None -> acc in
          if Date.(sp_end >= period_end) then List.rev acc else go rest acc
  in
  let overlapping = go seq [] in
  match overlapping with
  | [] -> []
  | first :: _ ->
      let last = List.hd (List.rev overlapping) in
      let prefix =
        if Date.(Period.start period < Period.start (span_period first)) then [ None ] else []
      in
      let suffix =
        if Date.(Period.end_ (span_period last) < Period.end_ period) then [ None ] else []
      in
      prefix @ List.map (fun s -> Some s) overlapping @ suffix

let walk_accum_delta tail date =
  let rec loop acc tail =
    match Seq.uncons tail with
    | None -> (List.rev acc, tail)
    | Some (head, rest) ->
        let start, end_ = Period.to_tuple (span_period head) in
        if Date.(end_ <= date) then loop (Some head :: acc) rest
        else if Date.(start >= date) then (List.rev acc, Seq.cons head rest)
        else
          let left, right = split_span date head in
          let acc = match left with Some _ -> left :: acc | None -> acc in
          let new_tail = match right with Some r -> Seq.cons r rest | None -> rest in
          (List.rev acc, new_tail)
  in
  loop [] tail

(* Build a buffered, on-demand cell producer for an [Unfold]. *)
let make_unfold_producer : type b. init:b -> step:(b -> (span * b) option) -> span Seq.t =
 fun ~init ~step ->
  let buf = Dynarray.create () in
  let state = ref init in
  let finished = ref false in
  let producing = ref false in
  let advance () =
    if !finished || !producing then false
    else begin
      producing := true;
      let result =
        try step !state
        with e ->
          producing := false;
          raise e
      in
      producing := false;
      match result with
      | None ->
          finished := true;
          false
      | Some (cell, next) ->
          Dynarray.add_last buf cell;
          state := next;
          true
    end
  in
  let rec view i () =
    if i < Dynarray.length buf then Seq.Cons (Dynarray.get buf i, view (i + 1))
    else if advance () then Seq.Cons (Dynarray.get buf i, view (i + 1))
    else Seq.Nil
  in
  view 0

let rec point_slot_for_cell cache (cell : point) : point_slot =
  match Hashtbl.find_opt cache.point_cells (point_id cell) with
  | Some slot -> slot
  | None ->
      let slot = make_point_slot () in
      set_point_slot_cell cache slot cell;
      slot

and set_point_slot_cell cache (slot : point_slot) (cell : point) =
  slot.cell <- Some cell;
  Hashtbl.replace cache.point_cells (point_id cell) slot;
  Cell_eval.set_ready slot.eval (fun ctx -> eval_point_cell ctx cache cell)

and point_cell_value ctx cache (cell : point) =
  let slot = point_slot_for_cell cache cell in
  match Cell_eval.read ctx slot.eval with Some value -> value | None -> 0.0

and eval_point_cell ctx cache = function
  | Const { value; _ } -> value ()
  | Map { dep; f; _ } -> f (point_cell_value ctx cache dep)
  | Derived { deps; f; _ } ->
      let values =
        List.map
          (function Some point -> Some (point_cell_value ctx cache point) | None -> None)
          deps
      in
      f values
  | Accum { init; base; delta; _ } ->
      let start = match base with Some point -> point_cell_value ctx cache point | None -> init in
      List.fold_left
        (fun acc -> function Some span -> acc +. span_cell_value ctx cache span | None -> acc)
        start delta

and span_slot_for_cell cache (cell : span) : span_slot =
  match Hashtbl.find_opt cache.span_cells (span_id cell) with
  | Some slot -> slot
  | None ->
      let slot = make_span_slot () in
      set_span_slot_cell cache slot cell;
      slot

and set_span_slot_cell cache (slot : span_slot) (cell : span) =
  slot.cell <- Some cell;
  Hashtbl.replace cache.span_cells (span_id cell) slot;
  Cell_eval.set_ready slot.eval (fun ctx -> eval_span_cell ctx cache cell)

and span_cell_value ctx cache (cell : span) =
  let slot = span_slot_for_cell cache cell in
  match Cell_eval.read ctx slot.eval with Some value -> value | None -> 0.0

and eval_span_cell ctx cache = function
  | Value { value; _ } -> value ()
  | Map { dep; f; _ } -> f (span_cell_value ctx cache dep)
  | Clip { dep; f; _ } -> f (span_cell_value ctx cache dep)
  | Map2 { a; b; f; _ } ->
      let value = function Some span -> Some (span_cell_value ctx cache span) | None -> None in
      f (value a) (value b)

and read_point_slot ctx (slot : point_slot) = Cell_eval.read ctx slot.eval
and read_span_slot ctx (slot : span_slot) = Cell_eval.read ctx slot.eval

and span_entry_for_series (cache : series_cache) (series : Span_series.t) : span_cache_entry =
  span_cell_cache cache (Span_series.id series) (fun () -> build_span_sequence cache series)

and seq_for_span_series (cache : series_cache) (series : Span_series.t) : span Seq.t =
  let ({ sequence; _ } : span_cache_entry) = span_entry_for_series cache series in
  sequence

and build_span_sequence (cache : series_cache) (series : Span_series.t) : span Seq.t =
  match series with
  | Const { period; value; _ } -> Seq.return (f_value period value const_split)
  | Map { dep; f; _ } ->
      Seq.memoize (Seq.map (fun span -> f_map span f) (seq_for_span_series cache dep))
  | Map2 { a; b; f; _ } ->
      let paired = align_span_seq (seq_for_span_series cache a) (seq_for_span_series cache b) in
      let seq =
        Seq.filter_map
          (fun (a, b) ->
            match (a, b) with
            | None, None -> None
            | _ ->
                let period =
                  match (a, b) with
                  | Some s, _ | _, Some s -> span_period s
                  | None, None -> assert false
                in
                Some (f_map2 period a b f))
          paired
      in
      Seq.memoize seq
  | Unfold { deps; init; step; _ } ->
      let readers =
        Deps.run
          ~query_span_values:(fun s period ->
            let ctx = active_context cache in
            query_span_series cache s period
            |> List.map (function Some slot -> read_span_slot ctx slot | None -> None))
          ~query_point_value:(fun s date ->
            let ctx = active_context cache in
            match query_point_series cache s date with
            | Some slot -> read_point_slot ctx slot
            | None -> None)
          (deps ())
      in
      make_unfold_producer ~init ~step:(step readers)

and query_span_series cache series period : span_slot option list =
  let ({ cells; sequence } : span_cache_entry) = span_entry_for_series cache series in
  match SpanCellCache.find_opt cells period with
  | Some values -> values
  | None ->
      let spans = collect_spans sequence period in
      let slots = List.map (Option.map (span_slot_for_cell cache)) spans in
      SpanCellCache.replace cells period slots;
      slots

and get_accum_entry cache point_series_id changes : accum_cache_entry =
  match Hashtbl.find_opt cache.accum point_series_id with
  | Some entry -> entry
  | None ->
      let entry = { checkpoints = DateMap.empty; sequence = seq_for_span_series cache changes } in
      Hashtbl.add cache.accum point_series_id entry;
      entry

and query_accum cache point_series_id init changes date : point_slot =
  let entry = get_accum_entry cache point_series_id changes in
  let nearest = DateMap.find_last_opt (fun k -> Date.(k <= date)) entry.checkpoints in
  match nearest with
  | Some (cp_date, cp) when Date.equal cp_date date -> cp.point
  | _ ->
      let base_slot, start_tail =
        match nearest with
        | Some (_, cp) -> (Some cp.point, cp.tail)
        | None -> (None, entry.sequence)
      in
      let delta, new_tail = walk_accum_delta start_tail date in
      let delta_slots = List.map (Option.map (span_slot_for_cell cache)) delta in
      let slot = make_point_slot () in
      Cell_eval.set_ready slot.eval (fun ctx ->
          let start =
            match base_slot with
            | Some base -> (
                match read_point_slot ctx base with Some value -> value | None -> init)
            | None -> init
          in
          List.fold_left
            (fun acc -> function
              | Some delta -> (
                  match read_span_slot ctx delta with Some value -> acc +. value | None -> acc)
              | None -> acc)
            start delta_slots);
      entry.checkpoints <- DateMap.add date { point = slot; tail = new_tail } entry.checkpoints;
      slot

and query_point_series cache series date : point_slot option =
  let cell_cache = point_cell_cache cache (Point_series.id series) in
  match PointCellCache.find_opt cell_cache date with
  | Some (Cached_point slot) -> if Cell_eval.is_missing slot.eval then None else Some slot
  | None ->
      let slot = make_point_slot () in
      PointCellCache.replace cell_cache date (Cached_point slot);
      let finish_missing () =
        Cell_eval.set_missing slot.eval;
        None
      in
      let finish_ready ?cell formula =
        Option.iter (set_point_slot_cell cache slot) cell;
        Cell_eval.set_ready slot.eval formula;
        Some slot
      in
      let value =
        match series with
        | Const { period; value; _ } ->
            if Period.contains date period then
              finish_ready ~cell:(p_const date value) (fun _ctx -> value ())
            else finish_missing ()
        | Map { dep; f; _ } -> (
            match query_point_series cache dep date with
            | Some dep_slot ->
                let cell =
                  match dep_slot.cell with Some dep -> Some (p_map dep f) | None -> None
                in
                finish_ready ?cell (fun ctx ->
                    match read_point_slot ctx dep_slot with
                    | Some dep_value -> f dep_value
                    | None -> f 0.0)
            | None -> finish_missing ())
        | Map2 { a; b; f; _ } ->
            let a_slot, b_slot =
              (query_point_series cache a date, query_point_series cache b date)
            in
            let cell =
              match (a_slot, b_slot) with
              | Some { cell = Some a_cell; _ }, Some { cell = Some b_cell; _ } ->
                  Some
                    (p_derived date [ Some a_cell; Some b_cell ] (function
                      | [ va; vb ] -> f va vb
                      | _ -> assert false))
              | Some { cell = Some a_cell; _ }, _ ->
                  Some
                    (p_derived date [ Some a_cell; None ] (function
                      | [ va; vb ] -> f va vb
                      | _ -> assert false))
              | _, Some { cell = Some b_cell; _ } ->
                  Some
                    (p_derived date [ None; Some b_cell ] (function
                      | [ va; vb ] -> f va vb
                      | _ -> assert false))
              | _ -> None
            in
            let read_option ctx = function
              | Some dep_slot -> read_point_slot ctx dep_slot
              | None -> None
            in
            finish_ready ?cell (fun ctx -> f (read_option ctx a_slot) (read_option ctx b_slot))
        | Accum { init; changes; _ } ->
            let accum_slot = query_accum cache (Point_series.id series) init changes date in
            PointCellCache.replace cell_cache date (Cached_point accum_slot);
            Some accum_slot
      in
      value

(* ----- Public float-based query API ----- *)

let query_span cache series ~period ~reduce =
  let ctx = Cell_eval.create_context () in
  with_context cache ctx (fun () ->
      let slots = query_span_series cache series period in
      List.iter (Option.iter (fun slot -> Cell_eval.touch ctx slot.eval)) slots;
      Cell_eval.solve ctx;
      slots
      |> List.map (function Some slot -> Some (Cell_eval.current slot.eval) | None -> None)
      |> reduce)

let query_point cache series ~date ~default =
  let ctx = Cell_eval.create_context () in
  with_context cache ctx (fun () ->
      match query_point_series cache series date with
      | None -> default
      | Some slot ->
          Cell_eval.touch ctx slot.eval;
          Cell_eval.solve ctx;
          Cell_eval.current slot.eval)

(* ----- Series dependencies ----- *)

module Series_id_set = Set.Make (struct
  type t = series_id

  let compare (Id a) (Id b) = Int.compare a b
end)

let series_dependencies : type a. a series -> packed_series list = function
  | Point_series series -> (
      match series with
      | Const _ -> []
      | Map { dep; _ } -> [ Series (Point_series dep) ]
      | Map2 { a; b; _ } -> [ Series (Point_series a); Series (Point_series b) ]
      | Accum { changes; _ } -> [ Series (Span_series changes) ])
  | Span_series series -> (
      match series with
      | Const _ -> []
      | Map { dep; _ } -> [ Series (Span_series dep) ]
      | Map2 { a; b; _ } -> [ Series (Span_series a); Series (Span_series b) ]
      | Unfold { deps; _ } ->
          Deps.dependencies (deps ())
          |> List.map (function
            | Deps.Span_item s -> Series (Span_series s)
            | Deps.Point_item s -> Series (Point_series s)))

let rec build_dependencies active_path (Series series) =
  let add_dependency dependency =
    let (Series dep_series) = dependency in
    let dep_id = id dep_series in
    if Series_id_set.mem dep_id active_path then
      { series = dependency; dependencies = []; is_back_edge = true }
    else
      {
        series = dependency;
        dependencies = build_dependencies (Series_id_set.add dep_id active_path) dependency;
        is_back_edge = false;
      }
  in
  series_dependencies series |> List.map add_dependency

let dependencies : type a. a series -> dependency list =
 fun series -> build_dependencies (Series_id_set.singleton (id series)) (Series series)
