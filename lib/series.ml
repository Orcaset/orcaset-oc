(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

open Cell_type

(* ----- Series ids ----- *)
type series_id = Id of int [@@unboxed]

let next_id = Atomic.make 0
let new_id () : series_id = Id (Atomic.fetch_and_add next_id 1)

(* ----- Span cells ----- *)
type split = split_strategy

let proportional_split : split = Cell_type.proportional_split
let const_split : split = Cell_type.const_split

(* ----- Series constructors ----- *)
module rec Formula : sig
  type 'a t

  type packed_query =
    | Span_query_item of { series : Spans.t; period : Period.t }
    | Point_query_item of { series : Points.t; date : Date.t }

  val pure : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val queries : 'a t -> packed_query list
  val span_query : Spans.t -> period:Period.t -> reduce:(float option list -> float) -> float t
  val point_query : Points.t -> date:Date.t -> default:float -> float t

  val eval_with_delta :
    query_span_values:(Spans.t -> Period.t -> float option list * float) ->
    query_point_value:(Points.t -> Date.t -> float option * float) ->
    'a t ->
    'a * float
end = struct
  type _ query =
    | Span_query : {
        series : Spans.t;
        period : Period.t;
        reduce : float option list -> float;
      }
        -> float query
    | Point_query : { series : Points.t; date : Date.t; default : float } -> float query

  type 'a t =
    | Pure : 'a -> 'a t
    | Map : ('a -> 'b) * 'a t -> 'b t
    | Map2 : ('a -> 'b -> 'c) * 'a t * 'b t -> 'c t
    | Query : 'a query -> 'a t

  type packed_query =
    | Span_query_item of { series : Spans.t; period : Period.t }
    | Point_query_item of { series : Points.t; date : Date.t }

  let pure x = Pure x
  let map f x = Map (f, x)
  let map2 f a b = Map2 (f, a, b)
  let ( let+ ) x f = map f x
  let ( and+ ) a b = map2 (fun x y -> (x, y)) a b
  let span_query series ~period ~reduce = Query (Span_query { series; period; reduce })
  let point_query series ~date ~default = Query (Point_query { series; date; default })

  let rec queries : type a. a t -> packed_query list = function
    | Pure _ -> []
    | Map (_, x) -> queries x
    | Map2 (_, a, b) -> queries a @ queries b
    | Query (Span_query { series; period; _ }) -> [ Span_query_item { series; period } ]
    | Query (Point_query { series; date; _ }) -> [ Point_query_item { series; date } ]

  let eval_with_delta (type a)
      ~(query_span_values : Spans.t -> Period.t -> float option list * float)
      ~(query_point_value : Points.t -> Date.t -> float option * float) (formula : a t) : a * float
      =
    let rec go : type a. a t -> a * float = function
      | Pure x -> (x, 0.0)
      | Map (f, x) ->
          let value, delta = go x in
          (f value, delta)
      | Map2 (f, a, b) ->
          let a_value, a_delta = go a in
          let b_value, b_delta = go b in
          (f a_value b_value, max a_delta b_delta)
      | Query (Span_query { series; period; reduce }) ->
          let values, delta = query_span_values series period in
          (reduce values, delta)
      | Query (Point_query { series; date; default }) ->
          let value, delta = query_point_value series date in
          (Option.value ~default value, delta)
    in
    go formula
end

and Spans : sig
  type unfold_cell

  type t =
    | Const of { id : series_id; label : string option; period : Period.t; value : unit -> float }
    | Map of { id : series_id; label : string option; dep : t; f : float -> float }
    | Map2 of {
        id : series_id;
        label : string option;
        a : t;
        b : t;
        f : float option -> float option -> float;
      }
    | Unfold : {
        id : series_id;
        label : string option;
        deps : unit -> 'readers Deps.t;
        init : 'state;
        cells : 'readers -> 'state -> (unfold_cell * 'state) option;
      }
        -> t

  val cell : period:Period.t -> split:split -> float Formula.t -> unfold_cell
  val unpack_unfold_cell : unfold_cell -> Period.t * split * float Formula.t
  val of_list : ?label:string -> split:split -> (Period.t * float) list -> t
  val id : t -> series_id
  val label : t -> string option
  val neg : ?label:string -> t -> t
  val scale : ?label:string -> float -> t -> t
  val sum : ?label:string -> ?fill:float -> t -> t -> t
  val sub : ?label:string -> ?fill:float -> t -> t -> t
  val mul : ?label:string -> ?fill:float -> t -> t -> t
  val div : ?label:string -> ?fill:float -> t -> t -> t
end = struct
  type unfold_cell = Cell of { period : Period.t; split : split; formula : float Formula.t }

  type t =
    | Const of { id : series_id; label : string option; period : Period.t; value : unit -> float }
    | Map of { id : series_id; label : string option; dep : t; f : float -> float }
    | Map2 of {
        id : series_id;
        label : string option;
        a : t;
        b : t;
        f : float option -> float option -> float;
      }
    | Unfold : {
        id : series_id;
        label : string option;
        deps : unit -> 'readers Deps.t;
        init : 'state;
        cells : 'readers -> 'state -> (unfold_cell * 'state) option;
      }
        -> t

  let cell ~period ~split formula = Cell { period; split; formula }
  let unpack_unfold_cell (Cell { period; split; formula }) = (period, split, formula)

  let of_list ?label ~split cells =
    Unfold
      {
        id = new_id ();
        label;
        deps = (fun () -> Deps.none);
        init = cells;
        cells =
          (fun () -> function
            | [] -> None
            | (period, value) :: rest -> Some (cell ~period ~split (Formula.pure value), rest));
      }

  let id = function
    | Const { id; _ } -> id
    | Map { id; _ } -> id
    | Map2 { id; _ } -> id
    | Unfold { id; _ } -> id

  let label = function
    | Const { label; _ } -> label
    | Map { label; _ } -> label
    | Map2 { label; _ } -> label
    | Unfold { label; _ } -> label

  let neg ?label dep = Map { id = new_id (); label; dep; f = (fun value -> -.value) }

  let scale ?label factor dep =
    Map { id = new_id (); label; dep; f = (fun value -> factor *. value) }

  let map2 ?label ?(fill = 0.0) a b f =
    Map2
      {
        id = new_id ();
        label;
        a;
        b;
        f = (fun a b -> f (Option.value ~default:fill a) (Option.value ~default:fill b));
      }

  let sum ?label ?fill a b = map2 ?label ?fill a b ( +. )
  let sub ?label ?fill a b = map2 ?label ?fill a b ( -. )
  let mul ?label ?fill a b = map2 ?label ?fill a b ( *. )
  let div ?label ?fill a b = map2 ?label ?fill a b ( /. )
end

and Points : sig
  type t =
    | Const of { id : series_id; label : string option; period : Period.t; value : unit -> float }
    | List of { id : series_id; label : string option; values : (Date.t * float) list }
    | Map of { id : series_id; label : string option; dep : t; f : float -> float }
    | Map2 of {
        id : series_id;
        label : string option;
        a : t;
        b : t;
        f : float option -> float option -> float;
      }
    | Accum of { id : series_id; label : string option; init : float; changes : Spans.t }

  val of_list : ?label:string -> (Date.t * float) list -> t
  val id : t -> series_id
  val label : t -> string option
  val neg : ?label:string -> t -> t
  val scale : ?label:string -> float -> t -> t
  val sum : ?label:string -> ?fill:float -> t -> t -> t
  val sub : ?label:string -> ?fill:float -> t -> t -> t
  val mul : ?label:string -> ?fill:float -> t -> t -> t
  val div : ?label:string -> ?fill:float -> t -> t -> t
end = struct
  type t =
    | Const of { id : series_id; label : string option; period : Period.t; value : unit -> float }
    | List of { id : series_id; label : string option; values : (Date.t * float) list }
    | Map of { id : series_id; label : string option; dep : t; f : float -> float }
    | Map2 of {
        id : series_id;
        label : string option;
        a : t;
        b : t;
        f : float option -> float option -> float;
      }
    | Accum of { id : series_id; label : string option; init : float; changes : Spans.t }

  let of_list ?label values = List { id = new_id (); label; values }

  let id = function
    | Const { id; _ } -> id
    | List { id; _ } -> id
    | Map { id; _ } -> id
    | Map2 { id; _ } -> id
    | Accum { id; _ } -> id

  let label = function
    | Const { label; _ } -> label
    | List { label; _ } -> label
    | Map { label; _ } -> label
    | Map2 { label; _ } -> label
    | Accum { label; _ } -> label

  let neg ?label dep = Map { id = new_id (); label; dep; f = (fun value -> -.value) }

  let scale ?label factor dep =
    Map { id = new_id (); label; dep; f = (fun value -> factor *. value) }

  let map2 ?label ?(fill = 0.0) a b f =
    Map2
      {
        id = new_id ();
        label;
        a;
        b;
        f = (fun a b -> f (Option.value ~default:fill a) (Option.value ~default:fill b));
      }

  let sum ?label ?fill a b = map2 ?label ?fill a b ( +. )
  let sub ?label ?fill a b = map2 ?label ?fill a b ( -. )
  let mul ?label ?fill a b = map2 ?label ?fill a b ( *. )
  let div ?label ?fill a b = map2 ?label ?fill a b ( /. )
end

and Deps : sig
  type span_reader = period:Period.t -> reduce:(float option list -> float) -> float Formula.t
  type point_reader = date:Date.t -> default:float -> float Formula.t
  type _ t

  val none : unit t
  val span_dep : Spans.t -> span_reader t
  val point_dep : Points.t -> point_reader t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

  type packed_dep = Span_item of Spans.t | Point_item of Points.t

  val dependencies : 'a t -> packed_dep list
  val run : 'a t -> 'a
end = struct
  type span_reader = period:Period.t -> reduce:(float option list -> float) -> float Formula.t
  type point_reader = date:Date.t -> default:float -> float Formula.t
  type _ dep = Span_dep : Spans.t -> span_reader dep | Point_dep : Points.t -> point_reader dep
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
  let ( let+ ) x f = map f x
  let ( and+ ) a b = ap (map (fun x y -> (x, y)) a) b

  type packed_dep = Span_item of Spans.t | Point_item of Points.t

  let rec dependencies : type a. a t -> packed_dep list = function
    | Pure _ -> []
    | Ap (Span_dep s, rest) -> Span_item s :: dependencies rest
    | Ap (Point_dep s, rest) -> Point_item s :: dependencies rest

  let run (type a) (d : a t) : a =
    let rec go : type a. a t -> a = function
      | Pure x -> x
      | Ap (Span_dep s, rest) ->
          let reader ~period ~reduce = Formula.span_query s ~period ~reduce in
          go rest reader
      | Ap (Point_dep s, rest) ->
          let reader ~date ~default = Formula.point_query s ~date ~default in
          go rest reader
    in
    go d
end

let cell = Spans.cell

(* ----- Existentially-packed series ----- *)

type _ series =
  | Point_series : Points.t -> [ `Point ] series
  | Span_series : Spans.t -> [ `Span ] series

let label : type a. a series -> string option = function
  | Point_series series -> Points.label series
  | Span_series series -> Spans.label series

type packed_series = Series : 'a series -> packed_series
type dependency = { series : packed_series; dependencies : dependency list; is_back_edge : bool }

let sum_float_opt ~fill =
  List.fold_left (fun acc -> function Some value -> acc +. value | None -> acc +. fill) 0.0

(* ----- Series id ownership ----- *)

type series_owner = Span_owner of Spans.t | Point_owner of Points.t
type owner_claims = { mutable owners : series_owner option array }

let make_owner_claims () = { owners = Array.make 16 None }
let series_id_index (Id id) = id

let same_owner existing claimed =
  match (existing, claimed) with
  | Span_owner a, Span_owner b -> a == b
  | Point_owner a, Point_owner b -> a == b
  | _ -> false

let ensure_owner_capacity claims id =
  if id >= Array.length claims.owners then begin
    let old_length = Array.length claims.owners in
    let new_length = ref (max 1 old_length) in
    while id >= !new_length do
      new_length := !new_length * 2
    done;
    let next = Array.make !new_length None in
    Array.blit claims.owners 0 next 0 old_length;
    claims.owners <- next
  end

let claim_owner claims series_id owner =
  let id = series_id_index series_id in
  ensure_owner_capacity claims id;
  match claims.owners.(id) with
  | None ->
      claims.owners.(id) <- Some owner;
      series_id
  | Some existing when same_owner existing owner -> series_id
  | Some _ -> invalid_arg (Printf.sprintf "series id %d reused by multiple series" id)

let claim_span_series claims series = claim_owner claims (Spans.id series) (Span_owner series)
let claim_point_series claims series = claim_owner claims (Points.id series) (Point_owner series)

let claim_series : type a. owner_claims -> a series -> series_id =
 fun claims -> function
  | Span_series series -> claim_span_series claims series
  | Point_series series -> claim_point_series claims series

(* ----- Series cache ----- *)
type cached_point = Cached_point of point option

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

type span_cache_entry = { cells : span option list SpanCellCache.t; sequence : span Seq.t }

(* ----- Accum checkpoint cache ----- *)

module DateMap = Map.Make (struct
  type t = Date.t

  let compare = Date.compare
end)

type accum_checkpoint = { point : point; tail : span Seq.t }
type accum_cache_entry = { mutable checkpoints : accum_checkpoint DateMap.t; sequence : span Seq.t }
type cell_key = Span_key of int | Point_key of int

module CellValueCache = Hashtbl.Make (struct
  type t = cell_key

  let equal = ( = )
  let hash = Hashtbl.hash
end)

type eval_cell = Span_eval_cell of span | Point_eval_cell of point

type resolving_cell = {
  cell : eval_cell;
  mutable current : float;
  mutable last : float;
  mutable step : int;
}

type eval_state = Resolving of resolving_cell | Resolved of { cell : eval_cell; value : float }

type series_cache = {
  owners : owner_claims;
  point : (series_id, cached_point PointCellCache.t) Hashtbl.t;
  span : (series_id, span_cache_entry) Hashtbl.t;
  accum : (series_id, accum_cache_entry) Hashtbl.t;
  span_formulas : (int, float Formula.t) Hashtbl.t;
  values : eval_state CellValueCache.t;
}

let make_cache () : series_cache =
  {
    owners = make_owner_claims ();
    point = Hashtbl.create 20;
    span = Hashtbl.create 20;
    accum = Hashtbl.create 20;
    span_formulas = Hashtbl.create 20;
    values = CellValueCache.create 100;
  }

let set_point cache series_id date value =
  let cell_cache =
    match Hashtbl.find_opt cache.point series_id with
    | Some cell_cache -> cell_cache
    | None ->
        let c = PointCellCache.create 16 in
        Hashtbl.add cache.point series_id c;
        c
  in
  PointCellCache.replace cell_cache date (Cached_point value)

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

(* Feed an Unfold [cells] stream: each yielded [unfold_cell] becomes a memoized span cell. *)
let make_unfold_producer ~init ~cells ~register_formula =
  let buf = Dynarray.create () in
  let state = ref init in
  let finished = ref false in
  let producing = ref false in
  let advance () =
    if !finished || !producing then false
    else begin
      producing := true;
      let result =
        try cells !state
        with e ->
          producing := false;
          raise e
      in
      producing := false;
      match result with
      | None ->
          finished := true;
          false
      | Some (cell, next_state) ->
          let period, split, formula = Spans.unpack_unfold_cell cell in
          Dynarray.add_last buf (register_formula ~period ~split formula);
          state := next_state;
          true
    end
  in
  let rec view i () =
    if i < Dynarray.length buf then Seq.Cons (Dynarray.get buf i, view (i + 1))
    else if advance () then Seq.Cons (Dynarray.get buf i, view (i + 1))
    else Seq.Nil
  in
  view 0

let rec seq_for_span_series (cache : series_cache) (series : Spans.t) : span Seq.t =
  ignore (claim_span_series cache.owners series : series_id);
  match series with
  | Const { period; value; _ } -> Seq.return (f_value period value const_split)
  | Map { dep; f; _ } ->
      seq_for_span_series cache dep |> Seq.map (fun span -> f_map span f) |> Seq.memoize
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
  | Unfold { deps; init; cells; _ } ->
      let readers = Deps.run (deps ()) in
      let register_formula ~period ~split formula =
        let span =
          f_value period (fun () -> failwith "formula span must be evaluated through Series") split
        in
        Hashtbl.replace cache.span_formulas (span_id span) formula;
        span
      in
      make_unfold_producer ~init ~cells:(cells readers) ~register_formula

and query_span_series cache series period : span option list =
  let series_id = claim_span_series cache.owners series in
  let series_entry_opt = Hashtbl.find_opt cache.span series_id in
  let { cells; sequence } =
    match series_entry_opt with
    | Some entry -> entry
    | None ->
        let c = { cells = SpanCellCache.create 16; sequence = seq_for_span_series cache series } in
        Hashtbl.add cache.span series_id c;
        c
  in
  match SpanCellCache.find_opt cells period with
  | Some values -> values
  | None ->
      let spans = collect_spans sequence period in
      SpanCellCache.replace cells period spans;
      spans

and get_accum_entry cache point_series_id changes : accum_cache_entry =
  match Hashtbl.find_opt cache.accum point_series_id with
  | Some entry -> entry
  | None ->
      let entry = { checkpoints = DateMap.empty; sequence = seq_for_span_series cache changes } in
      Hashtbl.add cache.accum point_series_id entry;
      entry

and query_accum cache point_series_id init changes date : point =
  let entry = get_accum_entry cache point_series_id changes in
  let nearest = DateMap.find_last_opt (fun k -> Date.(k <= date)) entry.checkpoints in
  match nearest with
  | Some (cp_date, cp) when Date.equal cp_date date -> cp.point
  | _ ->
      let base_point, start_tail =
        match nearest with
        | Some (_, cp) -> (Some cp.point, cp.tail)
        | None -> (None, entry.sequence)
      in
      let delta, new_tail = walk_accum_delta start_tail date in
      let new_point = p_accum date init base_point delta in
      entry.checkpoints <- DateMap.add date { point = new_point; tail = new_tail } entry.checkpoints;
      new_point

and query_point_series cache series date : point option =
  let series_id = claim_point_series cache.owners series in
  let cached_value =
    match Hashtbl.find_opt cache.point series_id with
    | Some cache -> PointCellCache.find_opt cache date
    | None -> None
  in
  match cached_value with
  | Some (Cached_point value) -> value
  | None ->
      let value =
        match series with
        | Const { period; value; _ } ->
            if Period.contains date period then Some (p_const date value) else None
        | List { values; _ } -> (
            match List.find_opt (fun (value_date, _) -> Date.equal value_date date) values with
            | Some (_, value) -> Some (p_const date (fun () -> value))
            | None -> None)
        | Map { dep; f; _ } -> (
            match query_point_series cache dep date with
            | Some pt -> Some (p_map pt f)
            | None -> None)
        | Map2 { a; b; f; _ } ->
            let oa, ob = (query_point_series cache a date, query_point_series cache b date) in
            Some (p_derived date [ oa; ob ] (function [ va; vb ] -> f va vb | _ -> assert false))
        | Accum { init; changes; _ } -> Some (query_accum cache series_id init changes date)
      in
      set_point cache series_id date value;
      value

(* ----- Iterative cell resolver ----- *)

exception Evaluation_did_not_converge of { iterations : int; tolerance : float; max_delta : float }

let iteration_tolerance = 1e-6
let max_iterations = 1000

type resolve_roots = Resolve_spans of span option list | Resolve_point of point

let span_key span = Span_key (span_id span)
let point_key point = Point_key (point_id point)

let current_value cache key =
  match CellValueCache.find_opt cache.values key with
  | Some (Resolved { value; _ }) -> value
  | Some (Resolving state) -> state.current
  | None -> invalid_arg "unresolved cell value"

let current_span_value cache span = current_value cache (span_key span)
let current_point_value cache point = current_value cache (point_key point)

let rec current_span_option_values cache = function
  | [] -> []
  | None :: rest -> None :: current_span_option_values cache rest
  | Some span :: rest ->
      Some (current_span_value cache span) :: current_span_option_values cache rest

let rec current_point_option_values cache = function
  | [] -> []
  | None :: rest -> None :: current_point_option_values cache rest
  | Some point :: rest ->
      Some (current_point_value cache point) :: current_point_option_values cache rest

let rec sum_current_span_options cache = function
  | [] -> 0.0
  | None :: rest -> sum_current_span_options cache rest
  | Some span :: rest -> current_span_value cache span +. sum_current_span_options cache rest

let clear_touched cache touched =
  let rec go = function
    | [] -> ()
    | key :: rest ->
        (match CellValueCache.find_opt cache.values key with
        | Some (Resolving _) -> CellValueCache.remove cache.values key
        | Some (Resolved _) | None -> ());
        go rest
  in
  go touched

let rec has_resolving cache = function
  | [] -> false
  | key :: rest -> (
      match CellValueCache.find_opt cache.values key with
      | Some (Resolving _) -> true
      | Some (Resolved _) | None -> has_resolving cache rest)

let finalize_touched cache touched =
  let rec go = function
    | [] -> ()
    | key :: rest ->
        (match CellValueCache.find_opt cache.values key with
        | Some (Resolving state) ->
            CellValueCache.replace cache.values key
              (Resolved { cell = state.cell; value = state.current })
        | Some (Resolved _) | None -> ());
        go rest
  in
  go touched

let rec prime_span cache touched span =
  let key = span_key span in
  match CellValueCache.find_opt cache.values key with
  | Some (Resolved _) -> true
  | Some (Resolving _) -> false
  | None -> (
      let cell = Span_eval_cell span in
      let state = { cell; current = 0.0; last = 0.0; step = 0 } in
      CellValueCache.add cache.values key (Resolving state);
      touched := key :: !touched;
      match prime_span_value cache touched span with
      | Some value ->
          CellValueCache.replace cache.values key (Resolved { cell; value });
          true
      | None -> false)

and prime_span_value cache touched = function
  | Value { id; value; _ } -> (
      match Hashtbl.find_opt cache.span_formulas id with
      | None -> Some (value ())
      | Some formula ->
          if prime_formula cache touched formula then Some (resolved_formula_value cache formula)
          else None)
  | Slice { dep; value; _ } ->
      if prime_span cache touched dep then Some (value (current_span_value cache dep)) else None
  | Map { dep; f; _ } ->
      if prime_span cache touched dep then Some (f (current_span_value cache dep)) else None
  | Map2 { a; b; f; _ } ->
      let a_resolved = prime_span_option cache touched a in
      let b_resolved = prime_span_option cache touched b in
      if a_resolved && b_resolved then
        Some (f (Option.map (current_span_value cache) a) (Option.map (current_span_value cache) b))
      else None

and prime_point cache touched point =
  let key = point_key point in
  match CellValueCache.find_opt cache.values key with
  | Some (Resolved _) -> true
  | Some (Resolving _) -> false
  | None -> (
      let cell = Point_eval_cell point in
      let state = { cell; current = 0.0; last = 0.0; step = 0 } in
      CellValueCache.add cache.values key (Resolving state);
      touched := key :: !touched;
      match prime_point_value cache touched point with
      | Some value ->
          CellValueCache.replace cache.values key (Resolved { cell; value });
          true
      | None -> false)

and prime_point_value cache touched = function
  | Const { value; _ } -> Some (value ())
  | Map { dep; f; _ } ->
      if prime_point cache touched dep then Some (f (current_point_value cache dep)) else None
  | Derived { deps; f; _ } ->
      if prime_point_options cache touched deps then
        Some (f (current_point_option_values cache deps))
      else None
  | Accum { init; base; delta; _ } ->
      let base_resolved = prime_point_option cache touched base in
      let delta_resolved = prime_span_options cache touched delta in
      if base_resolved && delta_resolved then
        let start =
          match base with Some point -> current_point_value cache point | None -> init
        in
        Some (start +. sum_current_span_options cache delta)
      else None

and prime_formula cache touched formula =
  let rec go = function
    | [] -> true
    | query :: rest ->
        let query_resolved = prime_formula_query cache touched query in
        let rest_resolved = go rest in
        query_resolved && rest_resolved
  in
  go (Formula.queries formula)

and prime_formula_query cache touched = function
  | Formula.Span_query_item { series; period } ->
      query_span_series cache series period |> prime_span_options cache touched
  | Formula.Point_query_item { series; date } -> (
      match query_point_series cache series date with
      | Some point -> prime_point cache touched point
      | None -> true)

and prime_span_option cache touched = function
  | None -> true
  | Some span -> prime_span cache touched span

and prime_span_options cache touched = function
  | [] -> true
  | cell :: rest ->
      let cell_resolved = prime_span_option cache touched cell in
      let rest_resolved = prime_span_options cache touched rest in
      cell_resolved && rest_resolved

and prime_point_option cache touched = function
  | None -> true
  | Some point -> prime_point cache touched point

and prime_point_options cache touched = function
  | [] -> true
  | cell :: rest ->
      let cell_resolved = prime_point_option cache touched cell in
      let rest_resolved = prime_point_options cache touched rest in
      cell_resolved && rest_resolved

and resolved_formula_value cache formula =
  fst
    (Formula.eval_with_delta
       ~query_span_values:(fun series period ->
         let values = query_span_series cache series period |> current_span_option_values cache in
         (values, 0.0))
       ~query_point_value:(fun series date ->
         let value =
           Option.map (current_point_value cache) (query_point_series cache series date)
         in
         (value, 0.0))
       formula)

let rec eval_span cache touched iteration span =
  let key = span_key span in
  match CellValueCache.find_opt cache.values key with
  | Some (Resolved { value; _ }) -> (value, 0.0)
  | Some (Resolving state) when state.step = iteration -> (state.current, 0.0)
  | Some (Resolving state) ->
      state.step <- iteration;
      let previous = state.current in
      let value, child_delta = eval_span_value cache touched iteration span in
      let delta = Float.abs (value -. previous) in
      state.last <- previous;
      state.current <- value;
      (value, max delta child_delta)
  | None ->
      ignore (prime_span cache touched span);
      eval_span cache touched iteration span

and eval_span_value cache touched iteration = function
  | Value { id; value; _ } -> (
      match Hashtbl.find_opt cache.span_formulas id with
      | None -> (value (), 0.0)
      | Some formula -> eval_formula cache touched iteration formula)
  | Slice { dep; value; _ } ->
      let dep_value, delta = eval_span cache touched iteration dep in
      (value dep_value, delta)
  | Map { dep; f; _ } ->
      let dep_value, delta = eval_span cache touched iteration dep in
      (f dep_value, delta)
  | Map2 { a; b; f; _ } ->
      let a_value, a_delta = eval_span_option cache touched iteration a in
      let b_value, b_delta = eval_span_option cache touched iteration b in
      (f a_value b_value, max a_delta b_delta)

and eval_point cache touched iteration point =
  let key = point_key point in
  match CellValueCache.find_opt cache.values key with
  | Some (Resolved { value; _ }) -> (value, 0.0)
  | Some (Resolving state) when state.step = iteration -> (state.current, 0.0)
  | Some (Resolving state) ->
      state.step <- iteration;
      let previous = state.current in
      let value, child_delta = eval_point_value cache touched iteration point in
      let delta = Float.abs (value -. previous) in
      state.last <- previous;
      state.current <- value;
      (value, max delta child_delta)
  | None ->
      ignore (prime_point cache touched point);
      eval_point cache touched iteration point

and eval_point_value cache touched iteration = function
  | Const { value; _ } -> (value (), 0.0)
  | Map { dep; f; _ } ->
      let dep_value, delta = eval_point cache touched iteration dep in
      (f dep_value, delta)
  | Derived { deps; f; _ } ->
      let values, delta = eval_point_options cache touched iteration deps in
      (f values, delta)
  | Accum { init; base; delta; _ } ->
      let base_value, base_delta =
        match base with
        | Some point -> eval_point cache touched iteration point
        | None -> (init, 0.0)
      in
      let delta_value, delta_delta = eval_span_option_sum cache touched iteration delta in
      (base_value +. delta_value, max base_delta delta_delta)

and eval_formula cache touched iteration formula =
  Formula.eval_with_delta
    ~query_span_values:(eval_span_query cache touched iteration)
    ~query_point_value:(eval_point_query cache touched iteration)
    formula

and eval_span_query cache touched iteration series period =
  query_span_series cache series period |> eval_span_options cache touched iteration

and eval_point_query cache touched iteration series date =
  match query_point_series cache series date with
  | Some point ->
      let value, delta = eval_point cache touched iteration point in
      (Some value, delta)
  | None -> (None, 0.0)

and eval_span_option cache touched iteration = function
  | Some span ->
      let value, delta = eval_span cache touched iteration span in
      (Some value, delta)
  | None -> (None, 0.0)

and eval_span_options cache touched iteration = function
  | [] -> ([], 0.0)
  | cell :: rest ->
      let value, delta = eval_span_option cache touched iteration cell in
      let values, rest_delta = eval_span_options cache touched iteration rest in
      (value :: values, max delta rest_delta)

and eval_point_option cache touched iteration = function
  | Some point ->
      let value, delta = eval_point cache touched iteration point in
      (Some value, delta)
  | None -> (None, 0.0)

and eval_point_options cache touched iteration = function
  | [] -> ([], 0.0)
  | cell :: rest ->
      let value, delta = eval_point_option cache touched iteration cell in
      let values, rest_delta = eval_point_options cache touched iteration rest in
      (value :: values, max delta rest_delta)

and eval_span_option_sum cache touched iteration = function
  | [] -> (0.0, 0.0)
  | cell :: rest ->
      let value, delta = eval_span_option cache touched iteration cell in
      let rest_value, rest_delta = eval_span_option_sum cache touched iteration rest in
      let value = Option.value ~default:0.0 value +. rest_value in
      (value, max delta rest_delta)

let prime_roots cache touched = function
  | Resolve_spans spans -> ignore (prime_span_options cache touched spans)
  | Resolve_point point -> ignore (prime_point cache touched point)

let eval_roots cache touched iteration = function
  | Resolve_spans spans ->
      let _values, delta = eval_span_options cache touched iteration spans in
      delta
  | Resolve_point point ->
      let _value, delta = eval_point cache touched iteration point in
      delta

let solve_roots cache touched roots =
  let rec loop iteration last_delta =
    if iteration > max_iterations then
      raise
        (Evaluation_did_not_converge
           { iterations = max_iterations; tolerance = iteration_tolerance; max_delta = last_delta })
    else
      let delta = eval_roots cache touched iteration roots in
      if delta <= iteration_tolerance then finalize_touched cache !touched
      else loop (iteration + 1) delta
  in
  if has_resolving cache !touched then loop 1 0.0

let resolve_roots cache roots =
  let touched = ref [] in
  try
    prime_roots cache touched roots;
    solve_roots cache touched roots
  with e ->
    clear_touched cache !touched;
    raise e

let resolve_span_options cache spans =
  resolve_roots cache (Resolve_spans spans);
  current_span_option_values cache spans

let resolve_point cache point =
  resolve_roots cache (Resolve_point point);
  current_point_value cache point

(* ----- Public float-based query API ----- *)

let query_span cache series ~period ~reduce =
  let spans = query_span_series cache series period in
  resolve_span_options cache spans |> reduce

let query_point cache series ~date ~default =
  match query_point_series cache series date with
  | Some point -> resolve_point cache point
  | None -> default

(* ----- Series dependencies ----- *)

module Series_id_set = Set.Make (struct
  type t = series_id

  let compare (Id a) (Id b) = Int.compare a b
end)

let series_dependencies : type a. a series -> packed_series list = function
  | Point_series series -> (
      match series with
      | Const _ -> []
      | List _ -> []
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

let rec build_dependencies claims active_path (Series series) =
  let add_dependency dependency =
    let (Series dep_series) = dependency in
    let dep_id = claim_series claims dep_series in
    if Series_id_set.mem dep_id active_path then
      { series = dependency; dependencies = []; is_back_edge = true }
    else
      {
        series = dependency;
        dependencies = build_dependencies claims (Series_id_set.add dep_id active_path) dependency;
        is_back_edge = false;
      }
  in
  series_dependencies series |> List.map add_dependency

let dependencies : type a. a series -> dependency list =
 fun series ->
  let claims = make_owner_claims () in
  let root_id = claim_series claims series in
  build_dependencies claims (Series_id_set.singleton root_id) (Series series)
