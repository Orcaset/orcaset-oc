(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(* ----- Stable identifiers ----- *)

type series_id = Id of int [@@unboxed]
type span_cell_id = Span_cell_id of int [@@unboxed]
type point_cell_id = Point_cell_id of int [@@unboxed]

let next_series_id = Atomic.make 0
let next_cell_id = Atomic.make 0
let new_id () : series_id = Id (Atomic.fetch_and_add next_series_id 1)
let new_span_cell_id () = Span_cell_id (Atomic.fetch_and_add next_cell_id 1)
let new_point_cell_id () = Point_cell_id (Atomic.fetch_and_add next_cell_id 1)

type split = Split.t

type 'key key_ops = {
  equal : 'key -> 'key -> bool;
  hash : 'key -> int;
  compare : 'key -> 'key -> int;
  to_string : 'key -> string;
}

(* ----- Span aggregation ----- *)

module Agg = struct
  type sample = { period : Period.t; value : float }
  type t = Agg of (sample option list -> float option)

  let make reduce = Agg reduce
  let reduce (Agg f) samples = f samples
  let present samples = List.filter_map Fun.id samples

  let sum =
    make (fun samples ->
        match present samples with
        | [] -> None
        | xs -> Some (List.fold_left (fun total sample -> total +. sample.value) 0.0 xs))

  let min =
    make (fun samples ->
        match present samples with
        | [] -> None
        | first :: rest ->
            Some (List.fold_left (fun acc sample -> Float.min acc sample.value) first.value rest))

  let max =
    make (fun samples ->
        match present samples with
        | [] -> None
        | first :: rest ->
            Some (List.fold_left (fun acc sample -> Float.max acc sample.value) first.value rest))

  let average =
    make (fun samples ->
        let total, count =
          samples |> present
          |> List.fold_left
               (fun (total, count) sample -> (total +. sample.value, count + 1))
               (0.0, 0)
        in
        if count = 0 then None else Some (total /. Float.of_int count))

  let time_weighted_average year_frac =
    make (fun samples ->
        let weighted_total, total_weight =
          samples |> present
          |> List.fold_left
               (fun (weighted_total, total_weight) sample ->
                 let start, end_ = Period.to_tuple sample.period in
                 let weight = year_frac start end_ in
                 (weighted_total +. (sample.value *. weight), total_weight +. weight))
               (0.0, 0.0)
        in
        if Float.equal total_weight 0.0 then None else Some (weighted_total /. total_weight))
end

(* ----- Families ----- *)

module Family = struct
  type ('key, 'series) t = {
    id : string;
    key : 'key key_ops;
    active_keys : Period.t -> 'key list;
    member : 'key -> 'series;
    mutable active_cache : (Period.t * 'key list) list;
    mutable member_cache : ('key * 'series) list;
  }

  let validate_unique family keys =
    let rec loop seen = function
      | [] -> ()
      | key :: rest ->
          if List.exists (family.key.equal key) seen then
            invalid_arg
              ("Family.active_keys returned duplicate key for " ^ family.id ^ ": "
             ^ family.key.to_string key)
          else loop (key :: seen) rest
    in
    loop [] keys

  let make ~id ~key ~active_keys ~member =
    { id; key; active_keys; member; active_cache = []; member_cache = [] }

  let active_keys family period =
    match List.find_opt (fun (p, _) -> Period.equal p period) family.active_cache with
    | Some (_, keys) -> keys
    | None ->
        let keys = family.active_keys period in
        validate_unique family keys;
        family.active_cache <- (period, keys) :: family.active_cache;
        keys

  let member family key =
    match List.find_opt (fun (k, _) -> family.key.equal k key) family.member_cache with
    | Some (_, series) -> series
    | None ->
        let series = family.member key in
        family.member_cache <- (key, series) :: family.member_cache;
        series

  let members family period =
    active_keys family period |> List.map (fun key -> (key, member family key))

  let series family period = members family period |> List.map snd
  let id family = family.id
  let key_equal family = family.key.equal
  let key_compare family = family.key.compare
  let key_to_string family = family.key.to_string
end

(* ----- Trace values ----- *)

module Trace = struct
  type span_cell_info = { id : span_cell_id; period : Period.t; series_label : string option }
  type point_cell_info = { id : point_cell_id; date : Date.t; series_label : string option }
  type cell = Span_cell of span_cell_info | Point_cell of point_cell_info

  type query =
    | Span_query of { period : Period.t; label : string option }
    | Point_query of { date : Date.t; label : string option }
    | Span_cell_value
    | Point_cell_value

  type edge = { from : cell; to_ : cell; query : query; is_back_edge : bool }
  type t = { roots : cell list; edges : edge list }

  let roots t = t.roots
  let edges t = t.edges
end

(* ----- Public series/formula definitions ----- *)

module rec Spans : sig
  type unfold_cell

  type +'tag t =
    | Const : {
        id : series_id;
        label : string option;
        split : split;
        agg : Agg.t;
        period : Period.t;
        value : float;
      }
        -> 'tag t
    | List : {
        id : series_id;
        label : string option;
        split : split;
        agg : Agg.t;
        values : (Period.t * float) list;
      }
        -> 'tag t
    | Map : {
        id : series_id;
        label : string option;
        agg : Agg.t;
        dep : 'in_tag t;
        f : float -> float;
      }
        -> 'out_tag t
    | Map2 : {
        id : series_id;
        label : string option;
        agg : Agg.t;
        a : 'a_tag t;
        b : 'b_tag t;
        f : float option -> float option -> float option;
      }
        -> 'out_tag t
    | MapN : {
        id : series_id;
        label : string option;
        agg : Agg.t;
        deps : packed list;
        f : float option list -> float option;
      }
        -> 'out_tag t
    | Sum_family : {
        id : series_id;
        label : string option;
        agg : Agg.t;
        family : ('key, 'member_tag t) Family.t;
      }
        -> 'out_tag t
    | Extend : { id : series_id; agg : Agg.t; a : 'tag t; b : 'tag t } -> 'tag t
    | Clipped : { id : series_id; after : Date.t; until : Date.t; base : 'tag t } -> 'tag t
    | With_agg : { id : series_id; agg : Agg.t; base : 'tag t } -> 'tag t
    | Unfold : {
        id : series_id;
        label : string option;
        agg : Agg.t;
        init : 'state;
        cells : 'state -> (unfold_cell * 'state) option;
      }
        -> 'tag t
    | Unfold_from : {
        id : series_id;
        label : string option;
        agg : Agg.t;
        base : 'tag t;
        cells : Period.t -> (unfold_cell * Period.t) option;
      }
        -> 'tag t

  and packed = Pack : 'tag t -> packed

  val pack : 'tag t -> packed
  val neg : ?label:string -> 'in_tag t -> 'out_tag t
  val scale : ?label:string -> float -> 'in_tag t -> 'out_tag t
  val sum : ?label:string -> agg:Agg.t -> packed list -> 'out_tag t
  val sub : ?label:string -> agg:Agg.t -> 'a_tag t -> 'b_tag t -> 'out_tag t
  val mul : ?label:string -> agg:Agg.t -> packed list -> 'out_tag t
  val div : ?label:string -> agg:Agg.t -> 'a_tag t -> 'b_tag t -> 'out_tag t
  val sum_family : ?label:string -> agg:Agg.t -> ('key, 'tag t) Family.t -> 'out_tag t
  val const : ?label:string -> split:split -> agg:Agg.t -> period:Period.t -> float -> 'tag t
  val of_list : ?label:string -> split:split -> agg:Agg.t -> (Period.t * float) list -> 'tag t
  val map : ?label:string -> (float -> float) -> 'in_tag t -> 'out_tag t

  val map2 :
    ?label:string ->
    agg:Agg.t ->
    'a_tag t ->
    'b_tag t ->
    (float option -> float option -> float option) ->
    'out_tag t

  val mapn :
    ?label:string -> agg:Agg.t -> packed list -> (float option list -> float option) -> 'out_tag t

  val extend : agg:Agg.t -> 'tag t -> 'tag t -> 'tag t
  val clipped : after:Date.t -> until:Date.t -> 'tag t -> 'tag t
  val after : Date.t -> 'tag t -> 'tag t
  val until : Date.t -> 'tag t -> 'tag t

  val unfold :
    ?label:string ->
    agg:Agg.t ->
    init:'state ->
    cells:('state -> (unfold_cell * 'state) option) ->
    unit ->
    'tag t

  val unfold_from :
    ?label:string ->
    agg:Agg.t ->
    cells:(Period.t -> (unfold_cell * Period.t) option) ->
    'tag t ->
    'tag t

  val unfold_rec :
    ?label:string ->
    agg:Agg.t ->
    init:'state ->
    cells:(self:'tag t -> 'state -> (unfold_cell * 'state) option) ->
    unit ->
    'tag t

  val cell : period:Period.t -> split:split -> float option Formula.t -> unfold_cell
  val unpack_unfold_cell : unfold_cell -> Period.t * split * float option Formula.t
  val id : 'tag t -> series_id
  val label : 'tag t -> string option
  val agg : 'tag t -> Agg.t
  val with_agg : agg:Agg.t -> 'tag t -> 'tag t
end = struct
  type unfold_cell =
    | Cell of { period : Period.t; split : split; formula : float option Formula.t }

  type +'tag t =
    | Const : {
        id : series_id;
        label : string option;
        split : split;
        agg : Agg.t;
        period : Period.t;
        value : float;
      }
        -> 'tag t
    | List : {
        id : series_id;
        label : string option;
        split : split;
        agg : Agg.t;
        values : (Period.t * float) list;
      }
        -> 'tag t
    | Map : {
        id : series_id;
        label : string option;
        agg : Agg.t;
        dep : 'in_tag t;
        f : float -> float;
      }
        -> 'out_tag t
    | Map2 : {
        id : series_id;
        label : string option;
        agg : Agg.t;
        a : 'a_tag t;
        b : 'b_tag t;
        f : float option -> float option -> float option;
      }
        -> 'out_tag t
    | MapN : {
        id : series_id;
        label : string option;
        agg : Agg.t;
        deps : packed list;
        f : float option list -> float option;
      }
        -> 'out_tag t
    | Sum_family : {
        id : series_id;
        label : string option;
        agg : Agg.t;
        family : ('key, 'member_tag t) Family.t;
      }
        -> 'out_tag t
    | Extend : { id : series_id; agg : Agg.t; a : 'tag t; b : 'tag t } -> 'tag t
    | Clipped : { id : series_id; after : Date.t; until : Date.t; base : 'tag t } -> 'tag t
    | With_agg : { id : series_id; agg : Agg.t; base : 'tag t } -> 'tag t
    | Unfold : {
        id : series_id;
        label : string option;
        agg : Agg.t;
        init : 'state;
        cells : 'state -> (unfold_cell * 'state) option;
      }
        -> 'tag t
    | Unfold_from : {
        id : series_id;
        label : string option;
        agg : Agg.t;
        base : 'tag t;
        cells : Period.t -> (unfold_cell * Period.t) option;
      }
        -> 'tag t

  and packed = Pack : 'tag t -> packed

  let pack series = Pack series
  let cell ~period ~split formula = Cell { period; split; formula }
  let unpack_unfold_cell (Cell { period; split; formula }) = (period, split, formula)

  let const ?label ~split ~agg ~period value =
    Const { id = new_id (); label; split; agg; period; value }

  let of_list ?label ~split ~agg values = List { id = new_id (); label; split; agg; values }

  let clipped ~after ~until base =
    if Date.(until < after) then invalid_arg "Spans.clipped: until before after";
    Clipped { id = new_id (); after; until; base }

  let after date base = clipped ~after:date ~until:Date.upper_bound base
  let until date base = clipped ~after:Date.lower_bound ~until:date base
  let unfold ?label ~agg ~init ~cells () = Unfold { id = new_id (); label; agg; init; cells }
  let unfold_from ?label ~agg ~cells base = Unfold_from { id = new_id (); label; agg; base; cells }

  let unfold_rec ?label ~agg ~init ~cells () =
    let rec self =
      Unfold { id = new_id (); label; agg; init; cells = (fun state -> cells ~self state) }
    in
    self

  let id = function
    | Const { id; _ }
    | List { id; _ }
    | Map { id; _ }
    | Map2 { id; _ }
    | MapN { id; _ }
    | Sum_family { id; _ }
    | Extend { id; _ }
    | Clipped { id; _ }
    | With_agg { id; _ }
    | Unfold { id; _ }
    | Unfold_from { id; _ } ->
        id

  let rec label = function
    | Const { label; _ }
    | List { label; _ }
    | Map { label; _ }
    | Map2 { label; _ }
    | MapN { label; _ }
    | Sum_family { label; _ }
    | Unfold { label; _ }
    | Unfold_from { label; _ } ->
        label
    | Extend { a; _ } | Clipped { base = a; _ } | With_agg { base = a; _ } -> label a

  let rec agg = function
    | Const { agg; _ }
    | List { agg; _ }
    | Map { agg; _ }
    | Map2 { agg; _ }
    | MapN { agg; _ }
    | Sum_family { agg; _ }
    | Extend { agg; _ }
    | With_agg { agg; _ }
    | Unfold { agg; _ }
    | Unfold_from { agg; _ } ->
        agg
    | Clipped { base; _ } -> agg base

  let with_agg ~agg base = With_agg { id = new_id (); agg; base }
  let map ?label f dep = Map { id = new_id (); label; agg = agg dep; dep; f }
  let neg ?label dep = map ?label (fun value -> -.value) dep
  let scale ?label factor dep = map ?label (fun value -> factor *. value) dep
  let map2 ?label ~agg a b f = Map2 { id = new_id (); label; agg; a; b; f }
  let mapn ?label ~agg deps f = MapN { id = new_id (); label; agg; deps; f }
  let extend ~agg a b = Extend { id = new_id (); agg; a; b }
  let sum_family ?label ~agg family = Sum_family { id = new_id (); label; agg; family }

  let sum ?label ~agg = function
    | [] -> invalid_arg "Spans.sum: empty list"
    | [ Pack x ] -> Map { id = new_id (); label; agg; dep = x; f = Fun.id }
    | deps ->
        mapn ?label ~agg deps (fun values ->
            match List.filter_map Fun.id values with
            | [] -> None
            | present -> Some (List.fold_left ( +. ) 0.0 present))

  let mul ?label ~agg = function
    | [] -> invalid_arg "Spans.mul: empty list"
    | [ Pack x ] -> Map { id = new_id (); label; agg; dep = x; f = Fun.id }
    | deps ->
        mapn ?label ~agg deps (fun values ->
            if List.for_all Option.is_some values then
              Some (List.fold_left (fun acc value -> acc *. Option.get value) 1.0 values)
            else None)

  let sub ?label ~agg a b =
    map2 ?label ~agg a b (fun a b ->
        match (a, b) with
        | None, None -> None
        | _ -> Some (Option.value ~default:0.0 a -. Option.value ~default:0.0 b))

  let div ?label ~agg a b =
    map2 ?label ~agg a b (fun a b ->
        match (a, b) with Some a, Some b -> Some (a /. b) | _ -> None)
end

and Points : sig
  type +'tag t =
    | Const : { id : series_id; label : string option; period : Period.t; value : float } -> 'tag t
    | List : { id : series_id; label : string option; values : (Date.t * float) list } -> 'tag t
    | Map : {
        id : series_id;
        label : string option;
        dep : 'in_tag t;
        f : float -> float;
      }
        -> 'out_tag t
    | Map2 : {
        id : series_id;
        label : string option;
        a : 'a_tag t;
        b : 'b_tag t;
        f : float option -> float option -> float option;
      }
        -> 'out_tag t
    | MapN : {
        id : series_id;
        label : string option;
        deps : packed list;
        f : float option list -> float option;
      }
        -> 'out_tag t
    | Accum : {
        id : series_id;
        label : string option;
        init : float;
        changes : 'change_tag Spans.t;
      }
        -> 'out_tag t

  and packed = Pack : 'tag t -> packed

  val pack : 'tag t -> packed
  val neg : ?label:string -> 'in_tag t -> 'out_tag t
  val scale : ?label:string -> float -> 'in_tag t -> 'out_tag t
  val sum : ?label:string -> packed list -> 'out_tag t
  val sub : ?label:string -> 'a_tag t -> 'b_tag t -> 'out_tag t
  val mul : ?label:string -> packed list -> 'out_tag t
  val div : ?label:string -> 'a_tag t -> 'b_tag t -> 'out_tag t
  val const : ?label:string -> period:Period.t -> float -> 'tag t
  val of_list : ?label:string -> (Date.t * float) list -> 'tag t
  val map : ?label:string -> (float -> float) -> 'in_tag t -> 'out_tag t

  val map2 :
    ?label:string ->
    'a_tag t ->
    'b_tag t ->
    (float option -> float option -> float option) ->
    'out_tag t

  val mapn : ?label:string -> packed list -> (float option list -> float option) -> 'out_tag t
  val accum : ?label:string -> init:float -> 'change_tag Spans.t -> 'out_tag t
  val id : 'tag t -> series_id
  val label : 'tag t -> string option
end = struct
  type +'tag t =
    | Const : { id : series_id; label : string option; period : Period.t; value : float } -> 'tag t
    | List : { id : series_id; label : string option; values : (Date.t * float) list } -> 'tag t
    | Map : {
        id : series_id;
        label : string option;
        dep : 'in_tag t;
        f : float -> float;
      }
        -> 'out_tag t
    | Map2 : {
        id : series_id;
        label : string option;
        a : 'a_tag t;
        b : 'b_tag t;
        f : float option -> float option -> float option;
      }
        -> 'out_tag t
    | MapN : {
        id : series_id;
        label : string option;
        deps : packed list;
        f : float option list -> float option;
      }
        -> 'out_tag t
    | Accum : {
        id : series_id;
        label : string option;
        init : float;
        changes : 'change_tag Spans.t;
      }
        -> 'out_tag t

  and packed = Pack : 'tag t -> packed

  let pack series = Pack series
  let const ?label ~period value = Const { id = new_id (); label; period; value }
  let of_list ?label values = List { id = new_id (); label; values }
  let accum ?label ~init changes = Accum { id = new_id (); label; init; changes }
  let map ?label f dep = Map { id = new_id (); label; dep; f }
  let neg ?label dep = map ?label (fun value -> -.value) dep
  let scale ?label factor dep = map ?label (fun value -> factor *. value) dep
  let map2 ?label a b f = Map2 { id = new_id (); label; a; b; f }
  let mapn ?label deps f = MapN { id = new_id (); label; deps; f }

  let id = function
    | Const { id; _ }
    | List { id; _ }
    | Map { id; _ }
    | Map2 { id; _ }
    | MapN { id; _ }
    | Accum { id; _ } ->
        id

  let label = function
    | Const { label; _ }
    | List { label; _ }
    | Map { label; _ }
    | Map2 { label; _ }
    | MapN { label; _ }
    | Accum { label; _ } ->
        label

  let sum ?label = function
    | [] -> invalid_arg "Points.sum: empty list"
    | [ Pack x ] -> map ?label Fun.id x
    | deps ->
        mapn ?label deps (fun values ->
            match List.filter_map Fun.id values with
            | [] -> None
            | present -> Some (List.fold_left ( +. ) 0.0 present))

  let mul ?label = function
    | [] -> invalid_arg "Points.mul: empty list"
    | [ Pack x ] -> map ?label Fun.id x
    | deps ->
        mapn ?label deps (fun values ->
            if List.for_all Option.is_some values then
              Some (List.fold_left (fun acc value -> acc *. Option.get value) 1.0 values)
            else None)

  let sub ?label a b =
    map2 ?label a b (fun a b ->
        match (a, b) with
        | None, None -> None
        | _ -> Some (Option.value ~default:0.0 a -. Option.value ~default:0.0 b))

  let div ?label a b =
    map2 ?label a b (fun a b -> match (a, b) with Some a, Some b -> Some (a /. b) | _ -> None)
end

and Formula : sig
  type 'a t

  type packed_query =
    | Span_query_item : { series : 'tag Spans.t; period : Period.t } -> packed_query
    | Point_query_item : { series : 'tag Points.t; date : Date.t } -> packed_query
    | Span_cell_value_item : span_cell_id -> packed_query
    | Point_cell_value_item : point_cell_id -> packed_query

  val pure : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val span_query : 'tag Spans.t -> period:Period.t -> float option t
  val point_query : 'tag Points.t -> date:Date.t -> float option t
  val span_cell_value : span_cell_id -> float option t
  val point_cell_value : point_cell_id -> float option t
  val queries : 'a t -> packed_query list

  type span_query_values = {
    query_span_values : 'tag. 'tag Spans.t -> Period.t -> Agg.sample option list * float;
  }

  type point_query_value = {
    query_point_value : 'tag. 'tag Points.t -> Date.t -> float option * float;
  }

  type span_cell_query = { query_span_cell_value : span_cell_id -> float option * float }
  type point_cell_query = { query_point_cell_value : point_cell_id -> float option * float }

  val eval_with_delta :
    query_span_values:span_query_values ->
    query_point_value:point_query_value ->
    query_span_cell_value:span_cell_query ->
    query_point_cell_value:point_cell_query ->
    'a t ->
    'a * float
end = struct
  type _ query =
    | Span_query : { series : 'tag Spans.t; period : Period.t } -> float option query
    | Point_query : { series : 'tag Points.t; date : Date.t } -> float option query
    | Span_cell_value : span_cell_id -> float option query
    | Point_cell_value : point_cell_id -> float option query

  type 'a t =
    | Pure : 'a -> 'a t
    | Map : ('a -> 'b) * 'a t -> 'b t
    | Map2 : ('a -> 'b -> 'c) * 'a t * 'b t -> 'c t
    | Query : 'a query -> 'a t

  type packed_query =
    | Span_query_item : { series : 'tag Spans.t; period : Period.t } -> packed_query
    | Point_query_item : { series : 'tag Points.t; date : Date.t } -> packed_query
    | Span_cell_value_item : span_cell_id -> packed_query
    | Point_cell_value_item : point_cell_id -> packed_query

  let pure x = Pure x
  let map f x = Map (f, x)
  let map2 f a b = Map2 (f, a, b)
  let ( let+ ) x f = map f x
  let ( and+ ) a b = map2 (fun x y -> (x, y)) a b
  let span_query series ~period = Query (Span_query { series; period })
  let point_query series ~date = Query (Point_query { series; date })
  let span_cell_value id = Query (Span_cell_value id)
  let point_cell_value id = Query (Point_cell_value id)

  let rec queries : type a. a t -> packed_query list = function
    | Pure _ -> []
    | Map (_, x) -> queries x
    | Map2 (_, a, b) -> queries a @ queries b
    | Query (Span_query { series; period }) -> [ Span_query_item { series; period } ]
    | Query (Point_query { series; date }) -> [ Point_query_item { series; date } ]
    | Query (Span_cell_value id) -> [ Span_cell_value_item id ]
    | Query (Point_cell_value id) -> [ Point_cell_value_item id ]

  type span_query_values = {
    query_span_values : 'tag. 'tag Spans.t -> Period.t -> Agg.sample option list * float;
  }

  type point_query_value = {
    query_point_value : 'tag. 'tag Points.t -> Date.t -> float option * float;
  }

  type span_cell_query = { query_span_cell_value : span_cell_id -> float option * float }
  type point_cell_query = { query_point_cell_value : point_cell_id -> float option * float }

  let eval_with_delta (type a) ~query_span_values ~query_point_value ~query_span_cell_value
      ~query_point_cell_value (formula : a t) : a * float =
    let rec go : type a. a t -> a * float = function
      | Pure x -> (x, 0.0)
      | Map (f, x) ->
          let value, delta = go x in
          (f value, delta)
      | Map2 (f, a, b) ->
          let a_value, a_delta = go a in
          let b_value, b_delta = go b in
          (f a_value b_value, max a_delta b_delta)
      | Query (Span_query { series; period }) ->
          let samples, delta = query_span_values.query_span_values series period in
          (Agg.reduce (Spans.agg series) samples, delta)
      | Query (Point_query { series; date }) -> query_point_value.query_point_value series date
      | Query (Span_cell_value id) -> query_span_cell_value.query_span_cell_value id
      | Query (Point_cell_value id) -> query_point_cell_value.query_point_cell_value id
    in
    go formula
end

(* ----- Existentially packed public series ----- *)

type span_kind
type point_kind

type (_, _) series =
  | Point_series : 'tag Points.t -> (point_kind, 'tag) series
  | Span_series : 'tag Spans.t -> (span_kind, 'tag) series

let label : type kind tag. (kind, tag) series -> string option = function
  | Point_series series -> Points.label series
  | Span_series series -> Spans.label series

type packed_series = Series : ('kind, 'tag) series -> packed_series

(* ----- Materialized cells ----- *)

type span_cell = {
  id : span_cell_id;
  owner : series_id;
  series_label : string option;
  period : Period.t;
  split : split;
  formula : float option Formula.t;
}

type point_cell = {
  id : point_cell_id;
  owner : series_id;
  series_label : string option;
  date : Date.t;
  formula : float option Formula.t;
}

type span_segment = Present of span_cell | Missing of Period.t

let span_id_int (Span_cell_id id) = id
let point_id_int (Point_cell_id id) = id

module SeriesIdHash = struct
  type t = series_id

  let equal (Id a) (Id b) = Int.equal a b
  let hash (Id id) = id
end

module SeriesIdTable = Hashtbl.Make (SeriesIdHash)

module PeriodTable = Hashtbl.Make (struct
  type t = Period.t

  let equal = Period.equal
  let hash = Period.hash
end)

module DateTable = Hashtbl.Make (struct
  type t = Date.t

  let equal = Date.equal
  let hash = Date.hash
end)

type cell_key = Span_key of int | Point_key of int

module CellValueCache = Hashtbl.Make (struct
  type t = cell_key

  let equal = ( = )
  let hash = Hashtbl.hash
end)

type eval_cell = Span_eval_cell of span_cell | Point_eval_cell of point_cell
type resolving_cell = { cell : eval_cell; mutable current : float option; mutable step : int }

type eval_state =
  | Resolving of resolving_cell
  | Resolved of { cell : eval_cell; value : float option }

type span_cache_entry = {
  cells : span_segment list PeriodTable.t;
  mutable sequence : span_cell Seq.t option;
}

type point_cache_entry = point_cell option DateTable.t

type series_cache = {
  span : span_cache_entry SeriesIdTable.t;
  point : point_cache_entry SeriesIdTable.t;
  span_cells : (int, span_cell) Hashtbl.t;
  point_cells : (int, point_cell) Hashtbl.t;
  values : eval_state CellValueCache.t;
}

let make_cache () =
  {
    span = SeriesIdTable.create 20;
    point = SeriesIdTable.create 20;
    span_cells = Hashtbl.create 100;
    point_cells = Hashtbl.create 100;
    values = CellValueCache.create 100;
  }

let make_span_cell cache ~owner ~series_label ~period ~split formula =
  let id = new_span_cell_id () in
  let cell = { id; owner; series_label; period; split; formula } in
  Hashtbl.replace cache.span_cells (span_id_int id) cell;
  cell

let make_point_cell cache ~owner ~series_label ~date formula =
  let id = new_point_cell_id () in
  let cell = { id; owner; series_label; date; formula } in
  Hashtbl.replace cache.point_cells (point_id_int id) cell;
  cell

let span_key (cell : span_cell) = Span_key (span_id_int cell.id)
let point_key (cell : point_cell) = Point_key (point_id_int cell.id)

(* ----- Span cell splitting, clipping, collection ----- *)

let split_period period date =
  let start, end_ = Period.to_tuple period in
  (Period.make start date, Period.make date end_)

let split_span_cell cache date cell =
  let start, end_ = Period.to_tuple cell.period in
  if Date.(date <= start) then (None, Some cell)
  else if Date.(date >= end_) then (Some cell, None)
  else
    let left_period, right_period = split_period cell.period date in
    let left, right = cell.split ~period:cell.period ~date in
    let mk period part =
      make_span_cell cache ~owner:cell.owner ~series_label:cell.series_label ~period
        ~split:cell.split
        (Formula.map (Option.map (Split.value part)) (Formula.span_cell_value cell.id))
    in
    (Some (mk left_period left), Some (mk right_period right))

let clip_span_cell cache bounds cell =
  let q_start, q_end = Period.to_tuple bounds in
  let c_start, c_end = Period.to_tuple cell.period in
  if Date.(c_end <= q_start) || Date.(c_start >= q_end) then None
  else
    let cell =
      if Date.(c_start < q_start) then
        match split_span_cell cache q_start cell with _, Some right -> right | _, None -> cell
      else cell
    in
    let c_end = Period.end_ cell.period in
    if Date.(c_end > q_end) then
      match split_span_cell cache q_end cell with Some left, _ -> Some left | None, _ -> Some cell
    else Some cell

let missing_if_nonempty period = if Period.days period = 0 then [] else [ Missing period ]

let collect_cells cache seq period =
  let period_start, period_end = Period.to_tuple period in
  let rec go seq acc =
    match seq () with
    | Seq.Nil -> List.rev acc
    | Seq.Cons (cell, rest) ->
        let cell_start, cell_end = Period.to_tuple cell.period in
        if Date.(cell_start >= period_end) then List.rev acc
        else
          let acc =
            match clip_span_cell cache period cell with
            | Some clipped -> clipped :: acc
            | None -> acc
          in
          if Date.(cell_end >= period_end) then List.rev acc else go rest acc
  in
  let add_gap start end_ acc =
    if Date.(start < end_) then Missing (Period.make start end_) :: acc else acc
  in
  let rec with_gaps cursor acc = function
    | [] -> List.rev (add_gap cursor period_end acc)
    | cell :: rest ->
        let cell_start, cell_end = Period.to_tuple cell.period in
        let acc = add_gap cursor cell_start acc in
        let cursor = if Date.(cursor < cell_end) then cell_end else cursor in
        with_gaps cursor (Present cell :: acc) rest
  in
  if Date.equal period_start period_end then []
  else match go seq [] with [] -> [ Missing period ] | cells -> with_gaps period_start [] cells

let seq_uncons seq = match seq () with Seq.Nil -> None | Seq.Cons (head, tail) -> Some (head, tail)

let seq_memoize seq =
  let buf = ref [] in
  let source = ref seq in
  let finished = ref false in
  let rec view i () =
    if i < List.length !buf then Seq.Cons (List.nth !buf i, view (i + 1))
    else if !finished then Seq.Nil
    else
      match !source () with
      | Seq.Nil ->
          finished := true;
          Seq.Nil
      | Seq.Cons (head, tail) ->
          buf := !buf @ [ head ];
          source := tail;
          Seq.Cons (head, view (i + 1))
  in
  view 0

let rec align_span_seqs cache (seqs : span_cell Seq.t list) : span_cell option list Seq.t =
 fun () ->
  let heads = List.map (fun seq -> (seq_uncons seq, seq)) seqs in
  let min_start =
    List.fold_left
      (fun acc (head, _) ->
        match head with
        | None -> acc
        | Some (cell, _) ->
            let start = Period.start cell.period in
            Some
              (match acc with
              | None -> start
              | Some current -> if Date.(start < current) then start else current))
      None heads
  in
  match min_start with
  | None -> Seq.Nil
  | Some min_start ->
      let row_end =
        List.fold_left
          (fun acc (head, _) ->
            match head with
            | None -> acc
            | Some (cell, _) ->
                let start, end_ = Period.to_tuple cell.period in
                let candidate = if Date.equal start min_start then end_ else start in
                Some
                  (match acc with
                  | None -> candidate
                  | Some e -> if Date.(candidate < e) then candidate else e))
          None heads
        |> Option.get
      in
      let row_and_next =
        List.map
          (fun (head, original) ->
            match head with
            | None -> (None, original)
            | Some (cell, rest) ->
                let cell_start, cell_end = Period.to_tuple cell.period in
                if Date.(cell_start > min_start) then (None, original)
                else if Date.equal cell_end row_end then (Some cell, rest)
                else
                  let left, right = split_span_cell cache row_end cell in
                  let next = match right with Some r -> Seq.cons r rest | None -> rest in
                  (left, next))
          heads
      in
      Seq.Cons (List.map fst row_and_next, align_span_seqs cache (List.map snd row_and_next))

let align_span_seq cache a b =
  align_span_seqs cache [ a; b ] |> Seq.map (function [ a; b ] -> (a, b) | _ -> assert false)

let formula_of_cell_option = function
  | None -> Formula.pure None
  | Some (cell : span_cell) -> Formula.span_cell_value cell.id

let formula_of_cell_options f cells =
  let rec combine = function
    | [] -> Formula.pure []
    | cell :: rest ->
        let open Formula in
        let+ value = formula_of_cell_option cell and+ rest = combine rest in
        value :: rest
  in
  Formula.map f (combine cells)

(* ----- Span sequence/materialization ----- *)

let register_unfold_formula cache ~owner ~series_label ~period ~split formula =
  make_span_cell cache ~owner ~series_label ~period ~split formula

let make_unfold_producer ~init ~cells ~register_formula =
  let buf = ref [] in
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
          buf := !buf @ [ register_formula ~period ~split formula ];
          state := next_state;
          true
    end
  in
  let rec view i () =
    if i < List.length !buf then Seq.Cons (List.nth !buf i, view (i + 1))
    else if advance () then Seq.Cons (List.nth !buf i, view (i + 1))
    else Seq.Nil
  in
  view 0

let unfold_from_span_seq base ~cells ~register_formula =
  let continuation = ref None in
  let continuation_from period =
    match !continuation with
    | Some seq -> seq
    | None ->
        let seq = make_unfold_producer ~init:period ~cells ~register_formula in
        continuation := Some seq;
        seq
  in
  let rec replay_base last_period base () =
    match base () with
    | Seq.Nil -> (
        match last_period with None -> Seq.Nil | Some period -> continuation_from period ())
    | Seq.Cons (cell, rest) -> Seq.Cons (cell, replay_base (Some cell.period) rest)
  in
  replay_base None base

let extend_span_seq cache a b =
  let rec continue_b start b () =
    match b () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (cell, rest) ->
        let cell_start, cell_end = Period.to_tuple cell.period in
        if Date.(cell_end <= start) then continue_b start rest ()
        else if Date.(cell_start < start) then
          match split_span_cell cache start cell with
          | _, Some right -> Seq.Cons (right, rest)
          | _, None -> continue_b start rest ()
        else Seq.Cons (cell, rest)
  in
  let rec continue_a last_end a () =
    match a () with
    | Seq.Nil -> ( match last_end with None -> b () | Some end_ -> continue_b end_ b ())
    | Seq.Cons (cell, rest) -> Seq.Cons (cell, continue_a (Some (Period.end_ cell.period)) rest)
  in
  continue_a None a

let clipped_span_seq cache ~after ~until seq =
  if Date.equal after until then Seq.empty
  else
    let bounds = Period.make after until in
    let rec go seq () =
      match seq () with
      | Seq.Nil -> Seq.Nil
      | Seq.Cons (cell, rest) -> (
          let start, end_ = Period.to_tuple cell.period in
          if Date.(end_ <= after) then go rest ()
          else if Date.(start >= until) then Seq.Nil
          else
            match clip_span_cell cache bounds cell with
            | Some clipped -> Seq.Cons (clipped, go rest)
            | None -> go rest ())
    in
    go seq

let span_entry cache id =
  match SeriesIdTable.find_opt cache.span id with
  | Some entry -> entry
  | None ->
      let entry = { cells = PeriodTable.create 16; sequence = None } in
      SeriesIdTable.add cache.span id entry;
      entry

let rec seq_for_span_series : type tag. series_cache -> tag Spans.t -> span_cell Seq.t =
 fun cache series ->
  let id = Spans.id series in
  let series_label = Spans.label series in
  let entry = span_entry cache id in
  match entry.sequence with
  | Some seq -> seq
  | None ->
      let seq =
        match series with
        | Spans.Const { period; value; split; _ } ->
            Seq.return
              (make_span_cell cache ~owner:id ~series_label ~period ~split
                 (Formula.pure (Some value)))
        | Spans.List { values; split; _ } ->
            values |> List.to_seq
            |> Seq.map (fun (period, value) ->
                make_span_cell cache ~owner:id ~series_label ~period ~split
                  (Formula.pure (Some value)))
        | Spans.Map { dep; f; _ } ->
            seq_for_span_series cache dep
            |> Seq.map (fun dep_cell ->
                make_span_cell cache ~owner:id ~series_label ~period:dep_cell.period
                  ~split:dep_cell.split
                  (Formula.map (Option.map f) (Formula.span_cell_value dep_cell.id)))
            |> seq_memoize
        | Spans.Map2 { a; b; f; _ } ->
            align_span_seq cache (seq_for_span_series cache a) (seq_for_span_series cache b)
            |> Seq.filter_map (fun (a, b) ->
                match (a, b) with
                | None, None -> None
                | _ ->
                    let period =
                      match (a, b) with
                      | Some c, _ | _, Some c -> c.period
                      | None, None -> assert false
                    in
                    Some
                      (make_span_cell cache ~owner:id ~series_label ~period ~split:Split.daily
                         (let open Formula in
                          let+ a = formula_of_cell_option a and+ b = formula_of_cell_option b in
                          f a b)))
            |> seq_memoize
        | Spans.MapN { deps; f; _ } ->
            let dep_seqs = List.map (fun (Spans.Pack dep) -> seq_for_span_series cache dep) deps in
            align_span_seqs cache dep_seqs
            |> Seq.map (fun row ->
                let period =
                  match List.find_map Fun.id row with Some c -> c.period | None -> assert false
                in
                make_span_cell cache ~owner:id ~series_label ~period ~split:Split.daily
                  (formula_of_cell_options f row))
            |> seq_memoize
        | Spans.Sum_family _ ->
            invalid_arg "Spans.sum_family cannot be replayed as an unbounded sequence"
        | Spans.Extend { a; b; _ } ->
            extend_span_seq cache (seq_for_span_series cache a) (seq_for_span_series cache b)
            |> seq_memoize
        | Spans.Clipped { after; until; base; _ } ->
            clipped_span_seq cache ~after ~until (seq_for_span_series cache base) |> seq_memoize
        | Spans.With_agg { base; _ } -> seq_for_span_series cache base |> seq_memoize
        | Spans.Unfold { id; init; cells; _ } ->
            make_unfold_producer ~init ~cells
              ~register_formula:(register_unfold_formula cache ~owner:id ~series_label)
        | Spans.Unfold_from { id; base; cells; _ } ->
            unfold_from_span_seq (seq_for_span_series cache base) ~cells
              ~register_formula:(register_unfold_formula cache ~owner:id ~series_label)
            |> seq_memoize
      in
      entry.sequence <- Some seq;
      seq

let materialize_sum_family cache id ~series_label _agg family period =
  match Family.series family period with
  | [] -> missing_if_nonempty period
  | members ->
      let formula =
        let rec combine = function
          | [] -> Formula.pure []
          | series :: rest ->
              let open Formula in
              let+ value = span_query series ~period and+ rest = combine rest in
              value :: rest
        in
        Formula.map
          (fun values ->
            match List.filter_map Fun.id values with
            | [] -> None
            | present -> Some (List.fold_left ( +. ) 0.0 present))
          (combine members)
      in
      [ Present (make_span_cell cache ~owner:id ~series_label ~period ~split:Split.daily formula) ]

let rec materialize_span : type tag.
    series_cache -> tag Spans.t -> period:Period.t -> span_segment list =
 fun cache series ~period ->
  match series with
  | Spans.Clipped { after; until; _ }
    when Date.(until <= after)
         || Date.(Period.end_ period <= after)
         || Date.(Period.start period >= until) ->
      missing_if_nonempty period
  | Spans.Sum_family { id; agg; family; _ } -> (
      let series_label = Spans.label series in
      let entry = span_entry cache id in
      match PeriodTable.find_opt entry.cells period with
      | Some cells -> cells
      | None ->
          let cells = materialize_sum_family cache id ~series_label agg family period in
          PeriodTable.replace entry.cells period cells;
          cells)
  | _ -> (
      let id = Spans.id series in
      let series_label = Spans.label series in
      let entry = span_entry cache id in
      match PeriodTable.find_opt entry.cells period with
      | Some cells -> cells
      | None ->
          let present_cells segments =
            List.filter_map (function Present cell -> Some cell | Missing _ -> None) segments
          in
          let materialized_seq dep =
            materialize_span cache dep ~period |> present_cells |> List.to_seq
          in
          let cells =
            match series with
            | Spans.Map { dep; f; _ } ->
                materialized_seq dep
                |> Seq.map (fun dep_cell ->
                    make_span_cell cache ~owner:id ~series_label ~period:dep_cell.period
                      ~split:dep_cell.split
                      (Formula.map (Option.map f) (Formula.span_cell_value dep_cell.id)))
                |> fun seq -> collect_cells cache seq period
            | Spans.Map2 { a; b; f; _ } ->
                align_span_seq cache (materialized_seq a) (materialized_seq b)
                |> Seq.filter_map (fun (a, b) ->
                    match (a, b) with
                    | None, None -> None
                    | _ ->
                        let period =
                          match (a, b) with
                          | Some c, _ | _, Some c -> c.period
                          | None, None -> assert false
                        in
                        Some
                          (make_span_cell cache ~owner:id ~series_label ~period ~split:Split.daily
                             (let open Formula in
                              let+ a = formula_of_cell_option a and+ b = formula_of_cell_option b in
                              f a b)))
                |> fun seq -> collect_cells cache seq period
            | Spans.MapN { deps; f; _ } ->
                let dep_seqs = List.map (fun (Spans.Pack dep) -> materialized_seq dep) deps in
                align_span_seqs cache dep_seqs
                |> Seq.filter_map (fun row ->
                    match List.find_map Fun.id row with
                    | None -> None
                    | Some cell ->
                        Some
                          (make_span_cell cache ~owner:id ~series_label ~period:cell.period
                             ~split:Split.daily (formula_of_cell_options f row)))
                |> fun seq -> collect_cells cache seq period
            | _ -> collect_cells cache (seq_for_span_series cache series) period
          in
          PeriodTable.replace entry.cells period cells;
          cells)

(* ----- Point materialization ----- *)

let point_entry cache id =
  match SeriesIdTable.find_opt cache.point id with
  | Some entry -> entry
  | None ->
      let entry = DateTable.create 16 in
      SeriesIdTable.add cache.point id entry;
      entry

let materialize_point : type tag. series_cache -> tag Points.t -> Date.t -> point_cell option =
 fun cache series date ->
  let id = Points.id series in
  let series_label = Points.label series in
  let entry = point_entry cache id in
  match DateTable.find_opt entry date with
  | Some cell -> cell
  | None ->
      let cell =
        match series with
        | Points.Const { period; value; _ } ->
            if Period.contains date period then
              Some (make_point_cell cache ~owner:id ~series_label ~date (Formula.pure (Some value)))
            else None
        | Points.List { values; _ } -> (
            match List.find_opt (fun (d, _) -> Date.equal d date) values with
            | Some (_, value) ->
                Some
                  (make_point_cell cache ~owner:id ~series_label ~date (Formula.pure (Some value)))
            | None -> None)
        | Points.Map { dep; f; _ } ->
            Some
              (make_point_cell cache ~owner:id ~series_label ~date
                 (Formula.map (Option.map f) (Formula.point_query dep ~date)))
        | Points.Map2 { a; b; f; _ } ->
            Some
              (make_point_cell cache ~owner:id ~series_label ~date
                 (let open Formula in
                  let+ a = point_query a ~date and+ b = point_query b ~date in
                  f a b))
        | Points.MapN { deps; f; _ } ->
            let rec combine = function
              | [] -> Formula.pure []
              | Points.Pack dep :: rest ->
                  let open Formula in
                  let+ value = point_query dep ~date and+ rest = combine rest in
                  value :: rest
            in
            Some
              (make_point_cell cache ~owner:id ~series_label ~date (Formula.map f (combine deps)))
        | Points.Accum { init; changes; _ } ->
            let period = Period.make Date.lower_bound date in
            Some
              (make_point_cell cache ~owner:id ~series_label ~date
                 (Formula.map
                    (fun delta -> Some (init +. Option.value ~default:0.0 delta))
                    (Formula.span_query changes ~period)))
      in
      DateTable.replace entry date cell;
      cell

(* ----- Solver ----- *)

exception Evaluation_did_not_converge of { iterations : int; tolerance : float; max_delta : float }

let iteration_tolerance = 1e-6
let max_iterations = 1000

type resolve_roots = Resolve_spans of span_segment list | Resolve_point of point_cell

let current_value cache key =
  match CellValueCache.find_opt cache.values key with
  | Some (Resolved { value; _ }) -> value
  | Some (Resolving state) -> state.current
  | None -> invalid_arg "unresolved cell value"

let current_span_value cache cell = current_value cache (span_key cell)
let current_point_value cache cell = current_value cache (point_key cell)

let value_delta previous value =
  match (previous, value) with
  | None, None -> 0.0
  | Some previous, Some value -> Float.abs (value -. previous)
  | None, Some _ | Some _, None -> Float.infinity

let rec prime_span_cell cache touched cell =
  let key = span_key cell in
  match CellValueCache.find_opt cache.values key with
  | Some _ -> ()
  | None ->
      CellValueCache.add cache.values key
        (Resolving { cell = Span_eval_cell cell; current = Some 0.0; step = 0 });
      touched := key :: !touched;
      prime_formula cache touched cell.formula

and prime_point_cell cache touched cell =
  let key = point_key cell in
  match CellValueCache.find_opt cache.values key with
  | Some _ -> ()
  | None ->
      CellValueCache.add cache.values key
        (Resolving { cell = Point_eval_cell cell; current = Some 0.0; step = 0 });
      touched := key :: !touched;
      prime_formula cache touched cell.formula

and prime_formula cache touched formula =
  Formula.queries formula |> List.iter (prime_formula_query cache touched)

and prime_formula_query cache touched = function
  | Formula.Span_query_item { series; period } ->
      materialize_span cache series ~period
      |> List.iter (function Present cell -> prime_span_cell cache touched cell | Missing _ -> ())
  | Formula.Point_query_item { series; date } -> (
      match materialize_point cache series date with
      | Some cell -> prime_point_cell cache touched cell
      | None -> ())
  | Formula.Span_cell_value_item id -> (
      match Hashtbl.find_opt cache.span_cells (span_id_int id) with
      | Some cell -> prime_span_cell cache touched cell
      | None -> invalid_arg "unknown span cell")
  | Formula.Point_cell_value_item id -> (
      match Hashtbl.find_opt cache.point_cells (point_id_int id) with
      | Some cell -> prime_point_cell cache touched cell
      | None -> invalid_arg "unknown point cell")

let rec eval_span_cell cache touched iteration cell =
  let key = span_key cell in
  match CellValueCache.find_opt cache.values key with
  | Some (Resolved { value; _ }) -> (value, 0.0)
  | Some (Resolving state) when state.step = iteration -> (state.current, 0.0)
  | Some (Resolving state) ->
      state.step <- iteration;
      let previous = state.current in
      let value, child_delta = eval_formula cache touched iteration cell.formula in
      let delta = value_delta previous value in
      state.current <- value;
      (value, max delta child_delta)
  | None ->
      prime_span_cell cache touched cell;
      eval_span_cell cache touched iteration cell

and eval_point_cell cache touched iteration cell =
  let key = point_key cell in
  match CellValueCache.find_opt cache.values key with
  | Some (Resolved { value; _ }) -> (value, 0.0)
  | Some (Resolving state) when state.step = iteration -> (state.current, 0.0)
  | Some (Resolving state) ->
      state.step <- iteration;
      let previous = state.current in
      let value, child_delta = eval_formula cache touched iteration cell.formula in
      let delta = value_delta previous value in
      state.current <- value;
      (value, max delta child_delta)
  | None ->
      prime_point_cell cache touched cell;
      eval_point_cell cache touched iteration cell

and eval_formula cache touched iteration formula =
  Formula.eval_with_delta
    ~query_span_values:
      {
        Formula.query_span_values =
          (fun series period ->
            materialize_span cache series ~period |> eval_span_segments cache touched iteration);
      }
    ~query_point_value:
      {
        Formula.query_point_value =
          (fun series date ->
            match materialize_point cache series date with
            | Some cell -> eval_point_cell cache touched iteration cell
            | None -> (None, 0.0));
      }
    ~query_span_cell_value:
      {
        Formula.query_span_cell_value =
          (fun id ->
            match Hashtbl.find_opt cache.span_cells (span_id_int id) with
            | Some cell -> eval_span_cell cache touched iteration cell
            | None -> invalid_arg "unknown span cell");
      }
    ~query_point_cell_value:
      {
        Formula.query_point_cell_value =
          (fun id ->
            match Hashtbl.find_opt cache.point_cells (point_id_int id) with
            | Some cell -> eval_point_cell cache touched iteration cell
            | None -> invalid_arg "unknown point cell");
      }
    formula

and eval_span_segments cache touched iteration segments =
  let rec loop acc delta = function
    | [] -> (List.rev acc, delta)
    | Missing _ :: rest -> loop (None :: acc) delta rest
    | Present cell :: rest ->
        let value, cell_delta = eval_span_cell cache touched iteration cell in
        let sample =
          match value with Some value -> Some { Agg.period = cell.period; value } | None -> None
        in
        loop (sample :: acc) (max delta cell_delta) rest
  in
  loop [] 0.0 segments

let prime_roots cache touched = function
  | Resolve_spans segments ->
      List.iter
        (function Present cell -> prime_span_cell cache touched cell | Missing _ -> ())
        segments
  | Resolve_point cell -> prime_point_cell cache touched cell

let eval_roots cache touched iteration = function
  | Resolve_spans segments ->
      let _, delta = eval_span_segments cache touched iteration segments in
      delta
  | Resolve_point cell ->
      let _, delta = eval_point_cell cache touched iteration cell in
      delta

let finalize_touched cache touched =
  List.iter
    (fun key ->
      match CellValueCache.find_opt cache.values key with
      | Some (Resolving state) ->
          CellValueCache.replace cache.values key
            (Resolved { cell = state.cell; value = state.current })
      | Some (Resolved _) | None -> ())
    touched

let clear_touched cache touched =
  List.iter
    (fun key ->
      match CellValueCache.find_opt cache.values key with
      | Some (Resolving _) -> CellValueCache.remove cache.values key
      | Some (Resolved _) | None -> ())
    touched

let solve_roots cache roots =
  let touched = ref [] in
  try
    prime_roots cache touched roots;
    let rec loop iteration last_delta =
      if iteration > max_iterations then
        raise
          (Evaluation_did_not_converge
             {
               iterations = max_iterations;
               tolerance = iteration_tolerance;
               max_delta = last_delta;
             })
      else
        let delta = eval_roots cache touched iteration roots in
        if delta <= iteration_tolerance then finalize_touched cache !touched
        else loop (iteration + 1) delta
    in
    loop 1 0.0
  with e ->
    clear_touched cache !touched;
    raise e

let resolve_span_segments cache segments =
  solve_roots cache (Resolve_spans segments);
  List.map
    (function
      | Missing _ -> None
      | Present cell -> (
          match current_span_value cache cell with
          | Some value -> Some { Agg.period = cell.period; value }
          | None -> None))
    segments

let resolve_point cache cell =
  solve_roots cache (Resolve_point cell);
  current_point_value cache cell

let query_span_samples cache series ~period =
  materialize_span cache series ~period |> resolve_span_segments cache

let query_span cache series ~period =
  query_span_samples cache series ~period |> Agg.reduce (Spans.agg series)

let query_point cache series ~date =
  match materialize_point cache series date with
  | Some cell -> resolve_point cache cell
  | None -> None

(* ----- Query-specific traces ----- *)

let trace_cell_of_span (cell : span_cell) =
  Trace.Span_cell { id = cell.id; period = cell.period; series_label = cell.series_label }

let trace_cell_of_point (cell : point_cell) =
  Trace.Point_cell { id = cell.id; date = cell.date; series_label = cell.series_label }

let trace_from_roots cache roots =
  let edges = ref [] in
  let rec cell_mem cell = function [] -> false | x :: xs -> x = cell || cell_mem cell xs
  and walk_span path cell =
    walk_formula (trace_cell_of_span cell :: path) (trace_cell_of_span cell) cell.formula
  and walk_point path cell =
    walk_formula (trace_cell_of_point cell :: path) (trace_cell_of_point cell) cell.formula
  and add_edge path from query to_cell recurse =
    let is_back_edge = cell_mem to_cell path in
    edges := { Trace.from; to_ = to_cell; query; is_back_edge } :: !edges;
    if not is_back_edge then recurse (to_cell :: path)
  and walk_formula path from formula =
    Formula.queries formula
    |> List.iter (function
      | Formula.Span_query_item { series; period } ->
          materialize_span cache series ~period
          |> List.iter (function
            | Missing _ -> ()
            | Present dep ->
                add_edge path from
                  (Trace.Span_query { period; label = Spans.label series })
                  (trace_cell_of_span dep)
                  (fun path -> walk_span path dep))
      | Formula.Point_query_item { series; date } -> (
          match materialize_point cache series date with
          | None -> ()
          | Some dep ->
              add_edge path from
                (Trace.Point_query { date; label = Points.label series })
                (trace_cell_of_point dep)
                (fun path -> walk_point path dep))
      | Formula.Span_cell_value_item id -> (
          match Hashtbl.find_opt cache.span_cells (span_id_int id) with
          | None -> ()
          | Some dep ->
              add_edge path from Trace.Span_cell_value (trace_cell_of_span dep) (fun path ->
                  walk_span path dep))
      | Formula.Point_cell_value_item id -> (
          match Hashtbl.find_opt cache.point_cells (point_id_int id) with
          | None -> ()
          | Some dep ->
              add_edge path from Trace.Point_cell_value (trace_cell_of_point dep) (fun path ->
                  walk_point path dep)))
  in
  List.iter
    (function
      | Trace.Span_cell { id; _ } -> (
          match Hashtbl.find_opt cache.span_cells (span_id_int id) with
          | Some cell -> walk_span [] cell
          | None -> ())
      | Trace.Point_cell { id; _ } -> (
          match Hashtbl.find_opt cache.point_cells (point_id_int id) with
          | Some cell -> walk_point [] cell
          | None -> ()))
    roots;
  { Trace.roots; edges = List.rev !edges }

let trace_span cache series ~period =
  let roots =
    materialize_span cache series ~period
    |> List.filter_map (function
      | Present cell -> Some (trace_cell_of_span cell)
      | Missing _ -> None)
  in
  trace_from_roots cache roots

let trace_point cache series ~date =
  let roots =
    match materialize_point cache series date with
    | None -> []
    | Some cell -> [ trace_cell_of_point cell ]
  in
  trace_from_roots cache roots
