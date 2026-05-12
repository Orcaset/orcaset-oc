(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

open Cell_type

(* ----- Series ids ----- *)
type series_id = Id of int [@@unboxed]

let next_id = Atomic.make 0
let new_id () : series_id = Id (Atomic.fetch_and_add next_id 1)

(* ----- Span cells ----- *)
type split = Split.t

let cell_split_strategy (split : split) : split_strategy =
 fun span date ->
  let period = span_period span in
  let left_period = Period.make (Period.start period) date in
  let right_period = Period.make date (Period.end_ period) in
  let left_part, right_part = split ~period ~date in
  ( Cell_type.split_part ~period:left_period ~value:(Split.value left_part),
    Cell_type.split_part ~period:right_period ~value:(Split.value right_part) )

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
        | present -> Some (List.fold_left (fun total sample -> total +. sample.value) 0.0 present))

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

(* ----- Series constructors ----- *)
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
    | Extend : { id : series_id; agg : Agg.t; a : 'tag t; b : 'tag t } -> 'tag t
    | Clipped : { id : series_id; after : Date.t; until : Date.t; base : 'tag t } -> 'tag t
    | With_agg : { id : series_id; agg : Agg.t; base : 'tag t } -> 'tag t
    | Unfold : {
        id : series_id;
        label : string option;
        agg : Agg.t;
        deps : unit -> 'readers Deps.t;
        init : 'state;
        cells : 'readers -> 'state -> (unfold_cell * 'state) option;
      }
        -> 'tag t
    | Unfold_from : {
        id : series_id;
        label : string option;
        agg : Agg.t;
        base : 'tag t;
        deps : unit -> 'readers Deps.t;
        cells : 'readers -> Period.t -> (unfold_cell * Period.t) option;
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
    deps:(unit -> 'readers Deps.t) ->
    init:'state ->
    cells:('readers -> 'state -> (unfold_cell * 'state) option) ->
    unit ->
    'tag t

  val unfold_from :
    ?label:string ->
    agg:Agg.t ->
    deps:(unit -> 'readers Deps.t) ->
    cells:('readers -> Period.t -> (unfold_cell * Period.t) option) ->
    'tag t ->
    'tag t

  val unfold_rec :
    ?label:string ->
    agg:Agg.t ->
    deps:('tag t -> 'readers Deps.t) ->
    init:'state ->
    cells:('readers -> 'state -> (unfold_cell * 'state) option) ->
    unit ->
    'tag t

  val cell : period:Period.t -> split:split -> float option Formula.t -> unfold_cell
  val id : 'tag t -> series_id
  val label : 'tag t -> string option
  val agg : 'tag t -> Agg.t
  val with_agg : agg:Agg.t -> 'tag t -> 'tag t
  val unpack_unfold_cell : unfold_cell -> Period.t * split * float option Formula.t
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
    | Extend : { id : series_id; agg : Agg.t; a : 'tag t; b : 'tag t } -> 'tag t
    | Clipped : { id : series_id; after : Date.t; until : Date.t; base : 'tag t } -> 'tag t
    | With_agg : { id : series_id; agg : Agg.t; base : 'tag t } -> 'tag t
    | Unfold : {
        id : series_id;
        label : string option;
        agg : Agg.t;
        deps : unit -> 'readers Deps.t;
        init : 'state;
        cells : 'readers -> 'state -> (unfold_cell * 'state) option;
      }
        -> 'tag t
    | Unfold_from : {
        id : series_id;
        label : string option;
        agg : Agg.t;
        base : 'tag t;
        deps : unit -> 'readers Deps.t;
        cells : 'readers -> Period.t -> (unfold_cell * Period.t) option;
      }
        -> 'tag t

  and packed = Pack : 'tag t -> packed

  let pack series = Pack series
  let cell ~period ~split formula = Cell { period; split; formula }
  let unpack_unfold_cell (Cell { period; split; formula }) = (period, split, formula)

  let const ?label ~split ~agg ~period value =
    Const { id = new_id (); label; split; agg; period; value }

  let clipped ~after ~until base =
    if Date.(until < after) then invalid_arg "Spans.clipped: until before after";
    Clipped { id = new_id (); after; until; base }

  let after date base = clipped ~after:date ~until:Date.upper_bound base
  let until date base = clipped ~after:Date.lower_bound ~until:date base

  let unfold ?label ~agg ~deps ~init ~cells () =
    Unfold { id = new_id (); label; agg; deps; init; cells }

  let unfold_from ?label ~agg ~deps ~cells base =
    Unfold_from { id = new_id (); label; agg; base; deps; cells }

  let unfold_rec ?label ~agg ~deps ~init ~cells () =
    let rec self =
      Unfold { id = new_id (); label; agg; deps = (fun () -> deps self); init; cells }
    in
    self

  let of_list ?label ~split ~agg cells =
    unfold ?label ~agg
      ~deps:(fun () -> Deps.none)
      ~init:cells
      ~cells:(fun () -> function
        | [] -> None
        | (period, value) :: rest -> Some (cell ~period ~split (Formula.pure (Some value)), rest))
      ()

  let id = function
    | Const { id; _ } -> id
    | Map { id; _ } -> id
    | Map2 { id; _ } -> id
    | MapN { id; _ } -> id
    | Extend { id; _ } -> id
    | Clipped { id; _ } -> id
    | With_agg { id; _ } -> id
    | Unfold { id; _ } -> id
    | Unfold_from { id; _ } -> id

  let rec label = function
    | Const { label; _ } -> label
    | Map { label; _ } -> label
    | Map2 { label; _ } -> label
    | MapN { label; _ } -> label
    | Extend { a; _ } -> label a
    | Clipped { base; _ } -> label base
    | With_agg { base; _ } -> label base
    | Unfold { label; _ } -> label
    | Unfold_from { label; _ } -> label

  let rec agg = function
    | Const { agg; _ } -> agg
    | Map { agg; _ } -> agg
    | Map2 { agg; _ } -> agg
    | MapN { agg; _ } -> agg
    | Extend { agg; _ } -> agg
    | Clipped { base; _ } -> agg base
    | With_agg { agg; _ } -> agg
    | Unfold { agg; _ } -> agg
    | Unfold_from { agg; _ } -> agg

  let with_agg ~agg base = With_agg { id = new_id (); agg; base }
  let map ?label f dep = Map { id = new_id (); label; agg = agg dep; dep; f }
  let extend ~agg a b = Extend { id = new_id (); agg; a; b }
  let neg ?label dep = map ?label (fun value -> -.value) dep
  let scale ?label factor dep = map ?label (fun value -> factor *. value) dep
  let map2 ?label ~agg a b f = Map2 { id = new_id (); label; agg; a; b; f }
  let mapn ?label ~agg deps f = MapN { id = new_id (); label; agg; deps; f }

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

  let id = function
    | Const { id; _ } -> id
    | List { id; _ } -> id
    | Map { id; _ } -> id
    | Map2 { id; _ } -> id
    | MapN { id; _ } -> id
    | Accum { id; _ } -> id

  let label = function
    | Const { label; _ } -> label
    | List { label; _ } -> label
    | Map { label; _ } -> label
    | Map2 { label; _ } -> label
    | MapN { label; _ } -> label
    | Accum { label; _ } -> label

  let map ?label f dep = Map { id = new_id (); label; dep; f }
  let neg ?label dep = map ?label (fun value -> -.value) dep
  let scale ?label factor dep = map ?label (fun value -> factor *. value) dep
  let map2 ?label a b f = Map2 { id = new_id (); label; a; b; f }
  let mapn ?label deps f = MapN { id = new_id (); label; deps; f }

  let effective_label override series =
    match override with Some _ -> override | None -> label series

  let sum ?label = function
    | [] -> invalid_arg "Points.sum: empty list"
    | [ Pack x ] -> Map { id = new_id (); label = effective_label label x; dep = x; f = Fun.id }
    | deps ->
        mapn ?label deps (fun values ->
            match List.filter_map Fun.id values with
            | [] -> None
            | present -> Some (List.fold_left ( +. ) 0.0 present))

  let mul ?label = function
    | [] -> invalid_arg "Points.mul: empty list"
    | [ Pack x ] -> Map { id = new_id (); label = effective_label label x; dep = x; f = Fun.id }
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

and Deps : sig
  type span_reader = period:Period.t -> float option Formula.t
  type point_reader = date:Date.t -> float option Formula.t
  type _ t

  val none : unit t
  val span_dep : 'tag Spans.t -> span_reader t
  val point_dep : 'tag Points.t -> point_reader t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

  type packed_dep =
    | Span_item : 'tag Spans.t -> packed_dep
    | Point_item : 'tag Points.t -> packed_dep

  val dependencies : 'a t -> packed_dep list
  val run : 'a t -> 'a
end = struct
  type span_reader = period:Period.t -> float option Formula.t
  type point_reader = date:Date.t -> float option Formula.t

  type _ dep =
    | Span_dep : 'tag Spans.t -> span_reader dep
    | Point_dep : 'tag Points.t -> point_reader dep

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

  type packed_dep =
    | Span_item : 'tag Spans.t -> packed_dep
    | Point_item : 'tag Points.t -> packed_dep

  let rec dependencies : type a. a t -> packed_dep list = function
    | Pure _ -> []
    | Ap (Span_dep s, rest) -> Span_item s :: dependencies rest
    | Ap (Point_dep s, rest) -> Point_item s :: dependencies rest

  let run (type a) (d : a t) : a =
    let rec go : type a. a t -> a = function
      | Pure x -> x
      | Ap (Span_dep s, rest) ->
          let reader ~period = Formula.span_query s ~period in
          go rest reader
      | Ap (Point_dep s, rest) ->
          let reader ~date = Formula.point_query s ~date in
          go rest reader
    in
    go d
end

and Formula : sig
  type 'a t

  type packed_query =
    | Span_query_item : { series : 'tag Spans.t; period : Period.t } -> packed_query
    | Point_query_item : { series : 'tag Points.t; date : Date.t } -> packed_query

  val pure : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val queries : 'a t -> packed_query list
  val span_query : 'tag Spans.t -> period:Period.t -> float option t
  val point_query : 'tag Points.t -> date:Date.t -> float option t

  type span_query_values = {
    query_span_values : 'tag. 'tag Spans.t -> Period.t -> Agg.sample option list * float;
  }

  type point_query_value = {
    query_point_value : 'tag. 'tag Points.t -> Date.t -> float option * float;
  }

  val eval_with_delta :
    query_span_values:span_query_values -> query_point_value:point_query_value -> 'a t -> 'a * float
end = struct
  type _ query =
    | Span_query : { series : 'tag Spans.t; period : Period.t } -> float option query
    | Point_query : { series : 'tag Points.t; date : Date.t } -> float option query

  type 'a t =
    | Pure : 'a -> 'a t
    | Map : ('a -> 'b) * 'a t -> 'b t
    | Map2 : ('a -> 'b -> 'c) * 'a t * 'b t -> 'c t
    | Query : 'a query -> 'a t

  type packed_query =
    | Span_query_item : { series : 'tag Spans.t; period : Period.t } -> packed_query
    | Point_query_item : { series : 'tag Points.t; date : Date.t } -> packed_query

  type span_query_values = {
    query_span_values : 'tag. 'tag Spans.t -> Period.t -> Agg.sample option list * float;
  }

  type point_query_value = {
    query_point_value : 'tag. 'tag Points.t -> Date.t -> float option * float;
  }

  let pure x = Pure x
  let map f x = Map (f, x)
  let map2 f a b = Map2 (f, a, b)
  let ( let+ ) x f = map f x
  let ( and+ ) a b = map2 (fun x y -> (x, y)) a b
  let span_query series ~period = Query (Span_query { series; period })
  let point_query series ~date = Query (Point_query { series; date })

  let rec queries : type a. a t -> packed_query list = function
    | Pure _ -> []
    | Map (_, x) -> queries x
    | Map2 (_, a, b) -> queries a @ queries b
    | Query (Span_query { series; period }) -> [ Span_query_item { series; period } ]
    | Query (Point_query { series; date }) -> [ Point_query_item { series; date } ]

  let eval_with_delta (type a) ~(query_span_values : span_query_values)
      ~(query_point_value : point_query_value) (formula : a t) : a * float =
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
      | Query (Point_query { series; date }) ->
          let value, delta = query_point_value.query_point_value series date in
          (value, delta)
    in
    go formula
end

(* ----- Existentially-packed series ----- *)

type span_kind
type point_kind

type (_, _) series =
  | Point_series : 'tag Points.t -> (point_kind, 'tag) series
  | Span_series : 'tag Spans.t -> (span_kind, 'tag) series

let label : type kind tag. (kind, tag) series -> string option = function
  | Point_series series -> Points.label series
  | Span_series series -> Spans.label series

type packed_series = Series : ('kind, 'tag) series -> packed_series
type dependency = { series : packed_series; dependencies : dependency list; is_back_edge : bool }

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
  mutable current : float option;
  mutable last : float option;
  mutable step : int;
}

type eval_state =
  | Resolving of resolving_cell
  | Resolved of { cell : eval_cell; value : float option }

type series_cache = {
  point : (series_id, cached_point PointCellCache.t) Hashtbl.t;
  span : (series_id, span_cache_entry) Hashtbl.t;
  accum : (series_id, accum_cache_entry) Hashtbl.t;
  span_formulas : (int, float option Formula.t) Hashtbl.t;
  values : eval_state CellValueCache.t;
}

let make_cache () : series_cache =
  {
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
  let period_start, period_end = Period.to_tuple period in
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
  let add_gap start end_ acc = if Date.(start < end_) then None :: acc else acc in
  let rec with_gaps cursor acc = function
    | [] -> List.rev (add_gap cursor period_end acc)
    | span :: rest ->
        let span_start, span_end = Period.to_tuple (span_period span) in
        let acc = add_gap cursor span_start acc in
        let cursor = if Date.(cursor < span_end) then span_end else cursor in
        with_gaps cursor (Some span :: acc) rest
  in
  let overlapping = if Date.equal period_start period_end then [] else go seq [] in
  if Date.equal period_start period_end then []
  else match overlapping with [] -> [ None ] | _ -> with_gaps period_start [] overlapping

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

(* Feed an unfold [cells] stream: each yielded [unfold_cell] becomes a memoized span cell. *)
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

let register_unfold_formula cache ~period ~split formula =
  let span =
    (* Formula-backed spans store a dummy scalar here; evaluation is redirected through
       [cache.span_formulas] before this value is ever consulted. *)
    f_value period Float.nan (cell_split_strategy split)
  in
  Hashtbl.replace cache.span_formulas (span_id span) formula;
  span

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
    | Seq.Cons (span, rest) ->
        let period = span_period span in
        Seq.Cons (span, replay_base (Some period) rest)
  in
  replay_base None base

let extend_span_seq a b =
  let rec continue_b start b () =
    match b () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (span, rest) ->
        let span_start, span_end = Period.to_tuple (span_period span) in
        if Date.(span_end <= start) then continue_b start rest ()
        else if Date.(span_start < start) then
          match split_span start span with
          | _, Some right -> Seq.Cons (right, rest)
          | _, None -> continue_b start rest ()
        else Seq.Cons (span, rest)
  in
  let rec continue_a last_end a () =
    match a () with
    | Seq.Nil -> ( match last_end with None -> b () | Some end_ -> continue_b end_ b ())
    | Seq.Cons (span, rest) ->
        Seq.Cons (span, continue_a (Some (Period.end_ (span_period span))) rest)
  in
  continue_a None a

let clipped_span_seq ~after ~until seq =
  if Date.equal after until then Seq.empty
  else
    let bounds = Period.make after until in
    let rec go seq () =
      match seq () with
      | Seq.Nil -> Seq.Nil
      | Seq.Cons (span, rest) -> (
          let span_start, span_end = Period.to_tuple (span_period span) in
          if Date.(span_end <= after) then go rest ()
          else if Date.(span_start >= until) then Seq.Nil
          else
            match clip_span bounds span with
            | Some clipped -> Seq.Cons (clipped, go rest)
            | None -> go rest ())
    in
    go seq

let rec seq_for_span_series : type tag. series_cache -> tag Spans.t -> span Seq.t =
 fun cache series ->
  match series with
  | Const { period; value; split; _ } ->
      Seq.return (f_value period value (cell_split_strategy split))
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
  | MapN { deps; f; _ } ->
      let dep_seqs = List.map (fun (Spans.Pack dep) -> seq_for_span_series cache dep) deps in
      let aligned = align_span_seqs dep_seqs in
      let seq =
        Seq.map
          (fun row ->
            let period =
              match List.find_map Fun.id row with Some sp -> span_period sp | None -> assert false
            in
            f_mapn period row f)
          aligned
      in
      Seq.memoize seq
  | Extend { a; b; _ } ->
      extend_span_seq (seq_for_span_series cache a) (seq_for_span_series cache b) |> Seq.memoize
  | Clipped { after; until; base; _ } ->
      clipped_span_seq ~after ~until (seq_for_span_series cache base) |> Seq.memoize
  | With_agg { base; _ } -> seq_for_span_series cache base |> Seq.memoize
  | Unfold { deps; init; cells; _ } ->
      let readers = Deps.run (deps ()) in
      make_unfold_producer ~init ~cells:(cells readers)
        ~register_formula:(register_unfold_formula cache)
  | Unfold_from { base; deps; cells; _ } ->
      let readers = Deps.run (deps ()) in
      unfold_from_span_seq (seq_for_span_series cache base) ~cells:(cells readers)
        ~register_formula:(register_unfold_formula cache)
      |> Seq.memoize

and query_span_series : type tag. series_cache -> tag Spans.t -> Period.t -> span option list =
 fun cache series period ->
  match series with
  | Clipped { after; until; _ }
    when Date.(until <= after)
         || Date.(Period.end_ period <= after)
         || Date.(Period.start period >= until) ->
      if Period.days period = 0 then [] else [ None ]
  | _ -> (
      let series_id = Spans.id series in
      let series_entry_opt = Hashtbl.find_opt cache.span series_id in
      let { cells; sequence } =
        match series_entry_opt with
        | Some entry -> entry
        | None ->
            let c =
              { cells = SpanCellCache.create 16; sequence = seq_for_span_series cache series }
            in
            Hashtbl.add cache.span series_id c;
            c
      in
      match SpanCellCache.find_opt cells period with
      | Some values -> values
      | None ->
          let spans = collect_spans sequence period in
          SpanCellCache.replace cells period spans;
          spans)

and get_accum_entry : type tag. series_cache -> series_id -> tag Spans.t -> accum_cache_entry =
 fun cache point_series_id changes ->
  match Hashtbl.find_opt cache.accum point_series_id with
  | Some entry -> entry
  | None ->
      let entry = { checkpoints = DateMap.empty; sequence = seq_for_span_series cache changes } in
      Hashtbl.add cache.accum point_series_id entry;
      entry

and query_accum : type tag. series_cache -> series_id -> float -> tag Spans.t -> Date.t -> point =
 fun cache point_series_id init changes date ->
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

and query_point_series : type tag. series_cache -> tag Points.t -> Date.t -> point option =
 fun cache series date ->
  let series_id = Points.id series in
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
            | Some (_, value) -> Some (p_const date value)
            | None -> None)
        | Map { dep; f; _ } -> (
            match query_point_series cache dep date with
            | Some pt -> Some (p_map pt f)
            | None -> None)
        | Map2 { a; b; f; _ } ->
            let oa, ob = (query_point_series cache a date, query_point_series cache b date) in
            Some (p_derived date [ oa; ob ] (function [ va; vb ] -> f va vb | _ -> assert false))
        | MapN { deps; f; _ } ->
            let child_opts =
              List.map (fun (Points.Pack dep) -> query_point_series cache dep date) deps
            in
            Some (p_derived date child_opts f)
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

let current_span_option_value cache = function
  | None -> None
  | Some span -> current_span_value cache span

let rec current_span_option_samples cache = function
  | [] -> []
  | None :: rest -> None :: current_span_option_samples cache rest
  | Some span :: rest -> (
      match current_span_value cache span with
      | None -> None :: current_span_option_samples cache rest
      | Some value ->
          Some { Agg.period = span_period span; value } :: current_span_option_samples cache rest)

let rec current_point_option_values cache = function
  | [] -> []
  | None :: rest -> None :: current_point_option_values cache rest
  | Some point :: rest -> current_point_value cache point :: current_point_option_values cache rest

let rec sum_current_span_options cache = function
  | [] -> 0.0
  | None :: rest -> sum_current_span_options cache rest
  | Some span :: rest ->
      Option.value ~default:0.0 (current_span_value cache span)
      +. sum_current_span_options cache rest

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
      let state = { cell; current = Some 0.0; last = Some 0.0; step = 0 } in
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
      | None -> Some (Some value)
      | Some formula ->
          if prime_formula cache touched formula then Some (resolved_formula_value cache formula)
          else None)
  | Slice { dep; value; _ } ->
      if prime_span cache touched dep then Some (Option.map value (current_span_value cache dep))
      else None
  | Map { dep; f; _ } ->
      if prime_span cache touched dep then Some (Option.map f (current_span_value cache dep))
      else None
  | Map2 { a; b; f; _ } ->
      let a_resolved = prime_span_option cache touched a in
      let b_resolved = prime_span_option cache touched b in
      if a_resolved && b_resolved then
        Some (f (current_span_option_value cache a) (current_span_option_value cache b))
      else None
  | MapN { deps; f; _ } ->
      if prime_span_options cache touched deps then
        Some (f (List.map (current_span_option_value cache) deps))
      else None

and prime_point cache touched point =
  let key = point_key point in
  match CellValueCache.find_opt cache.values key with
  | Some (Resolved _) -> true
  | Some (Resolving _) -> false
  | None -> (
      let cell = Point_eval_cell point in
      let state = { cell; current = Some 0.0; last = Some 0.0; step = 0 } in
      CellValueCache.add cache.values key (Resolving state);
      touched := key :: !touched;
      match prime_point_value cache touched point with
      | Some value ->
          CellValueCache.replace cache.values key (Resolved { cell; value });
          true
      | None -> false)

and prime_point_value cache touched = function
  | Const { value; _ } -> Some (Some value)
  | Map { dep; f; _ } ->
      if prime_point cache touched dep then Some (Option.map f (current_point_value cache dep))
      else None
  | Derived { deps; f; _ } ->
      if prime_point_options cache touched deps then
        Some (f (current_point_option_values cache deps))
      else None
  | Accum { init; base; delta; _ } ->
      let base_resolved = prime_point_option cache touched base in
      let delta_resolved = prime_span_options cache touched delta in
      if base_resolved && delta_resolved then
        let start =
          match base with
          | Some point -> Option.value ~default:init (current_point_value cache point)
          | None -> init
        in
        Some (Some (start +. sum_current_span_options cache delta))
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
       ~query_span_values:
         {
           Formula.query_span_values =
             (fun series period ->
               let samples =
                 query_span_series cache series period |> current_span_option_samples cache
               in
               (samples, 0.0));
         }
       ~query_point_value:
         {
           Formula.query_point_value =
             (fun series date ->
               let value =
                 match query_point_series cache series date with
                 | Some point -> current_point_value cache point
                 | None -> None
               in
               (value, 0.0));
         }
       formula)

let value_delta previous value =
  match (previous, value) with
  | None, None -> 0.0
  | Some previous, Some value -> Float.abs (value -. previous)
  | None, Some _ | Some _, None -> Float.infinity

let rec eval_span cache touched iteration span =
  let key = span_key span in
  match CellValueCache.find_opt cache.values key with
  | Some (Resolved { value; _ }) -> (value, 0.0)
  | Some (Resolving state) when state.step = iteration -> (state.current, 0.0)
  | Some (Resolving state) ->
      state.step <- iteration;
      let previous = state.current in
      let value, child_delta = eval_span_value cache touched iteration span in
      let delta = value_delta previous value in
      state.last <- previous;
      state.current <- value;
      (value, max delta child_delta)
  | None ->
      ignore (prime_span cache touched span);
      eval_span cache touched iteration span

and eval_span_value cache touched iteration = function
  | Value { id; value; _ } -> (
      match Hashtbl.find_opt cache.span_formulas id with
      | None -> (Some value, 0.0)
      | Some formula -> eval_formula cache touched iteration formula)
  | Slice { dep; value; _ } ->
      let dep_value, delta = eval_span cache touched iteration dep in
      (Option.map value dep_value, delta)
  | Map { dep; f; _ } ->
      let dep_value, delta = eval_span cache touched iteration dep in
      (Option.map f dep_value, delta)
  | Map2 { a; b; f; _ } ->
      let a_value, a_delta = eval_span_option cache touched iteration a in
      let b_value, b_delta = eval_span_option cache touched iteration b in
      (f a_value b_value, max a_delta b_delta)
  | MapN { deps; f; _ } ->
      let values, delta = eval_span_options cache touched iteration deps in
      (f values, delta)

and eval_point cache touched iteration point =
  let key = point_key point in
  match CellValueCache.find_opt cache.values key with
  | Some (Resolved { value; _ }) -> (value, 0.0)
  | Some (Resolving state) when state.step = iteration -> (state.current, 0.0)
  | Some (Resolving state) ->
      state.step <- iteration;
      let previous = state.current in
      let value, child_delta = eval_point_value cache touched iteration point in
      let delta = value_delta previous value in
      state.last <- previous;
      state.current <- value;
      (value, max delta child_delta)
  | None ->
      ignore (prime_point cache touched point);
      eval_point cache touched iteration point

and eval_point_value cache touched iteration = function
  | Const { value; _ } -> (Some value, 0.0)
  | Map { dep; f; _ } ->
      let dep_value, delta = eval_point cache touched iteration dep in
      (Option.map f dep_value, delta)
  | Derived { deps; f; _ } ->
      let values, delta = eval_point_options cache touched iteration deps in
      (f values, delta)
  | Accum { init; base; delta; _ } ->
      let base_value, base_delta =
        match base with
        | Some point -> eval_point cache touched iteration point
        | None -> (Some init, 0.0)
      in
      let delta_value, delta_delta = eval_span_option_sum cache touched iteration delta in
      (Some (Option.value ~default:init base_value +. delta_value), max base_delta delta_delta)

and eval_formula cache touched iteration formula =
  Formula.eval_with_delta
    ~query_span_values:
      {
        Formula.query_span_values =
          (fun series period -> eval_span_query cache touched iteration series period);
      }
    ~query_point_value:
      {
        Formula.query_point_value =
          (fun series date -> eval_point_query cache touched iteration series date);
      }
    formula

and eval_span_query : type tag.
    series_cache ->
    cell_key list ref ->
    int ->
    tag Spans.t ->
    Period.t ->
    Agg.sample option list * float =
 fun cache touched iteration series period ->
  query_span_series cache series period |> eval_span_option_samples cache touched iteration

and eval_point_query : type tag.
    series_cache -> cell_key list ref -> int -> tag Points.t -> Date.t -> float option * float =
 fun cache touched iteration series date ->
  match query_point_series cache series date with
  | Some point ->
      let value, delta = eval_point cache touched iteration point in
      (value, delta)
  | None -> (None, 0.0)

and eval_span_option cache touched iteration = function
  | Some span -> eval_span cache touched iteration span
  | None -> (None, 0.0)

and eval_span_options cache touched iteration = function
  | [] -> ([], 0.0)
  | cell :: rest ->
      let value, delta = eval_span_option cache touched iteration cell in
      let values, rest_delta = eval_span_options cache touched iteration rest in
      (value :: values, max delta rest_delta)

and eval_span_option_samples cache touched iteration = function
  | [] -> ([], 0.0)
  | cell :: rest ->
      let value, delta = eval_span_option cache touched iteration cell in
      let sample =
        match (cell, value) with
        | Some span, Some value -> Some { Agg.period = span_period span; value }
        | Some _, None | None, _ -> None
      in
      let samples, rest_delta = eval_span_option_samples cache touched iteration rest in
      (sample :: samples, max delta rest_delta)

and eval_point_option cache touched iteration = function
  | Some point -> eval_point cache touched iteration point
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

let resolve_span_option_samples cache spans =
  resolve_roots cache (Resolve_spans spans);
  current_span_option_samples cache spans

let resolve_point cache point =
  resolve_roots cache (Resolve_point point);
  current_point_value cache point

(* ----- Public float-based query API ----- *)

let query_span_samples cache series ~period =
  let spans = query_span_series cache series period in
  resolve_span_option_samples cache spans

let query_span cache series ~period =
  query_span_samples cache series ~period |> Agg.reduce (Spans.agg series)

let query_point cache series ~date =
  match query_point_series cache series date with
  | Some point -> resolve_point cache point
  | None -> None

(* ----- Series dependencies ----- *)

module Series_id_set = Set.Make (struct
  type t = series_id

  let compare (Id a) (Id b) = Int.compare a b
end)

let series_id : type kind tag. (kind, tag) series -> series_id = function
  | Point_series series -> Points.id series
  | Span_series series -> Spans.id series

let series_dependencies : type kind tag. (kind, tag) series -> packed_series list = function
  | Point_series series -> (
      match series with
      | Const _ -> []
      | List _ -> []
      | Map { dep; _ } -> [ Series (Point_series dep) ]
      | Map2 { a; b; _ } -> [ Series (Point_series a); Series (Point_series b) ]
      | MapN { deps; _ } -> List.map (fun (Points.Pack dep) -> Series (Point_series dep)) deps
      | Accum { changes; _ } -> [ Series (Span_series changes) ])
  | Span_series series -> (
      match series with
      | Const _ -> []
      | Map { dep; _ } -> [ Series (Span_series dep) ]
      | Map2 { a; b; _ } -> [ Series (Span_series a); Series (Span_series b) ]
      | MapN { deps; _ } -> List.map (fun (Spans.Pack dep) -> Series (Span_series dep)) deps
      | Extend { a; b; _ } -> [ Series (Span_series a); Series (Span_series b) ]
      | Clipped { base; _ } -> [ Series (Span_series base) ]
      | With_agg { base; _ } -> [ Series (Span_series base) ]
      | Unfold_from { base; deps; _ } ->
          Series (Span_series base)
          :: (Deps.dependencies (deps ())
             |> List.map (function
               | Deps.Span_item s -> Series (Span_series s)
               | Deps.Point_item s -> Series (Point_series s)))
      | Unfold { deps; _ } ->
          Deps.dependencies (deps ())
          |> List.map (function
            | Deps.Span_item s -> Series (Span_series s)
            | Deps.Point_item s -> Series (Point_series s)))

let rec build_dependencies active_path (Series series) =
  let add_dependency dependency =
    let (Series dep_series) = dependency in
    let dep_id = series_id dep_series in
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

let dependencies : type kind tag. (kind, tag) series -> dependency list =
 fun series -> build_dependencies (Series_id_set.singleton (series_id series)) (Series series)
