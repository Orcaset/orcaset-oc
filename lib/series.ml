(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Scoped series functor that ties the knot between period and point series evaluation.

    Parallels {!Eval} at the cell level: creates the shared cache and provides the callbacks that
    route evaluation across the period/point boundary. Each call to {!Make} produces a fresh module
    with abstract types, preventing accidental cross-scope mixing at compile time. *)

type 'c amount = Amount of float [@@unboxed]

exception Duplicate_label = Series_types.Duplicate_label

module type S = sig
  type 'c period_t
  type 'c point_t

  type 'c q_result =
    | QRPeriod of { label : string; period : Period.t; cells : 'c Period_cell.t list }
    | QRPoint of { label : string; cell : 'c Point_cell.t option }

  type 'c eval_result =
    | Period of { label : string; period : Period.t; value : 'c amount }
    | Point of { label : string; point : (Date.t * 'c amount) option }

  module Period : sig
    type 'c t = 'c period_t
    type reduce = Series_types.reduce
    type 'c dep
    type 'c point_dep
    type 'c cell
    type ('a, 'c) deps

    val no_deps : (unit, 'c) deps
    val dep_period : 'c t Lazy.t -> ('c dep, 'c) deps
    val dep_point : 'c point_t Lazy.t -> ('c point_dep, 'c) deps
    val both : ('a, 'c) deps -> ('b, 'c) deps -> ('a * 'b, 'c) deps
    val ( let+ ) : ('a, 'c) deps -> ('a -> 'b) -> ('b, 'c) deps
    val ( and+ ) : ('a, 'c) deps -> ('b, 'c) deps -> ('a * 'b, 'c) deps

    module Query : sig
      type ('a, 'c) t

      val pure : 'a -> ('a, 'c) t
      val self : period:Period.t -> reduce:reduce -> (float, 'c) t
      val period : 'c dep -> period:Period.t -> reduce:reduce -> (float, 'c) t
      val point : 'c point_dep -> date:Date.t -> (float option, 'c) t
      val point_or : default:float -> 'c point_dep -> date:Date.t -> (float, 'c) t
      val map : ('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
      val both : ('a, 'c) t -> ('b, 'c) t -> ('a * 'b, 'c) t
      val ( let+ ) : ('a, 'c) t -> ('a -> 'b) -> ('b, 'c) t
      val ( and+ ) : ('a, 'c) t -> ('b, 'c) t -> ('a * 'b, 'c) t
    end

    val of_seq : label:string -> 'c Period_cell.t Seq.t -> 'c t
    val const : period:Period.t -> (unit -> float) -> 'c cell
    val step : period:Period.t -> ('a, 'c) Query.t -> ('a -> float) -> 'c cell
    val unfold : label:string -> deps:('deps, 'c) deps -> cells:('deps -> 'c cell Seq.t) -> 'c t
    val unfold_self : label:string -> cells:(unit -> 'c cell Seq.t) -> 'c t
    val extend : label:string -> 'c t -> (Period.t -> 'c t) -> 'c t
    val reduce_sum : reduce
    val map : label:string -> (float -> float) -> 'c t Lazy.t -> 'c t
    val convert : label:string -> (Period.t -> float -> float) -> 'a t Lazy.t -> 'b t

    val map2 :
      label:string -> (float option -> float option -> float) -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t

    val const_ann_growth :
      label:string ->
      start:Date.t ->
      value:float ->
      rate:float ->
      offset:Offset.t ->
      yf:(Date.t -> Date.t -> float) ->
      'c t

    val sum : label:string -> 'c t Lazy.t list -> 'c t
    val sub : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
    val mul : label:string -> 'c t Lazy.t list -> 'c t
    val div : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
    val id : 'c t -> int
    val label : 'c t -> string
    val query : Period.t list -> 'c t -> 'c q_result list
    val to_seq : 'c t list -> 'c Period_cell.t Seq.t list
  end

  module Point : sig
    type 'c t = 'c point_t

    val const : label:string -> float -> 'c t
    val map : label:string -> (float -> float) -> 'c t Lazy.t -> 'c t
    val convert : label:string -> (Date.t -> float -> float) -> 'c t Lazy.t -> 'd t

    val accum :
      label:string -> start_date:Date.t -> initial_value:float -> 'c period_t Lazy.t -> 'c t

    val map2 :
      label:string -> (float option -> float option -> float) -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t

    val neg : label:string -> 'c t Lazy.t -> 'c t
    val sum : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
    val sub : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
    val mul : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
    val div : label:string -> 'c t Lazy.t -> 'c t Lazy.t -> 'c t
    val id : 'c t -> int
    val label : 'c t -> string
    val query : Date.t -> 'c t -> 'c q_result
    val query_many : Date.t list -> 'c t -> 'c q_result list
  end

  val eval : 'c q_result -> 'c eval_result
  val eval_many : 'c q_result list -> 'c eval_result list
  val labels : unit -> string list
  val label_of_id : int -> string
  val period_to_graph : 'c Period.t -> Graph.series
  val point_to_graph : 'c Point.t -> Graph.series
end

module Make () = struct
  (* Label store — maps label string to series id for diagnostics *)
  let label_store : (string, int) Hashtbl.t = Hashtbl.create 16
  let id_to_label : (int, string) Hashtbl.t = Hashtbl.create 16

  let register_label id label =
    match Hashtbl.find_opt label_store label with
    | Some existing_id ->
        raise (Series_types.Duplicate_label { label; existing_series_id = existing_id })
    | None ->
        Hashtbl.add label_store label id;
        Hashtbl.add id_to_label id label

  let labels () = Hashtbl.fold (fun label _ acc -> label :: acc) label_store []

  (* Types — representationally equal to Series_types *)
  type 'c period_t = 'c Period_series.t
  type 'c point_t = 'c Point_series.t

  type 'c q_result =
    | QRPeriod of { label : string; period : Period.t; cells : 'c Period_cell.t list }
    | QRPoint of { label : string; cell : 'c Point_cell.t option }

  type 'c eval_result =
    | Period of { label : string; period : Period.t; value : 'c amount }
    | Point of { label : string; point : (Date.t * 'c amount) option }

  let find_label id = Hashtbl.find id_to_label id

  (* Mutually recursive eval callbacks — same as before *)
  let rec eval_point cache date point_series =
    Point_series.eval_query ~eval_period cache point_series date

  and eval_period cache period_series period =
    Period_series.eval_query ~eval_point cache period period_series

  module Period = struct
    type 'c t = 'c period_t
    type reduce = Series_types.reduce
    type 'c dep = 'c t Lazy.t
    type 'c point_dep = 'c point_t Lazy.t
    type 'c dep_query = 'c Series_types.dep_query
    type 'c cell = 'c Series_types.unfold_cell

    type ('a, 'c) compiled_query = {
      queries : 'c dep_query list;
      decode : float list -> 'a * float list;
    }

    let take_one name = function
      | value :: rest -> (value, rest)
      | [] -> invalid_arg ("Series.Period.Query." ^ name ^ ": internal decode mismatch")

    let decode_all name decode values =
      let value, rest = decode values in
      match rest with
      | [] -> value
      | _ -> invalid_arg ("Series.Period." ^ name ^ ": internal decode mismatch")

    module Query = struct
      type ('a, 'c) t = ('a, 'c) compiled_query

      let pure value = { queries = []; decode = (fun xs -> (value, xs)) }

      let map f q =
        {
          queries = q.queries;
          decode =
            (fun xs ->
              let value, rest = q.decode xs in
              (f value, rest));
        }

      let both qa qb =
        {
          queries = qa.queries @ qb.queries;
          decode =
            (fun xs ->
              let a, xs = qa.decode xs in
              let b, xs = qb.decode xs in
              ((a, b), xs));
        }

      let ( let+ ) q f = map f q
      let ( and+ ) = both

      let self ~period ~reduce =
        { queries = [ Series_types.Self_query { period; reduce } ]; decode = take_one "self" }

      let period (dep : 'c dep) ~period ~reduce =
        {
          queries = [ Series_types.Period_query { dep; period; reduce } ];
          decode = take_one "period";
        }

      let point (dep : 'c point_dep) ~date =
        {
          queries =
            [
              Series_types.Point_present_query { dep; date }; Series_types.Point_query { dep; date };
            ];
          decode =
            (fun xs ->
              let present, xs = take_one "point" xs in
              let value, xs = take_one "point" xs in
              ((if Float.equal present 0.0 then None else Some value), xs));
        }

      let point_or ~default dep ~date = map (Option.value ~default) (point dep ~date)
    end

    type ('a, 'c) deps = { run : unit -> 'a * 'c Series_types.series_dep list }

    let no_deps = { run = (fun () -> ((), [])) }

    let dep_period (s : 'c t Lazy.t) : ('c dep, 'c) deps =
      { run = (fun () -> (s, [ Series_types.Period_dep s ])) }

    let dep_point (s : 'c point_t Lazy.t) : ('c point_dep, 'c) deps =
      { run = (fun () -> (s, [ Series_types.Point_dep s ])) }

    let both (a : ('a, 'c) deps) (b : ('b, 'c) deps) : ('a * 'b, 'c) deps =
      {
        run =
          (fun () ->
            let va, da = a.run () in
            let vb, db = b.run () in
            ((va, vb), da @ db));
      }

    let map (f : 'a -> 'b) (a : ('a, 'c) deps) : ('b, 'c) deps =
      {
        run =
          (fun () ->
            let v, d = a.run () in
            (f v, d));
      }

    let ( let+ ) a f = map f a
    let ( and+ ) = both
    let const ~period f = Series_types.Const { period; f }

    let step ~period (query : ('a, 'c) Query.t) f =
      Series_types.Step
        {
          period;
          queries = query.queries;
          f = (fun values -> f (decode_all "step" query.decode values));
        }

    let of_seq ~label cells =
      let s = Period_series.of_seq cells in
      register_label (Period_series.id s) label;
      s

    let unfold ~label ~deps ~cells =
      let deps_value, dep_list = deps.run () in
      let s = Period_series.unfold ~deps:dep_list (cells deps_value) in
      register_label (Period_series.id s) label;
      s

    let unfold_self ~label ~cells = unfold ~label ~deps:no_deps ~cells

    let extend ~label base cont =
      let s = Period_series.extend base cont in
      register_label (Period_series.id s) label;
      s

    let reduce_sum = Period_series.reduce_sum

    let map ~label f inner =
      let s = Period_series.map f inner in
      register_label (Period_series.id s) label;
      s

    let convert ~label f inner =
      let s = Period_series.convert f inner in
      register_label (Period_series.id s) label;
      s

    let map2 ~label f s1 s2 =
      let s = Period_series.map2 f s1 s2 in
      register_label (Period_series.id s) label;
      s

    let const_ann_growth ~label ~start ~value ~rate ~offset ~yf =
      let s = Period_series.const_ann_growth ~start ~value ~rate ~offset ~yf in
      register_label (Period_series.id s) label;
      s

    let sum ~label s_list =
      let s =
        match s_list with
        | [] -> Period_series.of_seq Seq.empty
        | [ single ] -> Lazy.force single
        | first :: rest ->
            List.fold_left (fun acc s -> Period_series.sum (lazy acc) s) (Lazy.force first) rest
      in
      register_label (Period_series.id s) label;
      s

    let sub ~label s1 s2 =
      let s = Period_series.sub s1 s2 in
      register_label (Period_series.id s) label;
      s

    let mul ~label s_list =
      let s =
        match s_list with
        | [] -> Period_series.of_seq Seq.empty
        | [ single ] -> Lazy.force single
        | first :: rest ->
            List.fold_left (fun acc s -> Period_series.mul (lazy acc) s) (Lazy.force first) rest
      in
      register_label (Period_series.id s) label;
      s

    let div ~label s1 s2 =
      let s = Period_series.div s1 s2 in
      register_label (Period_series.id s) label;
      s

    let id = Period_series.id
    let label s = find_label (Period_series.id s)

    let query periods series =
      let cache = Series_types.create_cache () in
      let lbl = find_label (Period_series.id series) in
      List.map
        (fun period ->
          let seq = Period_series.eval_query ~eval_point cache period series in
          let cells = List.of_seq seq in
          QRPeriod { label = lbl; period; cells })
        periods

    let to_seq series_list =
      let cache = Series_types.create_cache () in
      List.map (Period_series.eval_seq ~eval_point cache) series_list
  end

  module Point = struct
    type 'c t = 'c point_t

    let const ~label value =
      let s = Point_series.const value in
      register_label (Point_series.id s) label;
      s

    let map ~label f inner =
      let s = Point_series.map f inner in
      register_label (Point_series.id s) label;
      s

    let convert ~label f inner =
      let s = Point_series.convert f inner in
      register_label (Point_series.id s) label;
      s

    let accum ~label ~start_date ~initial_value changes =
      let s = Point_series.accum ~start_date ~initial_value changes in
      register_label (Point_series.id s) label;
      s

    let map2 ~label f s1 s2 =
      let s = Point_series.map2 f s1 s2 in
      register_label (Point_series.id s) label;
      s

    let neg ~label inner =
      let s = Point_series.neg inner in
      register_label (Point_series.id s) label;
      s

    let sum ~label s1 s2 =
      let s = Point_series.sum s1 s2 in
      register_label (Point_series.id s) label;
      s

    let sub ~label s1 s2 =
      let s = Point_series.sub s1 s2 in
      register_label (Point_series.id s) label;
      s

    let mul ~label s1 s2 =
      let s = Point_series.mul s1 s2 in
      register_label (Point_series.id s) label;
      s

    let div ~label s1 s2 =
      let s = Point_series.div s1 s2 in
      register_label (Point_series.id s) label;
      s

    let id = Point_series.id
    let label s = find_label (Point_series.id s)

    let query date series =
      let cache = Series_types.create_cache () in
      let cell = Point_series.eval_query ~eval_period cache series date in
      let lbl = find_label (Point_series.id series) in
      QRPoint { label = lbl; cell }

    let query_many dates series =
      let cache = Series_types.create_cache () in
      let lbl = find_label (Point_series.id series) in
      List.map
        (fun date ->
          let cell = Point_series.eval_query ~eval_period cache series date in
          QRPoint { label = lbl; cell })
        dates
  end

  (* Evaluation — wraps the internal Eval module so users never touch it directly *)

  let eval (type c) (qr : c q_result) : c eval_result =
    match qr with
    | QRPeriod { label; period; cells } ->
        let wrapped = List.map (fun c -> Cell_types.PeriodCell c) cells in
        let cache = Cell_cache.create () in
        List.iter (Eval.prime_tree cache) wrapped;
        Eval.iterate cache wrapped 1;
        let value = List.fold_left (fun acc c -> acc +. Eval.read_result cache c) 0.0 wrapped in
        Period { label; period; value = Amount value }
    | QRPoint { label; cell } -> (
        match cell with
        | None -> Point { label; point = None }
        | Some c ->
            let wrapped = Cell_types.PointCell c in
            let cache = Cell_cache.create () in
            Eval.prime_tree cache wrapped;
            Eval.iterate cache [ wrapped ] 1;
            let value = Eval.read_result cache wrapped in
            let date = Point_cell.date c in
            Point { label; point = Some (date, Amount value) })

  let eval_many (type c) (qrs : c q_result list) : c eval_result list =
    (* Collect every cell from every query result into a flat list for shared solving. *)
    let all_cells =
      List.concat_map
        (fun (qr : c q_result) ->
          match qr with
          | QRPeriod { cells; _ } -> List.map (fun c -> Cell_types.PeriodCell c) cells
          | QRPoint { cell = Some c; _ } -> [ Cell_types.PointCell c ]
          | QRPoint { cell = None; _ } -> [])
        qrs
    in
    let cache = Cell_cache.create () in
    List.iter (Eval.prime_tree cache) all_cells;
    Eval.iterate cache all_cells 1;
    (* Read back results per query result. *)
    List.map
      (fun (qr : c q_result) ->
        match qr with
        | QRPeriod { label; period; cells } ->
            let wrapped = List.map (fun c -> Cell_types.PeriodCell c) cells in
            let value = List.fold_left (fun acc c -> acc +. Eval.read_result cache c) 0.0 wrapped in
            Period { label; period; value = Amount value }
        | QRPoint { label; cell = None } -> Point { label; point = None }
        | QRPoint { label; cell = Some c } ->
            let wrapped = Cell_types.PointCell c in
            let value = Eval.read_result cache wrapped in
            let date = Point_cell.date c in
            Point { label; point = Some (date, Amount value) })
      qrs

  let label_of_id = find_label

  (* Graph bridge functions *)
  let period_to_graph (s : 'c Period.t) : Graph.series = Graph.pack_period s
  let point_to_graph (s : 'c Point.t) : Graph.series = Graph.pack_point s
end
