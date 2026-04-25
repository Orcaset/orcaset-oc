(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

(** Flat series module that ties the knot between period and point series evaluation.

    Parallels {!Eval} at the cell level: creates the shared cache and provides the callbacks that
    route evaluation across the period/point boundary. *)

type 'c amount = Amount of float [@@unboxed]

(* Types — aliases to the underlying implementations *)
type 'c period_t = 'c Period_series.t
type 'c point_t = 'c Point_series.t

type 'c q_result =
  | QRPeriod of { label : string; period : Period.t; cells : 'c Period_cell.t list }
  | QRPoint of { label : string; cell : 'c Point_cell.t option }

type 'c eval_result =
  | Period of { label : string; period : Period.t; value : 'c amount }
  | Point of { label : string; point : (Date.t * 'c amount) option }

(* Mutually recursive eval callbacks — module-level let rec *)
let rec eval_point cache date point_series =
  Point_series.eval_query ~eval_period cache point_series date

and eval_period cache period_series period =
  Period_series.eval_query ~eval_point cache period period_series

type period = Period.t

module LibPeriod = Period

module Period = struct
  type 'c t = 'c period_t
  type reduce = Series_types.reduce
  type 'c dep = 'c t Lazy.t
  type 'c point_dep = 'c point_t Lazy.t
  type 'c dep_query = 'c Series_types.period_dep_query
  type 'c cell = 'c Series_types.period_unfold_cell

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
      { queries = [ Series_types.Period_self_query { period; reduce } ]; decode = take_one "self" }

    let period (dep : 'c dep) ~period ~reduce =
      {
        queries = [ Series_types.Period_period_query { dep; period; reduce } ];
        decode = take_one "period";
      }

    let point (dep : 'c point_dep) ~date =
      {
        queries =
          [
            Series_types.Period_point_present_query { dep; date };
            Series_types.Period_point_query { dep; date };
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

  let run_deps (deps : ('a, 'c) deps) = deps.run ()
  let ( let+ ) a f = map f a
  let ( and+ ) = both
  let const ~period f = Series_types.Period_const_cell { period; f }

  let step ~period (query : ('a, 'c) Query.t) f =
    Series_types.Period_step_cell
      {
        period;
        queries = query.queries;
        f = (fun values -> f (decode_all "step" query.decode values));
      }

  let of_seq ~label cells = Period_series.of_seq ~label cells

  let unfold_seq ~label ~deps ~cells =
    let deps_value, dep_list = deps.run () in
    Period_series.unfold ~label ~deps:dep_list (cells deps_value)

  let unfold_seq_self ~label ~cells = unfold_seq ~label ~deps:no_deps ~cells

  let unfold ~label ~deps ~init ~cells =
    let deps_value, dep_list = deps.run () in
    Period_series.unfold ~label ~deps:dep_list (Seq.unfold (cells deps_value) init)

  let unfold_self ~label ~init ~cells =
    unfold ~label ~deps:no_deps ~init ~cells:(fun () state -> cells state)

  let extend ~label base cont = Period_series.extend ~label base cont
  let reduce_sum = Period_series.reduce_sum
  let map ~label f inner = Period_series.map ~label f inner
  let convert ~label f inner = Period_series.convert ~label f inner
  let map2 ~label f s1 s2 = Period_series.map2 ~label f s1 s2
  let const_ann_growth ~label = Period_series.const_ann_growth ~label

  let sum ~label s_list =
    match s_list with
    | [] -> Period_series.of_seq ~label Seq.empty
    | [ single ] -> Period_series.map ~label Fun.id single
    | first :: rest ->
        let s =
          List.fold_left
            (fun acc s -> Period_series.sum ~label (lazy acc) s)
            (Lazy.force first) rest
        in
        s

  let sub ~label s1 s2 = Period_series.sub ~label s1 s2

  let mul ~label s_list =
    match s_list with
    | [] -> Period_series.of_seq ~label Seq.empty
    | [ single ] -> Period_series.map ~label Fun.id single
    | first :: rest ->
        let s =
          List.fold_left
            (fun acc s -> Period_series.mul ~label (lazy acc) s)
            (Lazy.force first) rest
        in
        s

  let div ~label s1 s2 = Period_series.div ~label s1 s2
  let filter ~label f inner = Period_series.filter ~label f inner
  let after ~label date inner = Period_series.after ~label date inner
  let id = Period_series.id
  let label = Period_series.label

  let query periods series =
    let cache = Series_types.create_cache () in
    let lbl = Period_series.label series in
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
  type reduce = Series_types.reduce
  type 'c dep = 'c Period.dep
  type 'c point_dep = 'c Period.point_dep
  type 'c dep_query = 'c Series_types.point_dep_query
  type 'c cell = 'c Series_types.point_unfold_cell
  type ('a, 'c) deps = ('a, 'c) Period.deps

  let no_deps = Period.no_deps
  let dep_period = Period.dep_period
  let dep_point = Period.dep_point
  let both = Period.both
  let ( let+ ) = Period.( let+ )
  let ( and+ ) = Period.( and+ )
  let const ~label value = Point_series.const ~label value

  type ('a, 'c) compiled_query = {
    queries : 'c dep_query list;
    decode : float list -> 'a * float list;
  }

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

    let self ~date =
      {
        queries =
          [
            Series_types.Point_self_present_query { date };
            Series_types.Point_self_query { date };
          ];
        decode =
          (fun xs ->
            let present, xs = Period.take_one "self" xs in
            let value, xs = Period.take_one "self" xs in
            ((if Float.equal present 0.0 then None else Some value), xs));
      }

    let self_or ~default ~date = map (Option.value ~default) (self ~date)

    let period (dep : 'c dep) ~period ~reduce =
      {
        queries = [ Series_types.Point_period_query { dep; period; reduce } ];
        decode = Period.take_one "period";
      }

    let point (dep : 'c point_dep) ~date =
      {
        queries =
          [
            Series_types.Point_point_present_query { dep; date };
            Series_types.Point_point_query { dep; date };
          ];
        decode =
          (fun xs ->
            let present, xs = Period.take_one "point" xs in
            let value, xs = Period.take_one "point" xs in
            ((if Float.equal present 0.0 then None else Some value), xs));
      }

    let point_or ~default dep ~date = map (Option.value ~default) (point dep ~date)
  end

  let const_cell ~period f = Series_types.Point_const_cell { period; f }

  let step ~period (query : ('a, 'c) Query.t) f =
    Series_types.Point_step_cell
      {
        period;
        queries = query.queries;
        f = (fun values -> f (Period.decode_all "step" query.decode values));
      }

  let unfold_seq ~label ~deps ~cells =
    let deps_value, dep_list = Period.run_deps deps in
    Point_series.unfold ~label ~deps:dep_list (cells deps_value)

  let unfold_seq_self ~label ~cells = unfold_seq ~label ~deps:no_deps ~cells

  let unfold ~label ~deps ~init ~cells =
    let deps_value, dep_list = Period.run_deps deps in
    Point_series.unfold ~label ~deps:dep_list (Seq.unfold (cells deps_value) init)

  let unfold_self ~label ~init ~cells =
    unfold ~label ~deps:no_deps ~init ~cells:(fun () state -> cells state)

  let map ~label f inner = Point_series.map ~label f inner
  let convert ~label f inner = Point_series.convert ~label f inner
  let map2 ~label f s1 s2 = Point_series.map2 ~label f s1 s2
  let neg ~label inner = Point_series.neg ~label inner
  let sum ~label s1 s2 = Point_series.sum ~label s1 s2
  let sub ~label s1 s2 = Point_series.sub ~label s1 s2
  let mul ~label s1 s2 = Point_series.mul ~label s1 s2
  let div ~label s1 s2 = Point_series.div ~label s1 s2
  let id = Point_series.id
  let label = Point_series.label

  let query date series =
    let cache = Series_types.create_cache () in
    let cell = Point_series.eval_query ~eval_period cache series date in
    let lbl = Point_series.label series in
    QRPoint { label = lbl; cell }

  let query_many dates series =
    let cache = Series_types.create_cache () in
    let lbl = Point_series.label series in
    dates
    |> List.mapi (fun index date -> (index, date))
    |> List.sort (fun (_, left) (_, right) -> Date.compare left right)
    |> List.map (fun (index, date) ->
           let cell = Point_series.eval_query ~eval_period cache series date in
           (index, QRPoint { label = lbl; cell }))
    |> List.sort (fun (left, _) (right, _) -> Int.compare left right)
    |> List.map snd
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

(* Graph bridge functions *)
let period_to_graph (s : 'c Period.t) : Graph.series = Graph.PeriodSeries s
let point_to_graph (s : 'c Point.t) : Graph.series = Graph.PointSeries s

module Stmt = struct
  type packed_series =
    | PackedPeriod : _ Period.t -> packed_series
    | PackedPoint : _ Point.t -> packed_series

  type t =
    | Line of { label : string; series : packed_series }
    | Total of { label : string; series : packed_series; children : t list }
    | Group of { children : t list }

  (** A column slot produced during the query phase. Holds either a list of period cells (to be
      summed) or a single optional point cell. The cell cache is read later in the format phase. *)
  type col_slot = PeriodSlot of Cell_types.cell list | PointSlot of Cell_types.cell option

  type column_role =
    | Start_anchor
    | Period_end

  type slot_kind =
    | Slot_empty
    | Slot_period of period
    | Slot_point of Date.t

  type row_kind =
    | Row_line
    | Row_total

  type series_kind =
    | Series_period
    | Series_point

  type column = {
    id : string;
    label : string;
    date : Date.t;
    role : column_role;
  }

  type slot = {
    column_id : string;
    kind : slot_kind;
    value : float option;
    cell_ids : string list;
  }

  type row = {
    id : string;
    parent_id : string option;
    depth : int;
    kind : row_kind;
    label : string;
    series_kind : series_kind;
    series_runtime_id : int;
    slots : slot list;
  }

  type cell_kind =
    | Cell_period
    | Cell_point

  type cell_op =
    | Cell_const
    | Cell_deps
    | Cell_map
    | Cell_convert
    | Cell_map2
    | Cell_clip
    | Cell_ref

  type cell = {
    id : string;
    runtime_id : int;
    kind : cell_kind;
    op : cell_op;
    value : float;
    period : period option;
    date : Date.t option;
    dep_ids : string list;
  }

  type snapshot = {
    version : int;
    columns : column list;
    rows : row list;
    cells : cell list;
  }

  type statement_snapshot = {
    id : string;
    rows : row list;
  }

  type model_snapshot = {
    version : int;
    columns : column list;
    statements : statement_snapshot list;
    cells : cell list;
  }

  type pp_row = Data of { indent : int; label : string; values : string list } | Sep

  type snapshot_row_acc = {
    id : string;
    parent_id : string option;
    depth : int;
    kind : row_kind;
    label : string;
    series_kind : series_kind;
    series_runtime_id : int;
    slot_kinds : slot_kind list;
    slots : col_slot list;
  }

  type collected_statement = {
    id : string;
    rows : snapshot_row_acc list;
  }

  let snapshot_version = 1

  let period_line s = Line { label = Period.label s; series = PackedPeriod s }
  let point_line s = Line { label = Point.label s; series = PackedPoint s }
  let period_total s children = Total { label = Period.label s; series = PackedPeriod s; children }
  let point_total s children = Total { label = Point.label s; series = PackedPoint s; children }
  let group children = Group { children }

  let dates_of_periods periods =
    match periods with
    | [] -> []
    | first :: _ -> LibPeriod.start_date first :: List.map LibPeriod.end_date periods

  let columns_of_dates dates =
    List.mapi
      (fun idx date ->
        {
          id = Printf.sprintf "c%d" idx;
          label = Date.to_string date;
          date;
          role = if idx = 0 then Start_anchor else Period_end;
        })
      dates

  let columns_of_periods periods =
    let dates = dates_of_periods periods in
    (dates, columns_of_dates dates)

  let format_number v =
    let s = Printf.sprintf "%.0f" (Float.abs v) in
    let len = String.length s in
    let buf = Buffer.create (len + (len / 3)) in
    String.iteri
      (fun i c ->
        if i > 0 && (len - i) mod 3 = 0 then Buffer.add_char buf ',';
        Buffer.add_char buf c)
      s;
    let formatted = Buffer.contents buf in
    if v < -0.5 then "(" ^ formatted ^ ")" else formatted

  (** Build column slots for a single series using a shared series-level cache. Returns one
      [col_slot] per date/period column and accumulates every wrapped cell into [all_cells_acc] for
      later batch solving. *)
  let query_slots series_cache all_cells_acc periods dates packed =
    match packed with
    | PackedPeriod s ->
        let slots =
          List.map
            (fun period ->
              let seq = Period_series.eval_query ~eval_point series_cache period s in
              let cells = List.of_seq seq in
              let wrapped = List.map (fun c -> Cell_types.PeriodCell c) cells in
              List.iter (fun c -> all_cells_acc := c :: !all_cells_acc) wrapped;
              PeriodSlot wrapped)
            periods
        in
        (* Period series get a leading empty start-date column when periods are present. *)
        (match slots with [] -> [] | _ -> PointSlot None :: slots)
    | PackedPoint s ->
        List.map
          (fun date ->
            let cell = Point_series.eval_query ~eval_period series_cache s date in
            let wrapped = Option.map (fun c -> Cell_types.PointCell c) cell in
            Option.iter (fun c -> all_cells_acc := c :: !all_cells_acc) wrapped;
            PointSlot wrapped)
          dates

  (** Phase 1: traverse the statement tree, query every series using a shared series cache, and
      collect all cells. Returns a tree of [col_slot list] per node, mirroring the original [t]
      structure. *)
  type slot_tree =
    | SLine of { indent : int; label : string; slots : col_slot list }
    | STotal of { indent : int; label : string; slots : col_slot list }
    | SSep

  let rec collect_slots series_cache all_cells_acc periods dates indent = function
    | Line { label; series } ->
        let slots = query_slots series_cache all_cells_acc periods dates series in
        [ SLine { indent; label; slots } ]
    | Total { label; series; children } ->
        let child_trees =
          List.concat_map
            (collect_slots series_cache all_cells_acc periods dates (indent + 1))
            children
        in
        let slots = query_slots series_cache all_cells_acc periods dates series in
        child_trees @ [ SSep; STotal { indent; label; slots } ]
    | Group { children } ->
        List.concat_map (collect_slots series_cache all_cells_acc periods dates indent) children

  (** Phase 3: read solved values from the cell cache and format each slot. *)
  let read_slot cell_cache = function
    | PeriodSlot cells ->
        let v = List.fold_left (fun acc c -> acc +. Eval.read_result cell_cache c) 0.0 cells in
        Some v
    | PointSlot None -> None
    | PointSlot (Some c) -> Some (Eval.read_result cell_cache c)

  let format_value = function None -> "" | Some v -> format_number v

  let slot_tree_to_pp_rows cell_cache =
    List.map (function
      | SLine { indent; label; slots } ->
          let values = List.map (fun s -> format_value (read_slot cell_cache s)) slots in
          Data { indent; label; values }
      | STotal { indent; label; slots; _ } ->
          let values = List.map (fun s -> format_value (read_slot cell_cache s)) slots in
          Data { indent; label; values }
      | SSep -> Sep)

  let packed_series_info = function
    | PackedPeriod s -> (Series_period, Period.id s)
    | PackedPoint s -> (Series_point, Point.id s)

  let slot_kinds periods dates = function
    | PackedPeriod _ -> (
        match periods with
        | [] -> []
        | _ -> Slot_empty :: List.map (fun period -> Slot_period period) periods)
    | PackedPoint _ -> List.map (fun date -> Slot_point date) dates

  let make_row_id scope path =
    let local =
      match path with
      | [] -> "r"
      | _ -> "r" ^ String.concat "." (List.map string_of_int path)
    in
    match scope with None -> local | Some prefix -> prefix ^ ":" ^ local

  let rec collect_snapshot_rows series_cache all_cells_acc periods dates scope parent_id depth path =
    function
    | Line { label; series } ->
        let id = make_row_id scope path in
        let series_kind, series_runtime_id = packed_series_info series in
        let slots = query_slots series_cache all_cells_acc periods dates series in
        let slot_kinds = slot_kinds periods dates series in
        [
          {
            id;
            parent_id;
            depth;
            kind = Row_line;
            label;
            series_kind;
            series_runtime_id;
            slot_kinds;
            slots;
          };
        ]
    | Total { label; series; children } ->
        let id = make_row_id scope path in
        let child_rows =
          List.concat
            (List.mapi
               (fun idx child ->
                 collect_snapshot_rows series_cache all_cells_acc periods dates scope (Some id)
                   (depth + 1) (path @ [ idx ]) child)
               children)
        in
        let series_kind, series_runtime_id = packed_series_info series in
        let slots = query_slots series_cache all_cells_acc periods dates series in
        let slot_kinds = slot_kinds periods dates series in
        child_rows
        @ [
            {
              id;
              parent_id;
              depth;
              kind = Row_total;
              label;
              series_kind;
              series_runtime_id;
              slot_kinds;
              slots;
            };
          ]
    | Group { children } ->
        List.concat
          (List.mapi
             (fun idx child ->
               collect_snapshot_rows series_cache all_cells_acc periods dates scope parent_id depth
                 (path @ [ idx ]) child)
             children)

  let cell_export_id = function
    | Cell_types.PeriodCell c ->
        Printf.sprintf "period:%d:%s" (Period_cell.id c)
          (LibPeriod.to_string (Period_cell.period c))
    | Cell_types.PointCell c ->
        Printf.sprintf "point:%d:%s" (Point_cell.id c) (Date.to_string (Point_cell.date c))

  let slot_cell_ids = function
    | PeriodSlot cells -> List.map cell_export_id cells
    | PointSlot None -> []
    | PointSlot (Some cell) -> [ cell_export_id cell ]

  let direct_period_deps : type c. c Cell_types.period_cell -> Cell_types.cell list = function
    | Cell_types.RConst _ -> []
    | Cell_types.RDeps { deps; _ } -> deps
    | Cell_types.RMap { inner; _ } -> [ Cell_types.PeriodCell inner ]
    | Cell_types.RConvert { inner; _ } -> [ Cell_types.PeriodCell inner ]
    | Cell_types.RMap2 { c1; c2; _ } ->
        List.filter_map (Option.map (fun cell -> Cell_types.PeriodCell cell)) [ c1; c2 ]
    | Cell_types.RClip { inner; _ } -> [ Cell_types.PeriodCell inner ]
    | Cell_types.RRef { state = Cell_types.Resolved inner; _ } -> [ Cell_types.PeriodCell inner ]
    | Cell_types.RRef { state = Cell_types.Unresolved _; _ } -> []
    | Cell_types.RRef { state = Cell_types.Resolving; _ } -> []

  let direct_point_deps : type c. c Cell_types.point_cell -> Cell_types.cell list = function
    | Cell_types.TConst _ -> []
    | Cell_types.TMap { inner; _ } -> [ Cell_types.PointCell inner ]
    | Cell_types.TConvert { inner; _ } -> [ Cell_types.PointCell inner ]
    | Cell_types.TDeps { deps; _ } -> deps
    | Cell_types.TRef { cell = Some inner; _ } -> [ Cell_types.PointCell inner ]
    | Cell_types.TRef { cell = None; _ } -> []

  let direct_cell_deps = function
    | Cell_types.PeriodCell c -> direct_period_deps c
    | Cell_types.PointCell c -> direct_point_deps c

  let period_cell_op : type c. c Cell_types.period_cell -> cell_op = function
    | Cell_types.RConst _ -> Cell_const
    | Cell_types.RDeps _ -> Cell_deps
    | Cell_types.RMap _ -> Cell_map
    | Cell_types.RConvert _ -> Cell_convert
    | Cell_types.RMap2 _ -> Cell_map2
    | Cell_types.RClip _ -> Cell_clip
    | Cell_types.RRef _ -> Cell_ref

  let point_cell_op : type c. c Cell_types.point_cell -> cell_op = function
    | Cell_types.TConst _ -> Cell_const
    | Cell_types.TMap _ -> Cell_map
    | Cell_types.TConvert _ -> Cell_convert
    | Cell_types.TDeps _ -> Cell_deps
    | Cell_types.TRef _ -> Cell_ref

  let collect_cells cell_cache roots =
    let seen = Hashtbl.create 128 in
    let collected = ref [] in
    let rec visit cell =
      let id = cell_export_id cell in
      if not (Hashtbl.mem seen id) then begin
        Hashtbl.add seen id ();
        let deps = direct_cell_deps cell in
        List.iter visit deps;
        let dep_ids = List.map cell_export_id deps in
        let value = Eval.read_result cell_cache cell in
        let cell_snapshot =
          match cell with
          | Cell_types.PeriodCell c ->
              {
                id;
                runtime_id = Period_cell.id c;
                kind = Cell_period;
                op = period_cell_op c;
                value;
                period = Some (Period_cell.period c);
                date = None;
                dep_ids;
              }
          | Cell_types.PointCell c ->
              {
                id;
                runtime_id = Point_cell.id c;
                kind = Cell_point;
                op = point_cell_op c;
                value;
                period = None;
                date = Some (Point_cell.date c);
                dep_ids;
              }
        in
        collected := cell_snapshot :: !collected
      end
    in
    List.iter visit roots;
    List.rev !collected

  let solve_cells all_cells =
    let cell_cache = Cell_cache.create () in
    List.iter (Eval.prime_tree cell_cache) all_cells;
    (match all_cells with [] -> () | _ -> Eval.iterate cell_cache all_cells 1);
    cell_cache

  let rec map3 f xs ys zs =
    match (xs, ys, zs) with
    | [], [], [] -> []
    | x :: xs, y :: ys, z :: zs -> f x y z :: map3 f xs ys zs
    | _ -> invalid_arg "Series.Stmt.map3: mismatched list lengths"

  let row_of_acc (columns : column list) (cell_cache : Cell_cache.t) (row : snapshot_row_acc) : row =
    {
      id = row.id;
      parent_id = row.parent_id;
      depth = row.depth;
      kind = row.kind;
      label = row.label;
      series_kind = row.series_kind;
      series_runtime_id = row.series_runtime_id;
      slots =
        map3
          (fun (column : column) kind slot ->
            {
              column_id = column.id;
              kind;
              value = read_slot cell_cache slot;
              cell_ids = slot_cell_ids slot;
            })
          columns row.slot_kinds row.slots;
    }

  let snapshot t (periods : period list) =
    let dates, columns = columns_of_periods periods in
    let series_cache = Series_types.create_cache () in
    let all_cells_acc = ref [] in
    let rows = collect_snapshot_rows series_cache all_cells_acc periods dates None None 0 [] t in
    let all_cells = List.rev !all_cells_acc in
    let cell_cache = solve_cells all_cells in
    {
      version = snapshot_version;
      columns;
      rows = List.map (row_of_acc columns cell_cache) rows;
      cells = collect_cells cell_cache all_cells;
    }

  let snapshot_many statements (periods : period list) =
    let dates, columns = columns_of_periods periods in
    let series_cache = Series_types.create_cache () in
    let all_cells_acc = ref [] in
    let collected_statements =
      List.map
        (fun (id, stmt) ->
          {
            id;
            rows = collect_snapshot_rows series_cache all_cells_acc periods dates (Some id) None 0 [] stmt;
          })
        statements
    in
    let all_cells = List.rev !all_cells_acc in
    let cell_cache = solve_cells all_cells in
    {
      version = snapshot_version;
      columns;
      statements =
        List.map
          (fun (statement : collected_statement) ->
            ({ id = statement.id; rows = List.map (row_of_acc columns cell_cache) statement.rows }
              : statement_snapshot))
          collected_statements;
      cells = collect_cells cell_cache all_cells;
    }

  let json_escape s =
    let buf = Buffer.create (String.length s + 8) in
    String.iter
      (function
        | '"' -> Buffer.add_string buf "\\\""
        | '\\' -> Buffer.add_string buf "\\\\"
        | '\b' -> Buffer.add_string buf "\\b"
        | '\012' -> Buffer.add_string buf "\\f"
        | '\n' -> Buffer.add_string buf "\\n"
        | '\r' -> Buffer.add_string buf "\\r"
        | '\t' -> Buffer.add_string buf "\\t"
        | c when Char.code c < 0x20 ->
            Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
        | c -> Buffer.add_char buf c)
      s;
    Buffer.contents buf

  let json_string s = "\"" ^ json_escape s ^ "\""
  let json_float f = Printf.sprintf "%.12g" f
  let json_option f = function None -> "null" | Some value -> f value
  let json_list f items = "[" ^ String.concat "," (List.map f items) ^ "]"
  let json_field name value = Printf.sprintf "%s:%s" (json_string name) value
  let json_object fields = "{" ^ String.concat "," fields ^ "}"

  let json_date date = json_string (Date.to_string date)
  let json_period period = json_string (LibPeriod.to_string period)

  let json_column_role = function
    | Start_anchor -> json_string "start_anchor"
    | Period_end -> json_string "period_end"

  let json_slot_kind = function
    | Slot_empty -> json_string "empty"
    | Slot_period period -> json_object [ json_field "period" (json_period period) ]
    | Slot_point date -> json_object [ json_field "date" (json_date date) ]

  let json_row_kind = function
    | Row_line -> json_string "line"
    | Row_total -> json_string "total"

  let json_series_kind = function
    | Series_period -> json_string "period"
    | Series_point -> json_string "point"

  let json_cell_kind = function
    | Cell_period -> json_string "period"
    | Cell_point -> json_string "point"

  let json_cell_op = function
    | Cell_const -> json_string "const"
    | Cell_deps -> json_string "deps"
    | Cell_map -> json_string "map"
    | Cell_convert -> json_string "convert"
    | Cell_map2 -> json_string "map2"
    | Cell_clip -> json_string "clip"
    | Cell_ref -> json_string "ref"

  let json_column (column : column) =
    json_object
      [
        json_field "id" (json_string column.id);
        json_field "label" (json_string column.label);
        json_field "date" (json_date column.date);
        json_field "role" (json_column_role column.role);
      ]

  let json_slot (slot : slot) =
    json_object
      [
        json_field "column_id" (json_string slot.column_id);
        json_field "kind" (json_slot_kind slot.kind);
        json_field "value" (json_option json_float slot.value);
        json_field "cell_ids" (json_list json_string slot.cell_ids);
      ]

  let json_row (row : row) =
    json_object
      [
        json_field "id" (json_string row.id);
        json_field "parent_id" (json_option json_string row.parent_id);
        json_field "depth" (string_of_int row.depth);
        json_field "kind" (json_row_kind row.kind);
        json_field "label" (json_string row.label);
        json_field "series_kind" (json_series_kind row.series_kind);
        json_field "series_runtime_id" (string_of_int row.series_runtime_id);
        json_field "slots" (json_list json_slot row.slots);
      ]

  let json_cell (cell : cell) =
    json_object
      [
        json_field "id" (json_string cell.id);
        json_field "runtime_id" (string_of_int cell.runtime_id);
        json_field "kind" (json_cell_kind cell.kind);
        json_field "op" (json_cell_op cell.op);
        json_field "value" (json_float cell.value);
        json_field "period" (json_option json_period cell.period);
        json_field "date" (json_option json_date cell.date);
        json_field "dep_ids" (json_list json_string cell.dep_ids);
      ]

  let json_statement (statement : statement_snapshot) =
    json_object
      [
        json_field "id" (json_string statement.id);
        json_field "rows" (json_list json_row statement.rows);
      ]

  let snapshot_to_json (snapshot : snapshot) =
    json_object
      [
        json_field "version" (string_of_int snapshot.version);
        json_field "columns" (json_list json_column snapshot.columns);
        json_field "rows" (json_list json_row snapshot.rows);
        json_field "cells" (json_list json_cell snapshot.cells);
      ]

  let model_snapshot_to_json (snapshot : model_snapshot) =
    json_object
      [
        json_field "version" (string_of_int snapshot.version);
        json_field "columns" (json_list json_column snapshot.columns);
        json_field "statements" (json_list json_statement snapshot.statements);
        json_field "cells" (json_list json_cell snapshot.cells);
      ]

  let pp t (periods : period list) =
    let dates = dates_of_periods periods in
    (* Phase 1: query all series with a shared series-level cache. *)
    let series_cache = Series_types.create_cache () in
    let all_cells_acc = ref [] in
    let slot_trees = collect_slots series_cache all_cells_acc periods dates 0 t in
    let all_cells = List.rev !all_cells_acc in
    (* Phase 2: solve all cells with a single shared cell cache. *)
    let cell_cache = solve_cells all_cells in
    (* Phase 3: format rows by reading from the shared cell cache. *)
    let rows = slot_tree_to_pp_rows cell_cache slot_trees in
    let indent_size = 2 in
    let date_strs = List.map Date.to_string dates in
    let header_label = "Period end" in
    let label_width =
      List.fold_left
        (fun acc r ->
          match r with
          | Data { indent; label; _ } -> max acc (String.length label + (indent * indent_size))
          | Sep -> acc)
        (String.length header_label) rows
      + 2
    in
    let col_width =
      let max_val =
        List.fold_left
          (fun acc r ->
            match r with
            | Data { values; _ } -> List.fold_left (fun a v -> max a (String.length v)) acc values
            | Sep -> acc)
          0 rows
      in
      let max_date = List.fold_left (fun a d -> max a (String.length d)) 0 date_strs in
      max max_val max_date + 2
    in
    let pad_right n s =
      if String.length s >= n then s else s ^ String.make (n - String.length s) ' '
    in
    let pad_left n s =
      if String.length s >= n then s else String.make (n - String.length s) ' ' ^ s
    in
    let num_cols = List.length dates in
    let buf = Buffer.create 256 in
    Buffer.add_string buf (pad_right label_width header_label);
    List.iter (fun d -> Buffer.add_string buf (pad_left col_width d)) date_strs;
    Buffer.add_char buf '\n';
    Buffer.add_string buf (String.make (label_width + (col_width * num_cols)) '-');
    Buffer.add_char buf '\n';
    List.iter
      (fun r ->
        match r with
        | Data { indent; label; values } ->
            let lbl = String.make (indent * indent_size) ' ' ^ label in
            Buffer.add_string buf (pad_right label_width lbl);
            List.iter (fun v -> Buffer.add_string buf (pad_left col_width v)) values;
            Buffer.add_char buf '\n'
        | Sep ->
            Buffer.add_string buf (String.make label_width ' ');
            let dashes = String.make (col_width - 2) '-' in
            List.iter (fun _ -> Buffer.add_string buf (pad_left col_width dashes)) dates;
            Buffer.add_char buf '\n')
      rows;
    Buffer.contents buf
end
