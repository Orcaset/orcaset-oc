open Orcaset

(* --- Configuration -------------------------------------------------------- *)

let start_date = Date.make 2025 12 31
let offset = Offset.make ~months:3 ~month_end:true ()
let quarters = Period.make_seq ~start_date ~offset
let initial_period = Period.make start_date (Date.shift offset start_date)
let useful_life = 4
let starting_ppe = -75.0
let neg_offset n = Offset.make ~months:(-(n * 3)) ~month_end:true ()

(* --- Capex ---------------------------------------------------------------- *)

let capex =
  let step period =
    Series.Period.step ~period
      (Series.Period.Query.self
         ~period:(Period.prev (neg_offset 1) period)
         ~reduce:Series.Period.reduce_sum)
      (fun prev -> prev -. 100.0)
  in
  let rec cells last () =
    let p = Period.next offset last in
    Seq.Cons (step p, cells p)
  in
  Series.Period.unfold_self ~label:"Capex" ~cells:(fun () ->
      Seq.cons
        (Series.Period.const ~period:initial_period (fun () -> -100.0))
        (cells initial_period))

(* --- Existing PPE Depreciation Run-off ------------------------------------ *)

let existing_runoff =
  let p1 = Period.make (Date.make 2025 12 31) (Date.make 2026 3 31) in
  let p2 = Period.make (Date.make 2026 3 31) (Date.make 2026 6 30) in
  Series.Period.of_seq ~label:"Existing Runoff"
    (List.to_seq
       [
         Period_cell.const p1 (fun () -> -50.0) Period_cell.proportional_split;
         Period_cell.const p2 (fun () -> -25.0) Period_cell.proportional_split;
       ])

(* --- Depreciation by Lookback Offset -------------------------------------- *)

(* dep_offset.(i) in period P = capex(P - i*offset) / useful_life. *)
let dep_offsets =
  List.init useful_life (fun i ->
      Series.Period.unfold
        ~label:(Printf.sprintf "Dep Offset %d" i)
        ~deps:(Series.Period.dep_period (lazy capex))
        ~cells:(fun capex_dep ->
          Seq.map
            (fun period ->
              let lb = Period.shift (neg_offset i) period in
              Series.Period.step ~period
                (Series.Period.Query.period capex_dep ~period:lb ~reduce:Series.Period.reduce_sum)
                (fun cv -> cv /. Float.of_int useful_life))
            quarters))

(* --- Total Depreciation --------------------------------------------------- *)

let depreciation =
  let total_offsets =
    Series.Period.sum ~label:"Total Future Depreciation" (List.map (fun o -> lazy o) dep_offsets)
  in
  Series.Period.sum ~label:"Depreciation" [ lazy existing_runoff; lazy total_offsets ]

(* --- PPE ------------------------------------------------------------------ *)

let ppe_change = Series.Period.sub ~label:"PPE Change" (lazy capex) (lazy depreciation)

let ppe_net =
  Series.Point.accum ~label:"PPE Net" ~start_date ~initial_value:starting_ppe (lazy ppe_change)

(* --- Output --------------------------------------------------------------- *)

let num_periods = 8
let query_periods = List.of_seq (Seq.take num_periods quarters)

(* --- Dependency Graph ----------------------------------------------------- *)

let write_dep_graph () =
  let dot_path = "examples/6-Depreciation/depreciation_deps.dot" in
  let oc = open_out dot_path in
  let ppf = Format.formatter_of_out_channel oc in
  Graph.pp_dot ppf [ Series.period_to_graph depreciation ];
  Format.pp_print_flush ppf ();
  close_out oc

let () =
  let open Series.Stmt in
  let stmt =
    group
      [
        period_total ppe_change [ period_line capex; period_line depreciation ]; point_line ppe_net;
      ]
  in
  print_string (pp stmt query_periods);
  write_dep_graph ()
