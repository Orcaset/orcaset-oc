open Orcaset

(* ----- Assumptions ----- *)

let initial_period_start = Date.make 2025 12 31
let qtr_offset = Offset.make ~quarters:1 ~month_end:true ()
let qtr_lookback = Offset.make ~quarters:(-1) ~month_end:true ()
let initial_period = Period.make initial_period_start (Date.shift qtr_offset initial_period_start)
let sum_agg = Series.Agg.sum
let sp = Series.Spans.pack
(* ----- Model ----- *)

let revenue =
  Series.Spans.unfold_rec ~label:"Revenue" ~agg:sum_agg
    ~deps:(fun self -> Series.Deps.span_dep self)
    ~init:initial_period
    ~cells:(fun read_revenue period ->
      let formula =
        if Period.equal period initial_period then Series.Formula.pure (Some 1_000.0)
        else
          let open Series.Formula in
          let+ prior_revenue = read_revenue ~period:(Period.prev qtr_lookback period) in
          Option.map (fun prior_revenue -> prior_revenue *. 1.03) prior_revenue
      in
      Some (Series.Spans.cell ~period ~split:Split.daily formula, Period.next qtr_offset period))
    ()

let expenses = Series.Spans.scale ~label:"Expenses" (-0.50) revenue
let profit = Series.Spans.sum ~label:"Profit" ~agg:sum_agg [ sp revenue; sp expenses ]

(* ----- Output ----- *)

(* Statement table output *)
let () =
  (* Create a series of periods to query over *)
  let output_periods = 8 in
  let periods =
    Period.make_seq ~start:initial_period_start ~offset:qtr_offset
    |> Seq.take output_periods |> List.of_seq
  in
  (* Create a statement and evaluate it over the periods *)
  let stmt = Stmt.span_total profit [ Stmt.span_line revenue; Stmt.span_line expenses ] in
  let resolved = Stmt.eval_periods periods stmt in
  (* Print the statement using a fixed-width formatter *)
  Printf.printf "\n%s\n\n" (Stmt.fixed_width resolved)

(* Query values over an arbitrary date range *)
let () =
  let query_period = Period.make (Date.make 2026 4 15) (Date.make 2026 9 15) in
  let cache = Series.make_cache () in
  let profit_opt = Series.query_span cache profit ~period:query_period in
  let profit = Option.map (Printf.sprintf "%.2f") profit_opt |> Option.value ~default:"n/a" in
  Printf.printf "Total profit over the period %s: %s\n" (Period.to_string query_period) profit
