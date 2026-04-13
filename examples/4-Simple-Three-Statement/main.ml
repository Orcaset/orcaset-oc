open Orcaset

(* A three-statement financial model: Income Statement, Cash Flow Statement, and Balance Sheet.
   Demonstrates circular dependencies (depreciation <-> PPE) and point series accumulation for balance
   sheet items. *)

let start_date = Date.make 2025 12 31
let offset = Offset.make ~months:1 ~month_end:true ()
let revenue_lookback = Offset.make ~months:(-1) ~month_end:true ()
let months = Period.make_seq ~start_date ~offset
let initial_period = Period.make start_date (Date.shift offset start_date)

(* --- Income Statement -------------------------------------------------------- *)

(* Revenue: $1,000/month initial, 5% annual growth using Actual/360 day count. *)
let revenue_step period =
  let period_start, period_end = Period.to_tuple period in
  let yf = Yf.actual_360 period_start period_end in
  Series.Period.step ~period
    (Series.Period.Query.self
       ~period:(Period.shift revenue_lookback period)
       ~reduce:Series.Period.reduce_sum)
    (fun last_value -> last_value *. ((1.0 +. 0.05) ** yf))

let revenue =
  let rec generate_cells last_period () =
    let current_period = Period.next offset last_period in
    Seq.Cons (revenue_step current_period, generate_cells current_period)
  in
  Series.Period.unfold_self ~label:"Revenue" ~cells:(fun () ->
      Seq.cons
        (Series.Period.const ~period:initial_period (fun () -> 1000.0))
        (generate_cells initial_period))

(* COGS: 30% of revenue. *)
let cogs = Series.Period.map ~label:"COGS" (fun r -> r *. -0.30) (lazy revenue)

(* Gross Profit: Revenue + COGS. *)
let gross_profit = Series.Period.sum ~label:"Gross Profit" [ lazy revenue; lazy cogs ]

(* Opex: constant -$200/month. *)
let opex =
  Series.Period.unfold_self ~label:"Opex" ~cells:(fun () ->
      Seq.map (fun period -> Series.Period.const ~period (fun () -> -200.0)) months)

(* Capex: 5% of revenue (negative = cash outflow). *)
let capex = Series.Period.map ~label:"Capex" (fun r -> r *. -0.05) (lazy revenue)

(* Depreciation and PPE Net: circular dependency.
   Depreciation = prior PPE Net x (10% / 12), as an expense (negative).
   PPE Net starts at $10,000, increases by capex additions, decreases by depreciation. *)
let rec depreciation =
  lazy
    (Series.Period.unfold ~label:"Depreciation" ~deps:(Series.Period.dep_point ppe_net)
       ~cells:(fun ppe_net ->
         Seq.map
           (fun period ->
             Series.Period.step ~period
               (Series.Period.Query.point ppe_net ~date:(Period.start_date period))
               (function Some ppe -> ppe *. (-0.10 /. 12.0) | None -> 0.0))
           months))

and ppe_net =
  lazy
    (* TODO: Update *)
    (* (let neg_capex = Series.Period.map ~label:"Neg Capex" (fun x -> -.x) (lazy capex) in *)
    (* let ppe_change = Series.Period.sum ~label:"PPE Change" [ lazy neg_capex; depreciation ] in *)
    (* Series.Point.accum ~label:"PPE Net" ~start_date ~initial_value:10000.0 (lazy ppe_change)) *)
    (Series.Point.const ~label:"PPE Net" 10000.0)

let depreciation = Lazy.force depreciation
let ppe_net = Lazy.force ppe_net

(* Earnings before tax: Gross Profit + Opex + Depreciation. *)
let ebt =
  let gp_opex = Series.Period.sum ~label:"GP + Opex" [ lazy gross_profit; lazy opex ] in
  Series.Period.sum ~label:"EBT" [ lazy gp_opex; lazy depreciation ]

(* Tax: 20% of EBT (negative). *)
let tax = Series.Period.map ~label:"Tax" (fun x -> x *. -0.20) (lazy ebt)

(* Net Income: EBT + Tax. *)
let net_income = Series.Period.sum ~label:"Net Income" [ lazy ebt; lazy tax ]

(* --- Cash Flow Statement ----------------------------------------------------- *)

(* Depreciation add back: reverses the non-cash depreciation charge. *)
let dep_add_back = Series.Period.map ~label:"Dep Add Back" (fun x -> -.x) (lazy depreciation)

(* CF Operations: Net Income + Depreciation Add Back. *)
let cf_operations = Series.Period.sum ~label:"CF Operations" [ lazy net_income; lazy dep_add_back ]

(* CF Financing: constant $0. *)
let cf_financing =
  Series.Period.unfold_self ~label:"CF Financing" ~cells:(fun () ->
      Seq.map (fun period -> Series.Period.const ~period (fun () -> 0.0)) months)

(* Net Cash Change: CF Operations + Capex + CF Financing. *)
let net_cash_change =
  let ops_plus_capex = Series.Period.sum ~label:"Ops + Capex" [ lazy cf_operations; lazy capex ] in
  Series.Period.sum ~label:"Net Cash Change" [ lazy ops_plus_capex; lazy cf_financing ]

(* --- Balance Sheet ----------------------------------------------------------- *)

(* Cash: starts at $1,000, accumulates net cash change. *)
(* TODO: Update *)
(* let cash = Series.Point.accum ~label:"Cash" ~start_date ~initial_value:1000.0 (lazy net_cash_change) *)
let cash = Series.Point.const ~label:"Cash" 1000.0

(* Common Stock: constant $5,000. *)
let common_stock = Series.Point.const ~label:"Common Stock" 5000.0

(* Retained Earnings: starts at $6,000 (initial assets - common stock), accumulates net income. *)
let retained_earnings =
  (* TODO: Update *)
  (* Series.Point.accum ~label:"Retained Earnings" ~start_date ~initial_value:6000.0 (lazy net_income) *)
  Series.Point.const ~label:"Retained Earnings" 6000.0

(* Total Assets: Cash + PPE Net. *)
let total_assets = Series.Point.sum ~label:"Total Assets" (lazy cash) (lazy ppe_net)

(* Total Equity: Common Stock + Retained Earnings. *)
let total_equity =
  Series.Point.sum ~label:"Total Equity" (lazy common_stock) (lazy retained_earnings)

(* Balance Check: Total Assets - Total Equity (should be zero). *)
let balance_check = Series.Point.sub ~label:"Balance Check" (lazy total_assets) (lazy total_equity)

(* --- Output ----------------------------------------------------------------- *)

let num_periods =
  let n = ref 4 in
  Arg.parse
    [
      ("-n", Arg.Set_int n, "Number of periods to output");
      ("--num-periods", Arg.Set_int n, "Number of periods to output");
    ]
    (fun _ -> ())
    "Usage: main [-n <int>]";
  !n

let query_periods = List.of_seq (Seq.take num_periods months)

let () =
  let open Series.Stmt in
  let stmt =
    group
      [
        group
          [
            period_total net_income
              [
                period_total gross_profit [ period_line revenue; period_line cogs ];
                period_line opex;
                period_line depreciation;
                period_line tax;
              ];
          ];
        group
          [
            period_total net_cash_change
              [ period_line cf_operations; period_line capex; period_line cf_financing ];
          ];
        point_total balance_check
          [
            point_total total_assets [ point_line cash; point_line ppe_net ];
            point_total total_equity [ point_line common_stock; point_line retained_earnings ];
          ];
      ]
  in
  print_string (pp stmt query_periods)
