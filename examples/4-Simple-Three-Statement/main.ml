open Orcaset

(* A three-statement financial model with linked income, cash flow, and balance sheet statements.
   Demonstrates circular dependencies (depreciation <-> PPE) and point series accumulation for balance
   sheet items.

   Output defaults to 4 quarterly periods. Use -n to override the number of displayed periods and -p
   to switch the output periodicity to monthly, quarterly, or yearly.

   Example usage:
   ```
   dune build && dune exec examples/4-Simple-Three-Statement/main.exe -- -n 12 -p monthly
   ``` *)

let start_date = Date.make 2025 12 31
let offset = Offset.make ~months:1 ~month_end:true ()
let revenue_lookback = Offset.make ~months:(-1) ~month_end:true ()
let months = Period.make_seq ~start_date ~offset
let initial_period = Period.make start_date (Date.shift offset start_date)

let accumulate_period_flow ~label flow =
  Series.Point.unfold_seq ~label ~deps:(Series.Point.dep_period flow) ~cells:(fun flow_dep ->
      Seq.map
        (fun period ->
          Series.Point.step ~period
            (Series.Point.Query.period flow_dep ~period ~reduce:Series.Period.reduce_sum)
            Fun.id)
        months)

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
  Series.Period.unfold_seq_self ~label:"Revenue" ~cells:(fun () ->
      Seq.cons
        (Series.Period.const ~period:initial_period (fun () -> 1000.0))
        (generate_cells initial_period))

(* COGS: 30% of revenue. *)
let cogs = Series.Period.map ~label:"COGS" (fun r -> r *. -0.30) (lazy revenue)

(* Gross Profit: Revenue + COGS. *)
let gross_profit = Series.Period.sum ~label:"Gross Profit" [ lazy revenue; lazy cogs ]

(* Opex: constant -$200/month. *)
let opex =
  Series.Period.unfold_seq_self ~label:"Opex" ~cells:(fun () ->
      Seq.map (fun period -> Series.Period.const ~period (fun () -> -200.0)) months)

(* Capex: 5% of revenue (negative = cash outflow). *)
let capex = Series.Period.map ~label:"Capex" (fun r -> r *. -0.05) (lazy revenue)

(* Depreciation and PPE Net: circular dependency.
   Depreciation = prior PPE Net x (10% / 12), as an expense (negative).
   PPE Net starts at $10,000, increases by capex additions, decreases by depreciation. *)
let rec depreciation =
  lazy
    (Series.Period.unfold_seq ~label:"Depreciation" ~deps:(Series.Period.dep_point ppe_net)
       ~cells:(fun ppe_net ->
         Seq.map
           (fun period ->
             Series.Period.step ~period
               (Series.Period.Query.point ppe_net ~date:(Period.start_date period))
               (function Some ppe -> ppe *. (-0.10 /. 12.0) | None -> 0.0))
           months))

and ppe_net =
  lazy
    (let neg_capex = Series.Period.map ~label:"Neg Capex" (fun x -> -.x) (lazy capex) in
     let ppe_change = Series.Period.sum ~label:"PPE Change" [ lazy neg_capex; depreciation ] in
     let opening_ppe = Series.Point.const ~label:"Opening PPE Net" 10000.0 in
     let ppe_rollforward = accumulate_period_flow ~label:"PPE Rollforward" (lazy ppe_change) in
     Series.Point.sum ~label:"PPE Net" (lazy opening_ppe) (lazy ppe_rollforward))

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
  Series.Period.unfold_seq_self ~label:"CF Financing" ~cells:(fun () ->
      Seq.map (fun period -> Series.Period.const ~period (fun () -> 0.0)) months)

(* Net Cash Change: CF Operations + Capex + CF Financing. *)
let net_cash_change =
  let ops_plus_capex = Series.Period.sum ~label:"Ops + Capex" [ lazy cf_operations; lazy capex ] in
  Series.Period.sum ~label:"Net Cash Change" [ lazy ops_plus_capex; lazy cf_financing ]

(* --- Balance Sheet ----------------------------------------------------------- *)

(* Cash: starts at $1,000, accumulates net cash change. *)
let cash =
  let opening_cash = Series.Point.const ~label:"Opening Cash" 1000.0 in
  let cash_rollforward = accumulate_period_flow ~label:"Cash Rollforward" (lazy net_cash_change) in
  Series.Point.sum ~label:"Cash" (lazy opening_cash) (lazy cash_rollforward)

(* Common Stock: constant $5,000. *)
let common_stock = Series.Point.const ~label:"Common Stock" 5000.0

(* Retained Earnings: starts at $6,000 (initial assets - common stock), accumulates net income. *)
let retained_earnings =
  let opening_re = Series.Point.const ~label:"Opening Retained Earnings" 6000.0 in
  let re_rollforward = accumulate_period_flow ~label:"RE Rollforward" (lazy net_income) in
  Series.Point.sum ~label:"Retained Earnings" (lazy opening_re) (lazy re_rollforward)

(* Total Assets: Cash + PPE Net. *)
let total_assets = Series.Point.sum ~label:"Total Assets" (lazy cash) (lazy ppe_net)

(* Total Equity: Common Stock + Retained Earnings. *)
let total_equity =
  Series.Point.sum ~label:"Total Equity" (lazy common_stock) (lazy retained_earnings)

(* Balance Check: Total Assets - Total Equity (should be zero). *)
let balance_check = Series.Point.sub ~label:"Balance Check" (lazy total_assets) (lazy total_equity)

(* --- Output ----------------------------------------------------------------- *)

type periodicity = Monthly | Quarterly | Yearly

let periodicity_of_string = function
  | "monthly" -> Monthly
  | "quarterly" -> Quarterly
  | "yearly" -> Yearly
  | s -> invalid_arg ("Unsupported periodicity: " ^ s)

let offset_of_periodicity = function
  | Monthly -> Offset.make ~months:1 ~month_end:true ()
  | Quarterly -> Offset.make ~months:3 ~month_end:true ()
  | Yearly -> Offset.make ~years:1 ~month_end:true ()

let num_periods, periodicity =
  let n = ref 4 in
  let periodicity = ref Quarterly in
  Arg.parse
    [
      ("-n", Arg.Set_int n, "Number of periods to output");
      ("--num-periods", Arg.Set_int n, "Number of periods to output");
      ( "-p",
        Arg.Symbol
          ([ "monthly"; "quarterly"; "yearly" ], fun s -> periodicity := periodicity_of_string s),
        "Output periodicity: monthly, quarterly, or yearly" );
      ( "--periodicity",
        Arg.Symbol
          ([ "monthly"; "quarterly"; "yearly" ], fun s -> periodicity := periodicity_of_string s),
        "Output periodicity: monthly, quarterly, or yearly" );
    ]
    (fun _ -> ())
    "Usage: main [-n <int>] [-p <monthly|quarterly|yearly>]";
  (!n, !periodicity)

let query_periods =
  let query_offset = offset_of_periodicity periodicity in
  List.of_seq (Seq.take num_periods (Period.make_seq ~start_date ~offset:query_offset))

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
