open Orcaset

(* ----- Assumptions ----- *)

let start_date = Date.make 2025 12 31
let eomonth_offset = Offset.make ~months:1 ~month_end:true ()
let eomonth_lookback = Offset.make ~months:(-1) ~month_end:true ()
let initial_revenue = 1_000.0
let annual_revenue_growth = 0.20
let cost_of_revenue_ratio = -0.30
let monthly_opex = -200.0
let monthly_depreciation_rate = 0.10 /. 12.0
let income_tax_rate = -0.20
let capex_pct_revenue = -0.05
let initial_cash = 1_000.0
let initial_ppe = 10_000.0
let initial_common_stock = 5_000.0
let initial_retained_earnings = 6_000.0
let sum_agg = Series.Agg.sum

(* ----- Model ----- *)

(* Income Statement *)
let revenue =
  Series.Spans.unfold_rec ~label:"Revenue" ~agg:sum_agg
    ~deps:(fun self -> Series.Deps.span_dep self)
    ~init:(Period.make start_date (Date.shift eomonth_offset start_date))
    ~cells:(fun read_revenue period ->
      let formula =
        if Date.equal (Period.start period) start_date then
          Series.Formula.pure (Some initial_revenue)
        else
          let open Series.Formula in
          let+ prior_revenue = read_revenue ~period:(Period.prev eomonth_lookback period) in
          Option.map
            (fun prior_revenue ->
              let yf = Yf.act_360 (Period.start period) (Period.end_ period) in
              prior_revenue *. (1. +. (annual_revenue_growth *. yf)))
            prior_revenue
      in
      Some (Series.Spans.cell ~period ~split:Split.daily formula, Period.next eomonth_offset period))
    ()

let cost_of_revenue = Series.Spans.scale ~label:"Cost of revenue" cost_of_revenue_ratio revenue
let gross_profit = Series.Spans.sum ~label:"Gross profit" ~agg:sum_agg [ revenue; cost_of_revenue ]

let opex =
  Series.Spans.unfold ~label:"Operating expenses" ~agg:sum_agg
    ~deps:(fun () -> Series.Deps.none)
    ~init:(Period.make start_date (Date.shift eomonth_offset start_date))
    ~cells:(fun () period ->
      let next_period = Period.next eomonth_offset period in
      Some
        ( Series.Spans.cell ~period ~split:Split.daily (Series.Formula.pure (Some monthly_opex)),
          next_period ))
    ()

let capex = Series.Spans.scale ~label:"Capital expenditures" capex_pct_revenue revenue

let rec lazy_depreciation =
  lazy
    (Series.Spans.unfold ~label:"Depreciation" ~agg:sum_agg
       ~deps:(fun () -> Series.Deps.point_dep (Lazy.force lazy_ppe_net))
       ~init:(Period.make start_date (Date.shift eomonth_offset start_date))
       ~cells:(fun read_ppe_net period ->
         let formula =
           let open Series.Formula in
           let+ ppe_net = read_ppe_net ~date:(Period.start period) in
           Option.map (fun ppe_net -> -.ppe_net *. monthly_depreciation_rate) ppe_net
         in
         let next_period = Period.next eomonth_offset period in
         Some (Series.Spans.cell ~period ~split:Split.daily formula, next_period))
       ())

and lazy_ppe_change =
  lazy
    (let capitalized_capex = Series.Spans.scale (-1.0) capex in
     Series.Spans.sum ~label:"PPE change" ~agg:sum_agg
       [ capitalized_capex; Lazy.force lazy_depreciation ])

and lazy_ppe_net =
  lazy (Series.Points.accum ~label:"PPE net" ~init:initial_ppe (Lazy.force lazy_ppe_change))

let depreciation = Lazy.force lazy_depreciation
let ppe_net = Lazy.force lazy_ppe_net
let ebit = Series.Spans.sum ~label:"EBIT" ~agg:sum_agg [ gross_profit; opex; depreciation ]
let income_tax = Series.Spans.scale ~label:"Income tax" income_tax_rate ebit
let net_income = Series.Spans.sum ~label:"Net income" ~agg:sum_agg [ ebit; income_tax ]

(* Cash Flow Statement *)

let depreciation_add_back = Series.Spans.scale ~label:"Depreciation add back" (-1.0) depreciation

let operating_cf =
  Series.Spans.sum ~label:"Operating cash flow" ~agg:sum_agg [ net_income; depreciation_add_back ]

let investing_cf = capex

let cf_financing =
  Series.Spans.const ~label:"Cash flow from financing" ~split:Split.daily ~agg:sum_agg
    ~period:Period.unbounded 0.0

let total_cf =
  Series.Spans.sum ~label:"Total cash flow" ~agg:sum_agg
    [ operating_cf; investing_cf; cf_financing ]

(* Balance Sheet *)

let cash = Series.Points.accum ~label:"Cash" ~init:initial_cash total_cf
let total_assets = Series.Points.sum ~label:"Total assets" [ cash; ppe_net ]

let common_stock =
  Series.Points.const ~label:"Common stock" ~period:Period.unbounded initial_common_stock

let retained_earnings =
  Series.Points.accum ~label:"Retained earnings" ~init:initial_retained_earnings net_income

let total_equity_liabilities =
  Series.Points.sum ~label:"Total equity and liabilities" [ common_stock; retained_earnings ]

let bs_check = Series.Points.sub ~label:"Balance sheet check" total_assets total_equity_liabilities

(* ----- Output ----- *)
let income_stmt =
  Stmt.span_total net_income
    [
      Stmt.span_total gross_profit (Stmt.span_lines [ revenue; cost_of_revenue ]);
      Stmt.span_line opex;
      Stmt.span_line depreciation;
      Stmt.span_line income_tax;
      Stmt.span_line net_income;
    ]

let cash_flow_stmt =
  Stmt.span_total total_cf (Stmt.span_lines [ operating_cf; investing_cf; cf_financing; total_cf ])

let balance_sheet_stmt =
  Stmt.group
    [
      Stmt.point_total total_assets (Stmt.point_lines [ cash; ppe_net ]);
      Stmt.point_total total_equity_liabilities
        (Stmt.point_lines [ common_stock; retained_earnings ]);
      Stmt.point_line bs_check;
    ]

let total_stmt = Stmt.group [ income_stmt; cash_flow_stmt; balance_sheet_stmt ]

let () =
  let num_periods = 6 in
  let query_offset = Offset.make ~months:1 ~month_end:true () in
  let periods =
    Period.make_seq ~start:start_date ~offset:query_offset |> Seq.take num_periods |> List.of_seq
  in
  let resolved = Stmt.eval_periods periods total_stmt in
  Printf.printf "\n%s\n\n" (Stmt.fixed_width resolved)
