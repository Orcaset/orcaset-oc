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
let sp = Series.Spans.pack
let pt = Series.Points.pack

let rec seq_take n seq () =
  if n <= 0 then Seq.Nil
  else match seq () with Seq.Nil -> Seq.Nil | Seq.Cons (x, rest) -> Seq.Cons (x, seq_take (n - 1) rest)

(* ----- Model ----- *)

(* Income Statement *)
let revenue : [ `Revenue ] Series.Spans.t =
  Series.Spans.unfold_rec ~label:"Revenue" ~agg:sum_agg
    ~init:(Period.make start_date (Date.shift eomonth_offset start_date))
    ~cells:(fun ~self period ->
      let formula =
        if Date.equal (Period.start period) start_date then
          Series.Formula.pure (Some initial_revenue)
        else
          let open Series.Formula in
          let+ prior_revenue = span_query self ~period:(Period.prev eomonth_lookback period) in
          Option.map
            (fun prior_revenue ->
              let yf = Yf.act_360 (Period.start period) (Period.end_ period) in
              prior_revenue *. (1. +. (annual_revenue_growth *. yf)))
            prior_revenue
      in
      Some (Series.Spans.cell ~period ~split:Split.daily formula, Period.next eomonth_offset period))
    ()

let make_cost_of_revenue (revenue : [ `Revenue ] Series.Spans.t) :
    [ `Cost_of_revenue ] Series.Spans.t =
  Series.Spans.scale ~label:"Cost of revenue" cost_of_revenue_ratio revenue

let make_gross_profit (revenue : [ `Revenue ] Series.Spans.t)
    (cost_of_revenue : [ `Cost_of_revenue ] Series.Spans.t) : [ `Gross_profit ] Series.Spans.t =
  Series.Spans.sum ~label:"Gross profit" ~agg:sum_agg [ sp revenue; sp cost_of_revenue ]

let cost_of_revenue = make_cost_of_revenue revenue
let gross_profit = make_gross_profit revenue cost_of_revenue

let opex : [ `Operating_expenses ] Series.Spans.t =
  Series.Spans.unfold ~label:"Operating expenses" ~agg:sum_agg
    ~init:(Period.make start_date (Date.shift eomonth_offset start_date))
    ~cells:(fun period ->
      let next_period = Period.next eomonth_offset period in
      Some
        ( Series.Spans.cell ~period ~split:Split.daily (Series.Formula.pure (Some monthly_opex)),
          next_period ))
    ()

let make_capex (revenue : [ `Revenue ] Series.Spans.t) : [ `Capital_expenditures ] Series.Spans.t =
  Series.Spans.scale ~label:"Capital expenditures" capex_pct_revenue revenue

let capex = make_capex revenue

let rec lazy_depreciation : [ `Depreciation ] Series.Spans.t Lazy.t =
  lazy (make_depreciation lazy_ppe_net)

and lazy_ppe_change : [ `Ppe_change ] Series.Spans.t Lazy.t =
  lazy (make_ppe_change (make_capitalized_capex capex) (Lazy.force lazy_depreciation))

and lazy_ppe_net : [ `Ppe_net ] Series.Points.t Lazy.t =
  lazy (make_ppe_net (Lazy.force lazy_ppe_change))

and make_depreciation (ppe_net : [ `Ppe_net ] Series.Points.t Lazy.t) :
    [ `Depreciation ] Series.Spans.t =
  Series.Spans.unfold ~label:"Depreciation" ~agg:sum_agg
    ~init:(Period.make start_date (Date.shift eomonth_offset start_date))
    ~cells:(fun period ->
      let formula =
        let open Series.Formula in
        let+ ppe_net = point_query (Lazy.force ppe_net) ~date:(Period.start period) in
        Option.map (fun ppe_net -> -.ppe_net *. monthly_depreciation_rate) ppe_net
      in
      let next_period = Period.next eomonth_offset period in
      Some (Series.Spans.cell ~period ~split:Split.daily formula, next_period))
    ()

and make_capitalized_capex (capex : [ `Capital_expenditures ] Series.Spans.t) :
    [ `Capitalized_capex ] Series.Spans.t =
  Series.Spans.scale (-1.0) capex

and make_ppe_change (capitalized_capex : [ `Capitalized_capex ] Series.Spans.t)
    (depreciation : [ `Depreciation ] Series.Spans.t) : [ `Ppe_change ] Series.Spans.t =
  Series.Spans.sum ~label:"PPE change" ~agg:sum_agg [ sp capitalized_capex; sp depreciation ]

and make_ppe_net (ppe_change : [ `Ppe_change ] Series.Spans.t) : [ `Ppe_net ] Series.Points.t =
  Series.Points.accum ~label:"PPE net" ~init:initial_ppe ppe_change

let depreciation : [ `Depreciation ] Series.Spans.t = Lazy.force lazy_depreciation
let ppe_net : [ `Ppe_net ] Series.Points.t = Lazy.force lazy_ppe_net

let make_ebit (gross_profit : [ `Gross_profit ] Series.Spans.t)
    (opex : [ `Operating_expenses ] Series.Spans.t)
    (depreciation : [ `Depreciation ] Series.Spans.t) : [ `Ebit ] Series.Spans.t =
  Series.Spans.sum ~label:"EBIT" ~agg:sum_agg [ sp gross_profit; sp opex; sp depreciation ]

let make_income_tax (ebit : [ `Ebit ] Series.Spans.t) : [ `Income_tax ] Series.Spans.t =
  Series.Spans.scale ~label:"Income tax" income_tax_rate ebit

let make_net_income (ebit : [ `Ebit ] Series.Spans.t) (income_tax : [ `Income_tax ] Series.Spans.t)
    : [ `Net_income ] Series.Spans.t =
  Series.Spans.sum ~label:"Net income" ~agg:sum_agg [ sp ebit; sp income_tax ]

let ebit = make_ebit gross_profit opex depreciation
let income_tax = make_income_tax ebit
let net_income = make_net_income ebit income_tax

(* Cash Flow Statement *)

let make_depreciation_add_back (depreciation : [ `Depreciation ] Series.Spans.t) :
    [ `Depreciation_add_back ] Series.Spans.t =
  Series.Spans.scale ~label:"Depreciation add back" (-1.0) depreciation

let make_operating_cf (net_income : [ `Net_income ] Series.Spans.t)
    (depreciation_add_back : [ `Depreciation_add_back ] Series.Spans.t) :
    [ `Operating_cash_flow ] Series.Spans.t =
  Series.Spans.sum ~label:"Operating cash flow" ~agg:sum_agg
    [ sp net_income; sp depreciation_add_back ]

let depreciation_add_back = make_depreciation_add_back depreciation
let operating_cf = make_operating_cf net_income depreciation_add_back
let investing_cf : [ `Capital_expenditures ] Series.Spans.t = capex

let cf_financing : [ `Cash_flow_from_financing ] Series.Spans.t =
  Series.Spans.const ~label:"Cash flow from financing" ~split:Split.daily ~agg:sum_agg
    ~period:Period.unbounded 0.0

let make_total_cf (operating_cf : [ `Operating_cash_flow ] Series.Spans.t)
    (investing_cf : [ `Capital_expenditures ] Series.Spans.t)
    (cf_financing : [ `Cash_flow_from_financing ] Series.Spans.t) :
    [ `Total_cash_flow ] Series.Spans.t =
  Series.Spans.sum ~label:"Total cash flow" ~agg:sum_agg
    [ sp operating_cf; sp investing_cf; sp cf_financing ]

let total_cf = make_total_cf operating_cf investing_cf cf_financing

(* Balance Sheet *)

let make_cash (total_cf : [ `Total_cash_flow ] Series.Spans.t) : [ `Cash ] Series.Points.t =
  Series.Points.accum ~label:"Cash" ~init:initial_cash total_cf

let make_total_assets (cash : [ `Cash ] Series.Points.t) (ppe_net : [ `Ppe_net ] Series.Points.t) :
    [ `Total_assets ] Series.Points.t =
  Series.Points.sum ~label:"Total assets" [ pt cash; pt ppe_net ]

let cash = make_cash total_cf
let total_assets = make_total_assets cash ppe_net

let common_stock : [ `Common_stock ] Series.Points.t =
  Series.Points.const ~label:"Common stock" ~period:Period.unbounded initial_common_stock

let make_retained_earnings (net_income : [ `Net_income ] Series.Spans.t) :
    [ `Retained_earnings ] Series.Points.t =
  Series.Points.accum ~label:"Retained earnings" ~init:initial_retained_earnings net_income

let make_total_equity_liabilities (common_stock : [ `Common_stock ] Series.Points.t)
    (retained_earnings : [ `Retained_earnings ] Series.Points.t) :
    [ `Total_equity_and_liabilities ] Series.Points.t =
  Series.Points.sum ~label:"Total equity and liabilities" [ pt common_stock; pt retained_earnings ]

let make_bs_check (total_assets : [ `Total_assets ] Series.Points.t)
    (total_equity_liabilities : [ `Total_equity_and_liabilities ] Series.Points.t) :
    [ `Balance_sheet_check ] Series.Points.t =
  Series.Points.sub ~label:"Balance sheet check" total_assets total_equity_liabilities

let retained_earnings = make_retained_earnings net_income
let total_equity_liabilities = make_total_equity_liabilities common_stock retained_earnings
let bs_check = make_bs_check total_assets total_equity_liabilities

(* ----- Output ----- *)
let income_stmt =
  Stmt.span_total net_income
    [
      Stmt.span_total ebit
        [
          Stmt.span_total gross_profit [ Stmt.span_line revenue; Stmt.span_line cost_of_revenue ];
          Stmt.span_line opex;
          Stmt.span_line depreciation;
        ];
      Stmt.span_line income_tax;
    ]

let cash_flow_stmt =
  Stmt.span_total total_cf
    [
      Stmt.span_total operating_cf
        [ Stmt.span_line net_income; Stmt.span_line depreciation_add_back ];
      Stmt.span_line investing_cf;
      Stmt.span_line cf_financing;
    ]

let balance_sheet_stmt =
  Stmt.group
    [
      Stmt.point_total total_assets [ Stmt.point_line cash; Stmt.point_line ppe_net ];
      Stmt.point_total total_equity_liabilities
        [ Stmt.point_line common_stock; Stmt.point_line retained_earnings ];
      Stmt.point_line bs_check;
    ]

let total_stmt = Stmt.group [ income_stmt; cash_flow_stmt; balance_sheet_stmt ]

let () =
  let num_periods = 6 in
  let query_offset = Offset.make ~months:1 ~month_end:true () in
  let periods =
    Period.make_seq ~start:start_date ~offset:query_offset |> seq_take num_periods |> List.of_seq
  in
  let resolved = Stmt.eval_periods periods total_stmt in
  Printf.printf "\n%s\n\n" (Stmt.fixed_width resolved)
