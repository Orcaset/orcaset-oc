open Orcaset

type t = {
  net_income_add_back : Accrual.t Seq.t;
  depreciation_add_back : Accrual.t Seq.t;
  cf_ops : Accrual.t Seq.t;
  capex : Accrual.t Seq.t;
  cf_invest : Accrual.t Seq.t;
  cf_finance : Accrual.t Seq.t;
  net_cash_change : Accrual.t Seq.t;
}

let make ~start_date ~freq ~income_stmt_lazy ~capex_pct =
  let net_income_add_back =
    lazy (Lazy.force income_stmt_lazy).Income_statement.net_income |> Lazy.force |> Seq.memoize
  in

  let depreciation_add_back =
    let depr = lazy (Lazy.force income_stmt_lazy).Income_statement.depreciation |> Lazy.force in
    Seq.map (fun acc -> Accrual.map (fun v -> -.v) acc) depr |> Seq.memoize
  in

  let cf_ops = Accrual.sum_seq net_income_add_back depreciation_add_back |> Seq.memoize in

  let capex =
    let revenue =
      lazy (Lazy.force income_stmt_lazy).Income_statement.gross_profit.Gross_profit.revenue
      |> Lazy.force
    in
    Seq.map (fun rev -> Accrual.map (fun v -> v *. capex_pct) rev) revenue |> Seq.memoize
  in

  let cf_invest = capex in

  let cf_finance =
    Period.make_seq ~start_date ~offset:freq
    |> Seq.map (fun period ->
        Accrual.make ~period ~value:(lazy 0.0) ~split_fn:Accrual.default_split_fn)
    |> Seq.memoize
  in

  let net_cash_change =
    Accrual.sum_seq (Accrual.sum_seq cf_ops cf_invest) cf_finance |> Seq.memoize
  in

  {
    net_income_add_back;
    depreciation_add_back;
    cf_ops;
    capex;
    cf_invest;
    cf_finance;
    net_cash_change;
  }
