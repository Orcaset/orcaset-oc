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

let make ~income_statement_lazy ~capex_pct =
  let income_stmt = Lazy.force income_statement_lazy in
  let net_income_add_back = income_stmt.Income_statement.net_income in
  let depreciation_add_back =
    Seq.map (fun acc -> Accrual.map (fun v -> -.v) acc) income_stmt.Income_statement.depreciation
    |> Seq.memoize
  in
  let cf_ops = Accrual.sum_seq net_income_add_back depreciation_add_back |> Seq.memoize in
  let capex =
    Seq.map
      (fun acc -> Accrual.map (fun v -> v *. -.capex_pct) acc)
      income_stmt.Income_statement.gross_profit.revenue
    |> Seq.memoize
  in
  let cf_invest = capex in
  let cf_finance =
    Seq.map
      (fun acc ->
        Accrual.make ~period:(Accrual.period acc)
          ~value:(lazy 0.0)
          ~split_fn:Accrual.default_split_fn)
      income_stmt.Income_statement.gross_profit.revenue
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
