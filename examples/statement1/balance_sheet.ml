open Orcaset

type t = {
  cash : Balance_series.t;
  ppe_net : Balance_series.t;
  total_assets : Balance_series.t;
  common_stock : Balance_series.t;
  retained_earnings : Balance_series.t;
  total_liabilities_equity : Balance_series.t;
  balance_check : Balance_series.t;
}

let make ~start_date ~initial_cash ~initial_ppe ~common_stock_amount ~cash_flow_statement_lazy
    ~income_statement_lazy =
  let ppe_change_seq_lazy =
    lazy
      (let cash_flow_stmt = Lazy.force cash_flow_statement_lazy in
       let income_stmt = Lazy.force income_statement_lazy in
       let capex_seq = cash_flow_stmt.Cash_flow_statement.capex in
       let depreciation_seq = income_stmt.Income_statement.depreciation in
       Accrual.sub_seq depreciation_seq capex_seq |> Seq.memoize)
  in
  let ppe_net =
    Balance_series.from_accruals ~initial_date:start_date
      ~initial_value:(lazy initial_ppe)
      ppe_change_seq_lazy
  in
  let cash =
    Balance_series.from_flow ~initial_date:start_date
      ~initial_value:(lazy initial_cash)
      ~sum_between:(fun ~start_date ~end_date ->
        let cash_flow_stmt = Lazy.force cash_flow_statement_lazy in
        Accrual.accrue start_date end_date cash_flow_stmt.Cash_flow_statement.net_cash_change)
  in
  let total_assets = Balance_series.sum cash ppe_net in
  let common_stock = Balance_series.constant (lazy common_stock_amount) in
  let initial_re = initial_cash +. initial_ppe -. common_stock_amount in
  let retained_earnings =
    Balance_series.from_flow ~initial_date:start_date
      ~initial_value:(lazy initial_re)
      ~sum_between:(fun ~start_date ~end_date ->
        let income_stmt = Lazy.force income_statement_lazy in
        Accrual.accrue start_date end_date income_stmt.Income_statement.net_income)
  in
  let total_liabilities_equity = Balance_series.sum common_stock retained_earnings in
  let balance_check = Balance_series.sub total_assets total_liabilities_equity in
  {
    cash;
    ppe_net;
    total_assets;
    common_stock;
    retained_earnings;
    total_liabilities_equity;
    balance_check;
  }
