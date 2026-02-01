open Orcaset

type t = {
  cash : BalanceSeries.t;
  ppe_net : BalanceSeries.t;
  total_assets : BalanceSeries.t;
  common_stock : BalanceSeries.t;
  retained_earnings : BalanceSeries.t;
  total_liabilities_equity : BalanceSeries.t;
  balance_check : BalanceSeries.t;
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
    BalanceSeries.from_accruals ~initial_date:start_date
      ~initial_amount:(lazy initial_ppe)
      ppe_change_seq_lazy
  in
  let cash =
    BalanceSeries.from_flow ~initial_date:start_date
      ~initial_amount:(lazy initial_cash)
      ~sum_between:(fun ~start_date ~end_date ->
        let cash_flow_stmt = Lazy.force cash_flow_statement_lazy in
        Accrual.accrue start_date end_date cash_flow_stmt.Cash_flow_statement.net_cash_change)
  in
  let total_assets = BalanceSeries.sum cash ppe_net in
  let common_stock = BalanceSeries.constant (lazy common_stock_amount) in
  let initial_re = initial_cash +. initial_ppe -. common_stock_amount in
  let retained_earnings =
    BalanceSeries.from_flow ~initial_date:start_date
      ~initial_amount:(lazy initial_re)
      ~sum_between:(fun ~start_date ~end_date ->
        let income_stmt = Lazy.force income_statement_lazy in
        Accrual.accrue start_date end_date income_stmt.Income_statement.net_income)
  in
  let total_liabilities_equity = BalanceSeries.sum common_stock retained_earnings in
  let balance_check = BalanceSeries.sub total_assets total_liabilities_equity in
  {
    cash;
    ppe_net;
    total_assets;
    common_stock;
    retained_earnings;
    total_liabilities_equity;
    balance_check;
  }
