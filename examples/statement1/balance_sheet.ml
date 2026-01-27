open Orcaset

type t = {
  cash : Balance.t Seq.t;
  ppe_net : Balance.t Seq.t;
  total_assets : Balance.t Seq.t;
  common_stock : Balance.t Seq.t;
  retained_earnings : Balance.t Seq.t;
  total_liabilities_equity : Balance.t Seq.t;
  balance_check : Balance.t Seq.t;
}

let make ~start_date ~freq ~initial_cash ~initial_ppe ~common_stock_amount
    ~initial_retained_earnings ~cash_flow_lazy ~income_stmt_lazy =
  let periods = Period.make_seq ~start_date ~offset:freq in

  let cf = lazy (Lazy.force cash_flow_lazy) in
  let inc = lazy (Lazy.force income_stmt_lazy) in
  let net_cash_change_memo = lazy (Seq.memoize (Lazy.force cf).Cash_flow.net_cash_change) in
  let capex_memo = lazy (Seq.memoize (Lazy.force cf).Cash_flow.capex) in
  let depreciation_memo = lazy (Seq.memoize (Lazy.force inc).Income_statement.depreciation) in
  let net_income_memo = lazy (Seq.memoize (Lazy.force inc).Income_statement.net_income) in

  let cash =
    Seq.unfold
      (fun (prev_cash, cf_seq, periods_seq) ->
        match periods_seq () with
        | Seq.Nil -> None
        | Seq.Cons (period, rest_periods) ->
            let date = period.Period.end_date in
            let cf_change =
              Accrual.accrue period.Period.start_date period.Period.end_date
                (Lazy.force net_cash_change_memo)
            in
            let new_cash = prev_cash +. cf_change in
            let balance = Balance.make ~date ~amount:(lazy new_cash) in
            Some (balance, (new_cash, cf_seq, rest_periods)))
      (initial_cash, Seq.empty, periods)
    |> Seq.memoize
  in

  let ppe_net =
    let initial_balance = Balance.make ~date:start_date ~amount:(lazy initial_ppe) in
    let rest =
      Seq.unfold
        (fun (prev_ppe, periods_seq) ->
          match periods_seq () with
          | Seq.Nil -> None
          | Seq.Cons (period, rest_periods) ->
              let date = period.Period.end_date in
              let capex_change =
                Accrual.accrue period.Period.start_date period.Period.end_date
                  (Lazy.force capex_memo)
              in
              let depreciation_change =
                Accrual.accrue period.Period.start_date period.Period.end_date
                  (Lazy.force depreciation_memo)
              in
              let new_ppe = prev_ppe -. capex_change +. depreciation_change in
              let balance = Balance.make ~date ~amount:(lazy new_ppe) in
              Some (balance, (new_ppe, rest_periods)))
        (initial_ppe, periods)
    in
    Seq.cons initial_balance rest |> Seq.memoize
  in

  let total_assets = Balance.combine_seq ( +. ) cash ppe_net |> Seq.memoize in

  let common_stock =
    Seq.unfold
      (fun periods_seq ->
        match periods_seq () with
        | Seq.Nil -> None
        | Seq.Cons (period, rest_periods) ->
            let date = period.Period.end_date in
            let balance = Balance.make ~date ~amount:(lazy common_stock_amount) in
            Some (balance, rest_periods))
      periods
    |> Seq.memoize
  in

  let retained_earnings =
    Seq.unfold
      (fun (prev_re, periods_seq) ->
        match periods_seq () with
        | Seq.Nil -> None
        | Seq.Cons (period, rest_periods) ->
            let date = period.Period.end_date in
            let net_income_change =
              Accrual.accrue period.Period.start_date period.Period.end_date
                (Lazy.force net_income_memo)
            in
            let new_re = prev_re +. net_income_change in
            let balance = Balance.make ~date ~amount:(lazy new_re) in
            Some (balance, (new_re, rest_periods)))
      (initial_retained_earnings, periods)
    |> Seq.memoize
  in

  let total_liabilities_equity =
    Balance.combine_seq ( +. ) common_stock retained_earnings |> Seq.memoize
  in

  let balance_check =
    Balance.combine_seq ( -. ) total_assets total_liabilities_equity |> Seq.memoize
  in

  {
    cash;
    ppe_net;
    total_assets;
    common_stock;
    retained_earnings;
    total_liabilities_equity;
    balance_check;
  }
