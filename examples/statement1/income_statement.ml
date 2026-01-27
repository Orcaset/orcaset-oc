open Orcaset

type t = {
  gross_profit : Gross_profit.t;
  opex : Accrual.t Seq.t;
  depreciation : Accrual.t Seq.t;
  tax : Accrual.t Seq.t;
  net_income : Accrual.t Seq.t;
}

let make ~start_date ~freq ~yf ~revenue_initial ~revenue_growth ~cogs_pct ~opex_monthly ~tax_rate
    ~depreciation_rate ~ppe_net_lazy =
  let gross_profit =
    Gross_profit.make ~start_date ~freq ~yf ~revenue_initial ~revenue_growth ~cogs_pct
  in

  let opex =
    Period.make_seq ~start_date ~offset:freq
    |> Seq.map (fun period ->
        Accrual.make ~period ~value:(lazy opex_monthly) ~split_fn:Accrual.default_split_fn)
    |> Seq.memoize
  in

  let depreciation =
    let periods = Period.make_seq ~start_date ~offset:freq in
    Seq.map
      (fun period ->
        let depreciation_value =
          lazy
            (let ppe_net = Lazy.force ppe_net_lazy in
             let beginning_ppe_net = Balance.on ppe_net period.Period.start_date in
             -.(Lazy.force beginning_ppe_net.Balance.amount *. (depreciation_rate /. 12.)))
        in
        Accrual.make ~period ~value:depreciation_value ~split_fn:Accrual.default_split_fn)
      periods
    |> Seq.memoize
  in

  let pretax_income =
    Accrual.sum_seq (Accrual.sum_seq gross_profit.total opex) depreciation |> Seq.memoize
  in

  let tax =
    Seq.map
      (fun acc -> Accrual.map (fun v -> if v > 0. then v *. -.tax_rate else 0.) acc)
      pretax_income
    |> Seq.memoize
  in

  let net_income = Accrual.sum_seq pretax_income tax |> Seq.memoize in

  { gross_profit; opex; depreciation; tax; net_income }
