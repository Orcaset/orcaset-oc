open Orcaset

type t = {
  gross_profit : Gross_profit.t;
  opex : Accrual.t Seq.t;
  depreciation : Accrual.t Seq.t;
  tax : Accrual.t Seq.t;
  net_income : Accrual.t Seq.t;
}

let make ~start_date ~freq ~yf ~revenue_initial ~revenue_growth ~cogs_pct ~opex_monthly ~tax_rate
    ~depreciation_rate ~initial_ppe ~ppe_net_lazy =
  let gross_profit =
    Gross_profit.make ~start_date ~freq ~yf ~revenue_initial ~revenue_growth ~cogs_pct
  in

  let opex =
    Period.make_seq ~start_date ~offset:freq
    |> Seq.map (fun period ->
        Accrual.make ~period ~value:(lazy opex_monthly) ~split_fn:Accrual.default_split_fn)
  in

  let depreciation =
    let periods = Period.make_seq ~start_date ~offset:freq in
    Seq.unfold
      (fun (is_first, periods_seq) ->
        match periods_seq () with
        | Seq.Nil -> None
        | Seq.Cons (period, rest) ->
            let value =
              lazy
                (let beginning_ppe =
                   if is_first then initial_ppe
                   else
                     let ppe_net_seq = Lazy.force ppe_net_lazy in
                     let prev_end = CalendarLib.Date.prev period.Period.start_date `Day in
                     let bal = Balance.on ppe_net_seq prev_end in
                     Lazy.force bal.Balance.amount
                 in
                 beginning_ppe *. depreciation_rate /. -12.0)
            in
            let accrual = Accrual.make ~period ~value ~split_fn:Accrual.default_split_fn in
            Some (accrual, (false, rest)))
      (true, periods)
  in

  let pretax_income = Accrual.sum_seq (Accrual.sum_seq gross_profit.total opex) depreciation in

  let tax =
    Seq.map
      (fun acc -> Accrual.map (fun v -> if v > 0. then v *. -.tax_rate else 0.) acc)
      pretax_income
  in

  let net_income = Accrual.sum_seq pretax_income tax in

  { gross_profit; opex; depreciation; tax; net_income }
