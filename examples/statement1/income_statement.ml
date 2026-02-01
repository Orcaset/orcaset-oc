open Orcaset

module Gross_profit = struct
  type t = { revenue : Accrual.t Seq.t; cogs : Accrual.t Seq.t; total : Accrual.t Seq.t }

  let make ~start_date ~freq ~yf ~initial_revenue ~revenue_growth_rate ~cogs_pct =
    let revenue =
      Accrual.const_annual_growth_seq ~start_date ~initial_value:initial_revenue
        ~rate:revenue_growth_rate ~freq ~yf
      |> Seq.memoize
    in
    let cogs =
      Seq.map (fun acc -> Accrual.map (fun v -> v *. -.cogs_pct) acc) revenue |> Seq.memoize
    in
    let total = Accrual.sum_seq revenue cogs |> Seq.memoize in
    { revenue; cogs; total }
end

type t = {
  gross_profit : Gross_profit.t;
  opex : Accrual.t Seq.t;
  depreciation : Accrual.t Seq.t;
  ebt : Accrual.t Seq.t;
  tax : Accrual.t Seq.t;
  net_income : Accrual.t Seq.t;
}

let make ~start_date ~freq ~yf ~initial_revenue ~revenue_growth_rate ~cogs_pct ~opex_monthly
    ~tax_rate ~depreciation_rate ~initial_ppe ~capex_seq_lazy =
  let periods = Period.make_seq ~start_date ~offset:freq |> Seq.memoize in
  let gross_profit =
    Gross_profit.make ~start_date ~freq ~yf ~initial_revenue ~revenue_growth_rate ~cogs_pct
  in
  let opex =
    Seq.map
      (fun period ->
        Accrual.make ~period ~value:(lazy (-.opex_monthly)) ~split_fn:Accrual.default_split_fn)
      periods
    |> Seq.memoize
  in
  let depreciation =
    Seq.unfold
      (fun (periods_tail, capex_tail, prior_ppe_net) ->
        match (periods_tail (), (Lazy.force capex_tail) ()) with
        | Seq.Nil, _ | _, Seq.Nil -> None
        | Seq.Cons (period, rest_periods), Seq.Cons (capex_acc, rest_capex) ->
            let depr_val = prior_ppe_net *. depreciation_rate /. 12.0 in
            let capex_val = -.Accrual.value capex_acc in
            let ppe_change = capex_val -. depr_val in
            let next_ppe_net = prior_ppe_net +. ppe_change in
            let depr_accrual =
              Accrual.make ~period ~value:(lazy (-.depr_val)) ~split_fn:Accrual.default_split_fn
            in
            Some (depr_accrual, (rest_periods, lazy rest_capex, next_ppe_net)))
      (periods, capex_seq_lazy, initial_ppe)
    |> Seq.memoize
  in
  let ebt = Accrual.sum_seq (Accrual.sum_seq gross_profit.total opex) depreciation |> Seq.memoize in
  let tax = Seq.map (fun acc -> Accrual.map (fun v -> v *. -.tax_rate) acc) ebt |> Seq.memoize in
  let net_income = Accrual.sum_seq ebt tax |> Seq.memoize in
  { gross_profit; opex; depreciation; ebt; tax; net_income }
