open Orcaset

type t = { revenue : Accrual.t Seq.t; cogs : Accrual.t Seq.t; total : Accrual.t Seq.t }

let make ~start_date ~freq ~yf ~revenue_initial ~revenue_growth ~cogs_pct =
  let revenue =
    Accrual.const_annual_growth_seq ~start_date ~initial_value:revenue_initial ~rate:revenue_growth
      ~freq ~yf
    |> Seq.memoize
  in

  let cogs = Seq.map (fun rev -> Accrual.map (fun v -> v *. cogs_pct) rev) revenue |> Seq.memoize in

  let total = Accrual.sum_seq revenue cogs |> Seq.memoize in

  { revenue; cogs; total }
