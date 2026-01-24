module Accrual = Orcaset.Accrual
module Period = Orcaset.Period

type t = { cogs : Accrual.t Seq.t; admin : Accrual.t Seq.t; total : Accrual.t Seq.t }

let make ~revenue_lazy ~cogs_pct_rev ~admin_first ~admin_rate ~start_date ~freq ~yf =
  let cogs =
    let future_software_rev () =
      let software_rev = (Lazy.force revenue_lazy).Revenue.software in
      Accrual.after start_date software_rev
    in
    Seq.map (fun rev -> Accrual.map (fun v -> v *. cogs_pct_rev) rev) (future_software_rev ())
  in
  let admin =
    Accrual.const_annual_growth_seq ~start_date ~initial_value:admin_first ~rate:admin_rate ~freq
      ~yf
  in
  let total = Accrual.sum_seq cogs admin in
  { cogs; admin; total }
