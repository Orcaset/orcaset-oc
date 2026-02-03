(* Capital Expenditures Module for Commercial Real Estate Pro Forma
   
   CapEx items for property maintenance and improvements:
   - Capital reserves (% of EGI set aside for future repairs)
   - Tenant improvements (TI allowances)
   - Leasing commissions
*)

module Accrual = Orcaset.Accrual
module Period = Orcaset.Period

type t = {
  capital_reserves : Accrual.t Seq.t;
  tenant_improvements : Accrual.t Seq.t;
  leasing_commissions : Accrual.t Seq.t;
  total : Accrual.t Seq.t;
}

let make ~egi_lazy ~start_date ~reserve_pct ~ti_per_sf_annual ~building_sf ~commission_pct ~freq =
  (* Capital reserves: Percentage of EGI set aside for future capex *)
  let capital_reserves =
    let periods = Period.make_seq ~start_date ~offset:freq in
    Seq.map
      (fun period ->
        let value =
          lazy
            (let egi =
               Accrual.accrue period.Period.start_date period.end_date (Lazy.force egi_lazy)
             in
             -.egi *. reserve_pct)
        in
        Accrual.make ~period ~value ~split_fn:Accrual.default_split_fn)
      periods
    |> Seq.memoize
  in
  (* Tenant improvements: Fixed annual amount based on building SF, spread monthly *)
  let ti_annual = ti_per_sf_annual *. building_sf in
  let tenant_improvements =
    let periods = Period.make_seq ~start_date ~offset:freq in
    Seq.map
      (fun period ->
        Accrual.make ~period ~value:(lazy (-.ti_annual /. 12.0)) ~split_fn:Accrual.default_split_fn)
      periods
    |> Seq.memoize
  in
  (* Leasing commissions: Percentage of EGI (assumes steady lease-up activity) *)
  let leasing_commissions =
    let periods = Period.make_seq ~start_date ~offset:freq in
    Seq.map
      (fun period ->
        let value =
          lazy
            (let egi =
               Accrual.accrue period.Period.start_date period.end_date (Lazy.force egi_lazy)
             in
             -.egi *. commission_pct)
        in
        Accrual.make ~period ~value ~split_fn:Accrual.default_split_fn)
      periods
    |> Seq.memoize
  in
  let total =
    Accrual.sum_seq (Accrual.sum_seq capital_reserves tenant_improvements) leasing_commissions
    |> Seq.memoize
  in
  { capital_reserves; tenant_improvements; leasing_commissions; total }
