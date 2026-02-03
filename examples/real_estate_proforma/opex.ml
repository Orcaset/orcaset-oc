(* Operating Expenses Module for Commercial Real Estate Pro Forma
   
   Operating expenses for a small office building:
   - Property taxes
   - Property insurance
   - Utilities (common area)
   - Repairs & maintenance
   - Property management fee (% of EGI)
   - Janitorial/cleaning
   - Landscaping
   - Security
*)

module Accrual = Orcaset.Accrual
module Period = Orcaset.Period

type t = {
  property_taxes : Accrual.t Seq.t;
  insurance : Accrual.t Seq.t;
  utilities : Accrual.t Seq.t;
  repairs_maintenance : Accrual.t Seq.t;
  property_management : Accrual.t Seq.t;
  janitorial : Accrual.t Seq.t;
  landscaping : Accrual.t Seq.t;
  security : Accrual.t Seq.t;
  total : Accrual.t Seq.t;
}

let make ~egi_lazy ~start_date ~property_taxes_annual ~insurance_annual ~utilities_monthly
    ~repairs_monthly ~management_fee_pct ~janitorial_monthly ~landscaping_monthly ~security_monthly
    ~expense_growth ~freq ~yf =
  (* Property taxes: Annual amount distributed monthly, with annual growth *)
  let property_taxes =
    Accrual.const_annual_growth_seq ~start_date ~initial_value:(-.property_taxes_annual /. 12.0)
      ~rate:expense_growth ~freq ~yf
    |> Seq.memoize
  in
  (* Insurance: Annual premium distributed monthly, with modest growth *)
  let insurance =
    Accrual.const_annual_growth_seq ~start_date ~initial_value:(-.insurance_annual /. 12.0)
      ~rate:expense_growth ~freq ~yf
    |> Seq.memoize
  in
  (* Utilities: Common area utilities with expense growth *)
  let utilities =
    Accrual.const_annual_growth_seq ~start_date ~initial_value:(-.utilities_monthly)
      ~rate:expense_growth ~freq ~yf
    |> Seq.memoize
  in
  (* Repairs & Maintenance: Fixed monthly with expense growth *)
  let repairs_maintenance =
    Accrual.const_annual_growth_seq ~start_date ~initial_value:(-.repairs_monthly)
      ~rate:expense_growth ~freq ~yf
    |> Seq.memoize
  in
  (* Property Management: Percentage of Effective Gross Income *)
  let property_management =
    let periods = Period.make_seq ~start_date ~offset:freq in
    Seq.map
      (fun period ->
        let value =
          lazy
            (let egi =
               Accrual.accrue period.Period.start_date period.end_date (Lazy.force egi_lazy)
             in
             (* Management fee is negative expense *)
             -.egi *. management_fee_pct)
        in
        Accrual.make ~period ~value ~split_fn:Accrual.default_split_fn)
      periods
    |> Seq.memoize
  in
  (* Janitorial: Fixed monthly with expense growth *)
  let janitorial =
    Accrual.const_annual_growth_seq ~start_date ~initial_value:(-.janitorial_monthly)
      ~rate:expense_growth ~freq ~yf
    |> Seq.memoize
  in
  (* Landscaping: Fixed monthly with expense growth *)
  let landscaping =
    Accrual.const_annual_growth_seq ~start_date ~initial_value:(-.landscaping_monthly)
      ~rate:expense_growth ~freq ~yf
    |> Seq.memoize
  in
  (* Security: Fixed monthly with expense growth *)
  let security =
    Accrual.const_annual_growth_seq ~start_date ~initial_value:(-.security_monthly)
      ~rate:expense_growth ~freq ~yf
    |> Seq.memoize
  in
  (* Total operating expenses *)
  let total =
    let sum2 a b = Accrual.sum_seq a b |> Seq.memoize in
    sum2 property_taxes insurance |> sum2 utilities |> sum2 repairs_maintenance
    |> sum2 property_management |> sum2 janitorial |> sum2 landscaping |> sum2 security
    |> Seq.memoize
  in
  {
    property_taxes;
    insurance;
    utilities;
    repairs_maintenance;
    property_management;
    janitorial;
    landscaping;
    security;
    total;
  }
