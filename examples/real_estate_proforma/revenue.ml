(* Revenue Module for Commercial Real Estate Pro Forma
   
   Revenue streams for a small office building:
   - Base rent (tenant leases)
   - Parking income 
   - CAM (Common Area Maintenance) recoveries
   - Other income (storage, signage fees)
*)

module Accrual = Orcaset.Accrual
module Period = Orcaset.Period

type t = {
  base_rent : Accrual.t Seq.t;
  parking : Accrual.t Seq.t;
  cam_recoveries : Accrual.t Seq.t;
  other_income : Accrual.t Seq.t;
  gross_potential_rent : Accrual.t Seq.t;
  vacancy_loss : Accrual.t Seq.t;
  effective_gross_income : Accrual.t Seq.t;
}

let make ~opex_total_lazy ~start_date ~base_rent_first ~rent_growth ~parking_monthly
    ~cam_recovery_pct ~cam_estimate_first ~other_income_monthly ~vacancy_rate ~freq ~yf =
  (* Base rent: Monthly rent with annual escalation *)
  let base_rent =
    Accrual.const_annual_growth_seq ~start_date ~initial_value:base_rent_first ~rate:rent_growth
      ~freq ~yf
    |> Seq.memoize
  in
  (* Parking: Fixed monthly income with modest growth *)
  let parking =
    Accrual.const_annual_growth_seq ~start_date ~initial_value:parking_monthly ~rate:rent_growth
      ~freq ~yf
    |> Seq.memoize
  in
  (* CAM recoveries: Based on PRIOR month's operating expenses
     First month uses an estimated value (prior year budget), then uses N-1 opex *)
  let cam_recoveries =
    let start_period =
      Period.make ~start_date
        ~end_date:(CalendarLib.Date.add start_date (CalendarLib.Date.Period.month 1))
    in
    let first_accrual =
      Accrual.make ~period:start_period
        ~value:(lazy cam_estimate_first)
        ~split_fn:Accrual.default_split_fn
    in
    let periods = Period.make_seq ~start_date:start_period.end_date ~offset:freq in
    Seq.cons first_accrual (fun () ->
        Seq.map
          (fun period ->
            let value =
              lazy
                (let prior_start = CalendarLib.Date.prev period.Period.start_date `Month in
                 let opex =
                   Accrual.accrue prior_start period.Period.start_date (Lazy.force opex_total_lazy)
                 in
                 (* CAM recoveries are positive (negate negative opex) *)
                 Float.abs opex *. cam_recovery_pct)
            in
            Accrual.make ~period ~value ~split_fn:Accrual.default_split_fn)
          periods ())
    |> Seq.memoize
  in
  (* Other income: Storage, signage, misc fees *)
  let other_income =
    Accrual.const_annual_growth_seq ~start_date ~initial_value:other_income_monthly
      ~rate:rent_growth ~freq ~yf
    |> Seq.memoize
  in
  (* Gross potential rent = sum of all revenue streams *)
  let gross_potential_rent =
    Accrual.sum_seq
      (Accrual.sum_seq (Accrual.sum_seq base_rent parking) cam_recoveries)
      other_income
    |> Seq.memoize
  in
  (* Vacancy loss: Percentage of GPR lost to vacancy *)
  let vacancy_loss =
    Seq.map (fun gpr -> Accrual.map (fun v -> v *. -.vacancy_rate) gpr) gross_potential_rent
    |> Seq.memoize
  in
  (* Effective Gross Income = GPR - Vacancy *)
  let effective_gross_income = Accrual.sum_seq gross_potential_rent vacancy_loss |> Seq.memoize in
  {
    base_rent;
    parking;
    cam_recoveries;
    other_income;
    gross_potential_rent;
    vacancy_loss;
    effective_gross_income;
  }
