(* Property Module for Commercial Real Estate Pro Forma
   
   Encapsulates a single property with all its assumptions,
   revenue, opex, capex, debt, and computed cash flows.
   Enables portfolio-level aggregation of multiple properties.
*)

module Accrual = Orcaset.Accrual
module Period = Orcaset.Period
module Statement = Orcaset.Statement
module Date = CalendarLib.Date

(* Property input assumptions *)
type assumptions = {
  name : string;
  start_date : Date.t;
  freq : Period.offset;
  yf : Date.t -> Date.t -> float;
  (* Physical characteristics *)
  building_sf : float;
  parking_spaces : int;
  (* Revenue assumptions *)
  base_rent_per_sf_year1 : float;
  rent_growth : float;
  parking_rate_monthly : float;
  cam_recovery_pct : float;
  cam_estimate_first : float;
  other_income_monthly : float;
  vacancy_rate : float;
  (* OpEx assumptions *)
  property_taxes_annual : float;
  insurance_annual : float;
  utilities_monthly : float;
  repairs_monthly : float;
  management_fee_pct : float;
  janitorial_monthly : float;
  landscaping_monthly : float;
  security_monthly : float;
  expense_growth : float;
  (* Debt assumptions *)
  purchase_price : float;
  ltv : float;
  interest_rate : float;
  loan_term_years : int;
  (* CapEx assumptions *)
  reserve_pct : float;
  ti_per_sf_annual : float;
  leasing_commission_pct : float;
}

(* Computed property model *)
type t = {
  assumptions : assumptions;
  revenue : Revenue.t;
  opex : Opex.t;
  capex : Capex.t;
  debt : Debt.t;
  noi : Accrual.t Seq.t;
  cfbf : Accrual.t Seq.t;
  cfaf : Accrual.t Seq.t;
}

let make (a : assumptions) : t =
  let base_rent_monthly = a.building_sf *. a.base_rent_per_sf_year1 /. 12.0 in
  let parking_monthly = float_of_int a.parking_spaces *. a.parking_rate_monthly in
  let loan_amount = a.purchase_price *. a.ltv in
  let loan_term_months = a.loan_term_years * 12 in
  (* Build model with circular dependencies using lazy evaluation *)
  let rec lazy_revenue =
    lazy
      (Revenue.make
         ~opex_total_lazy:(lazy (Lazy.force lazy_opex).Opex.total)
         ~start_date:a.start_date ~base_rent_first:base_rent_monthly ~rent_growth:a.rent_growth
         ~parking_monthly ~cam_recovery_pct:a.cam_recovery_pct
         ~cam_estimate_first:a.cam_estimate_first ~other_income_monthly:a.other_income_monthly
         ~vacancy_rate:a.vacancy_rate ~freq:a.freq ~yf:a.yf)
  and lazy_opex =
    lazy
      (Opex.make
         ~egi_lazy:(lazy (Lazy.force lazy_revenue).Revenue.effective_gross_income)
         ~start_date:a.start_date ~property_taxes_annual:a.property_taxes_annual
         ~insurance_annual:a.insurance_annual ~utilities_monthly:a.utilities_monthly
         ~repairs_monthly:a.repairs_monthly ~management_fee_pct:a.management_fee_pct
         ~janitorial_monthly:a.janitorial_monthly ~landscaping_monthly:a.landscaping_monthly
         ~security_monthly:a.security_monthly ~expense_growth:a.expense_growth ~freq:a.freq ~yf:a.yf)
  and lazy_capex =
    lazy
      (Capex.make
         ~egi_lazy:(lazy (Lazy.force lazy_revenue).Revenue.effective_gross_income)
         ~start_date:a.start_date ~reserve_pct:a.reserve_pct ~ti_per_sf_annual:a.ti_per_sf_annual
         ~building_sf:a.building_sf ~commission_pct:a.leasing_commission_pct ~freq:a.freq)
  in
  let revenue = Lazy.force lazy_revenue in
  let opex = Lazy.force lazy_opex in
  let capex = Lazy.force lazy_capex in
  (* Debt service (not circular) *)
  let debt =
    Debt.make ~loan_amount ~annual_rate:a.interest_rate ~term_months:loan_term_months
      ~start_date:a.start_date ~yf:a.yf
  in
  (* Net Operating Income = EGI - Operating Expenses *)
  let noi = Accrual.sum_seq revenue.effective_gross_income opex.total |> Seq.memoize in
  (* Cash Flow Before Financing = NOI - CapEx *)
  let cfbf = Accrual.sum_seq noi capex.total |> Seq.memoize in
  (* Cash Flow After Financing = CFBF - Debt Service *)
  let cfaf = Accrual.sum_seq cfbf debt.total_debt_service |> Seq.memoize in
  { assumptions = a; revenue; opex; capex; debt; noi; cfbf; cfaf }

(* Create statement for a single property *)
let to_statement (p : t) : Accrual.t Seq.t Statement.item =
  let open Statement in
  group p.assumptions.name
    [
      group ~total:p.revenue.gross_potential_rent "Gross Potential Rent"
        [
          line "Base Rent" p.revenue.base_rent;
          line "Parking Income" p.revenue.parking;
          line "CAM Recoveries" p.revenue.cam_recoveries;
          line "Other Income" p.revenue.other_income;
        ];
      line "Less: Vacancy & Credit Loss" p.revenue.vacancy_loss;
      line "Effective Gross Income" p.revenue.effective_gross_income;
      group ~total:p.opex.total "Operating Expenses"
        [
          line "Property Taxes" p.opex.property_taxes;
          line "Insurance" p.opex.insurance;
          line "Utilities" p.opex.utilities;
          line "Repairs & Maintenance" p.opex.repairs_maintenance;
          line "Property Management" p.opex.property_management;
          line "Janitorial" p.opex.janitorial;
          line "Landscaping" p.opex.landscaping;
          line "Security" p.opex.security;
        ];
      line "Net Operating Income (NOI)" p.noi;
      group ~total:p.capex.total "Capital Expenditures"
        [
          line "Capital Reserves" p.capex.capital_reserves;
          line "Tenant Improvements" p.capex.tenant_improvements;
          line "Leasing Commissions" p.capex.leasing_commissions;
        ];
      line "Cash Flow Before Financing" p.cfbf;
      group ~total:p.debt.total_debt_service "Debt Service"
        [
          line "Interest Expense" p.debt.interest_expense;
          line "Principal Payment" p.debt.principal_payment;
        ];
      line "Cash Flow After Financing" p.cfaf;
    ]

let print_assumptions (a : assumptions) =
  Printf.printf "PROPERTY: %s\n" a.name;
  Printf.printf "====================\n";
  Printf.printf "Building Size:           %.0f SF\n" a.building_sf;
  Printf.printf "Parking Spaces:          %d\n" a.parking_spaces;
  Printf.printf "Purchase Price:          $%.0f ($%.0f/SF)\n" a.purchase_price
    (a.purchase_price /. a.building_sf);
  Printf.printf "Year 1 Base Rent:        $%.2f/SF/year\n" a.base_rent_per_sf_year1;
  Printf.printf "Rent Growth:             %.1f%%/year\n" (a.rent_growth *. 100.0);
  Printf.printf "Vacancy/Credit Loss:     %.0f%%\n" (a.vacancy_rate *. 100.0);
  Printf.printf "Loan Amount:             $%.0f (%.0f%% LTV)\n" (a.purchase_price *. a.ltv)
    (a.ltv *. 100.0);
  Printf.printf "Interest Rate:           %.2f%%\n" (a.interest_rate *. 100.0);
  Printf.printf "Term:                    %d years\n" a.loan_term_years;
  Printf.printf "Monthly Payment:         $%.2f\n"
    (Debt.calc_monthly_payment ~loan_amount:(a.purchase_price *. a.ltv) ~annual_rate:a.interest_rate
       ~term_months:(a.loan_term_years * 12));
  Printf.printf "\n"
