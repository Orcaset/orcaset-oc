(* Property Module for Commercial Real Estate Pro Forma
   
   Encapsulates a complete property with all revenue, expenses, debt, and cash flows.
   Allows creating multiple properties with the same or different assumptions.
*)

module Accrual = Orcaset.Accrual
module Period = Orcaset.Period
module Statement = Orcaset.Statement

(* Property assumptions record *)
type assumptions = {
  (* Property Characteristics *)
  building_sf : float;
  parking_spaces : int;
  (* Revenue Assumptions *)
  base_rent_per_sf_year1 : float;
  rent_growth : float;
  parking_rate_monthly : float;
  cam_recovery_pct : float;
  other_income_monthly : float;
  vacancy_rate : float;
  (* Operating Expense Assumptions *)
  property_taxes_annual : float;
  property_tax_growth : float;
  insurance_annual : float;
  insurance_growth : float;
  utilities_monthly : float;
  repairs_monthly : float;
  management_fee_pct : float;
  janitorial_monthly : float;
  landscaping_monthly : float;
  security_monthly : float;
  (* Debt Assumptions *)
  purchase_price : float;
  ltv : float;
  interest_rate : float;
  loan_term_years : int;
  (* CapEx Assumptions *)
  reserve_pct : float;
  ti_per_sf_annual : float;
  leasing_commission_pct : float;
}

(* Property type containing all cash flow streams *)
type t = {
  name : string;
  assumptions : assumptions;
  revenue : Revenue.t;
  opex : Opex.t;
  capex : Capex.t;
  debt : Debt.t;
  noi : Accrual.t Seq.t;
  cfbf : Accrual.t Seq.t;
  cfaf : Accrual.t Seq.t;
}

(* Default assumptions matching main.ml *)
let default_assumptions =
  {
    (* Property Characteristics *)
    building_sf = 25000.0;
    parking_spaces = 50;
    (* Revenue Assumptions *)
    base_rent_per_sf_year1 = 22.0;
    (* $22/SF/year *)
    rent_growth = 0.03;
    (* 3% annual rent growth *)
    parking_rate_monthly = 75.0;
    (* $75/space/month *)
    cam_recovery_pct = 0.85;
    (* 85% of opex recovered from tenants *)
    other_income_monthly = 1500.0;
    (* Storage, signage, etc. *)
    vacancy_rate = 0.07;
    (* 7% vacancy/credit loss *)
    (* Operating Expense Assumptions *)
    property_taxes_annual = 72000.0;
    (* $2.88/SF *)
    property_tax_growth = 0.02;
    (* 2% annual increase *)
    insurance_annual = 15000.0;
    (* $0.60/SF *)
    insurance_growth = 0.03;
    (* 3% annual increase *)
    utilities_monthly = 6250.0;
    (* $3/SF/year common area *)
    repairs_monthly = 4167.0;
    (* $2/SF/year *)
    management_fee_pct = 0.04;
    (* 4% of EGI *)
    janitorial_monthly = 5208.0;
    (* $2.50/SF/year *)
    landscaping_monthly = 1250.0;
    (* $0.60/SF/year *)
    security_monthly = 2083.0;
    (* $1/SF/year *)
    (* Debt Assumptions *)
    purchase_price = 4500000.0;
    (* $180/SF *)
    ltv = 0.70;
    (* 70% LTV *)
    interest_rate = 0.055;
    (* 5.5% fixed rate *)
    loan_term_years = 25;
    (* CapEx Assumptions *)
    reserve_pct = 0.03;
    (* 3% of EGI for reserves *)
    ti_per_sf_annual = 1.50;
    (* $1.50/SF/year TI *)
    leasing_commission_pct = 0.02;
    (* 2% of EGI *)
  }

(* Create a property with given name, assumptions, and start date *)
let make ~name ~assumptions ~start_date =
  let freq = Period.make_offset ~months:1 () in
  let yf = Orcaset.Yf.actual_360 in

  (* Derived values *)
  let base_rent_monthly = assumptions.building_sf *. assumptions.base_rent_per_sf_year1 /. 12.0 in
  let parking_monthly =
    float_of_int assumptions.parking_spaces *. assumptions.parking_rate_monthly
  in
  let cam_estimate_first =
    assumptions.property_taxes_annual *. assumptions.cam_recovery_pct /. 12.0
  in
  let loan_amount = assumptions.purchase_price *. assumptions.ltv in
  let loan_term_months = assumptions.loan_term_years * 12 in

  (* Build model with circular dependencies *)
  let rec lazy_revenue =
    lazy
      (Revenue.make
         ~opex_total_lazy:(lazy (Lazy.force lazy_opex).Opex.total)
         ~start_date ~base_rent_first:base_rent_monthly ~rent_growth:assumptions.rent_growth
         ~parking_monthly ~cam_recovery_pct:assumptions.cam_recovery_pct ~cam_estimate_first
         ~other_income_monthly:assumptions.other_income_monthly
         ~vacancy_rate:assumptions.vacancy_rate ~freq ~yf)
  and lazy_opex =
    lazy
      (Opex.make
         ~egi_lazy:(lazy (Lazy.force lazy_revenue).Revenue.effective_gross_income)
         ~start_date ~property_taxes_annual:assumptions.property_taxes_annual
         ~property_tax_growth:assumptions.property_tax_growth
         ~insurance_annual:assumptions.insurance_annual
         ~insurance_growth:assumptions.insurance_growth
         ~utilities_monthly:assumptions.utilities_monthly
         ~repairs_monthly:assumptions.repairs_monthly
         ~management_fee_pct:assumptions.management_fee_pct
         ~janitorial_monthly:assumptions.janitorial_monthly
         ~landscaping_monthly:assumptions.landscaping_monthly
         ~security_monthly:assumptions.security_monthly ~freq ~yf)
  and lazy_capex =
    lazy
      (Capex.make
         ~egi_lazy:(lazy (Lazy.force lazy_revenue).Revenue.effective_gross_income)
         ~start_date ~reserve_pct:assumptions.reserve_pct
         ~ti_per_sf_annual:assumptions.ti_per_sf_annual ~building_sf:assumptions.building_sf
         ~commission_pct:assumptions.leasing_commission_pct ~freq)
  in

  let revenue = Lazy.force lazy_revenue in
  let opex = Lazy.force lazy_opex in
  let capex = Lazy.force lazy_capex in

  (* Debt service *)
  let debt =
    Debt.make ~loan_amount ~annual_rate:assumptions.interest_rate ~term_months:loan_term_months
      ~start_date ~yf
  in

  (* Net Operating Income = EGI - Operating Expenses *)
  let noi = Accrual.sum_seq revenue.effective_gross_income opex.total |> Seq.memoize in

  (* Cash Flow Before Financing = NOI - CapEx *)
  let cfbf = Accrual.sum_seq noi capex.total |> Seq.memoize in

  (* Cash Flow After Financing = CFBF - Debt Service *)
  let cfaf = Accrual.sum_seq cfbf debt.total_debt_service |> Seq.memoize in

  { name; assumptions; revenue; opex; capex; debt; noi; cfbf; cfaf }

(* Create a property with default assumptions *)
let make_default ~name ~start_date = make ~name ~assumptions:default_assumptions ~start_date

(* Build pro forma statement for a single property *)
let pro_forma_statement prop =
  Statement.group ("Property: " ^ prop.name)
    [
      Statement.group ~total:prop.revenue.gross_potential_rent "Gross Potential Rent"
        [
          Statement.line "Base Rent" prop.revenue.base_rent;
          Statement.line "Parking Income" prop.revenue.parking;
          Statement.line "CAM Recoveries" prop.revenue.cam_recoveries;
          Statement.line "Other Income" prop.revenue.other_income;
        ];
      Statement.line "Less: Vacancy & Credit Loss" prop.revenue.vacancy_loss;
      Statement.line "Effective Gross Income" prop.revenue.effective_gross_income;
      Statement.group ~total:prop.opex.total "Operating Expenses"
        [
          Statement.line "Property Taxes" prop.opex.property_taxes;
          Statement.line "Insurance" prop.opex.insurance;
          Statement.line "Utilities" prop.opex.utilities;
          Statement.line "Repairs & Maintenance" prop.opex.repairs_maintenance;
          Statement.line "Property Management" prop.opex.property_management;
          Statement.line "Janitorial" prop.opex.janitorial;
          Statement.line "Landscaping" prop.opex.landscaping;
          Statement.line "Security" prop.opex.security;
        ];
      Statement.line "Net Operating Income (NOI)" prop.noi;
      Statement.group ~total:prop.capex.total "Capital Expenditures"
        [
          Statement.line "Capital Reserves" prop.capex.capital_reserves;
          Statement.line "Tenant Improvements" prop.capex.tenant_improvements;
          Statement.line "Leasing Commissions" prop.capex.leasing_commissions;
        ];
      Statement.line "Cash Flow Before Financing" prop.cfbf;
      Statement.group ~total:prop.debt.total_debt_service "Debt Service"
        [
          Statement.line "Interest Expense" prop.debt.interest_expense;
          Statement.line "Principal Payment" prop.debt.principal_payment;
        ];
      Statement.line "Cash Flow After Financing" prop.cfaf;
    ]

(* Print property assumptions *)
let print_assumptions prop =
  let a = prop.assumptions in
  Printf.printf "PROPERTY: %s\n" prop.name;
  Printf.printf "====================\n";
  Printf.printf "Building Size:           %.0f SF\n" a.building_sf;
  Printf.printf "Parking Spaces:          %d\n" a.parking_spaces;
  Printf.printf "Purchase Price:          $%.0f ($%.0f/SF)\n" a.purchase_price
    (a.purchase_price /. a.building_sf);
  Printf.printf "\n";
  Printf.printf "REVENUE ASSUMPTIONS\n";
  Printf.printf "====================\n";
  Printf.printf "Year 1 Base Rent:        $%.2f/SF/year\n" a.base_rent_per_sf_year1;
  Printf.printf "Rent Growth:             %.1f%%/year\n" (a.rent_growth *. 100.0);
  Printf.printf "Parking Rate:            $%.0f/space/month\n" a.parking_rate_monthly;
  Printf.printf "CAM Recovery:            %.0f%% of OpEx\n" (a.cam_recovery_pct *. 100.0);
  Printf.printf "Vacancy/Credit Loss:     %.0f%%\n" (a.vacancy_rate *. 100.0);
  Printf.printf "\n";
  Printf.printf "DEBT ASSUMPTIONS\n";
  Printf.printf "====================\n";
  let loan_amount = a.purchase_price *. a.ltv in
  let loan_term_months = a.loan_term_years * 12 in
  Printf.printf "Loan Amount:             $%.0f (%.0f%% LTV)\n" loan_amount (a.ltv *. 100.0);
  Printf.printf "Interest Rate:           %.2f%%\n" (a.interest_rate *. 100.0);
  Printf.printf "Term:                    %d years\n" a.loan_term_years;
  Printf.printf "Monthly Payment:         $%.2f\n"
    (Debt.calc_monthly_payment ~loan_amount ~annual_rate:a.interest_rate
       ~term_months:loan_term_months);
  Printf.printf "\n"
