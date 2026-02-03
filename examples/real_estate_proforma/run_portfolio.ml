(* Portfolio Pro Forma
   
   Aggregates multiple commercial real estate properties into
   a single portfolio-level pro forma statement. Demonstrates
   model reuse and running projections across large portfolios.
*)

module Accrual = Orcaset.Accrual
module Period = Orcaset.Period
module Statement = Orcaset.Statement

(* =====================================================
   SHARED ASSUMPTIONS
   ===================================================== *)

let property_count = 1000
let start_date = CalendarLib.Date.make 2023 1 1
let freq = Period.make_offset ~months:1 ()
let output_freq = Period.make_offset ~months:1 ()
let output_periods = 120
let yf = Orcaset.Yf.actual_360

(* =====================================================
   PROPERTY ASSUMPTIONS
   ===================================================== *)

(* Create new property assumptions *)
let downtown_office : Property.assumptions =
  {
    name = "Downtown Office (Class B)";
    start_date;
    freq;
    yf;
    building_sf = 25000.0;
    parking_spaces = 50;
    base_rent_per_sf_year1 = 22.0;
    rent_growth = 0.03;
    parking_rate_monthly = 75.0;
    cam_recovery_pct = 0.85;
    cam_estimate_first = 24000.0 *. 0.85 /. 12.0;
    other_income_monthly = 1500.0;
    vacancy_rate = 0.07;
    property_taxes_annual = 72000.0;
    insurance_annual = 15000.0;
    utilities_monthly = 6250.0;
    repairs_monthly = 4167.0;
    management_fee_pct = 0.04;
    janitorial_monthly = 5208.0;
    landscaping_monthly = 1250.0;
    security_monthly = 2083.0;
    expense_growth = 0.025;
    purchase_price = 4500000.0;
    ltv = 0.70;
    interest_rate = 0.055;
    loan_term_years = 25;
    reserve_pct = 0.03;
    ti_per_sf_annual = 1.50;
    leasing_commission_pct = 0.02;
  }
(* =====================================================
   BUILD PORTFOLIO MODEL
   ===================================================== *)

let properties =
  let assumptions_list = [ downtown_office ] in
  List.to_seq assumptions_list |> Seq.cycle |> Seq.take property_count |> Seq.map Property.make
  |> List.of_seq

(* Output periods *)
let periods =
  Period.make_seq ~start_date ~offset:output_freq |> Seq.take output_periods |> List.of_seq

(* Helper to aggregate a field across all properties using accrue_periods *)
let aggregate f =
  let zero = List.init output_periods (fun _ -> 0.0) in
  List.fold_left
    (fun acc p -> List.map2 ( +. ) acc (Accrual.accrue_periods periods (f p)))
    zero properties

(* Portfolio totals - aggregate all property line items *)

(* Revenue line items *)
let portfolio_base_rent = aggregate (fun p -> p.Property.revenue.Revenue.base_rent)
let portfolio_parking = aggregate (fun p -> p.Property.revenue.Revenue.parking)
let portfolio_cam_recoveries = aggregate (fun p -> p.Property.revenue.Revenue.cam_recoveries)
let portfolio_other_income = aggregate (fun p -> p.Property.revenue.Revenue.other_income)
let portfolio_gpr = aggregate (fun p -> p.Property.revenue.Revenue.gross_potential_rent)
let portfolio_vacancy_loss = aggregate (fun p -> p.Property.revenue.Revenue.vacancy_loss)
let portfolio_egi = aggregate (fun p -> p.Property.revenue.Revenue.effective_gross_income)

(* Operating expenses line items *)
let portfolio_property_taxes = aggregate (fun p -> p.Property.opex.Opex.property_taxes)
let portfolio_insurance = aggregate (fun p -> p.Property.opex.Opex.insurance)
let portfolio_utilities = aggregate (fun p -> p.Property.opex.Opex.utilities)
let portfolio_repairs = aggregate (fun p -> p.Property.opex.Opex.repairs_maintenance)
let portfolio_management = aggregate (fun p -> p.Property.opex.Opex.property_management)
let portfolio_janitorial = aggregate (fun p -> p.Property.opex.Opex.janitorial)
let portfolio_landscaping = aggregate (fun p -> p.Property.opex.Opex.landscaping)
let portfolio_security = aggregate (fun p -> p.Property.opex.Opex.security)
let portfolio_total_opex = aggregate (fun p -> p.Property.opex.Opex.total)

(* Capital expenditures line items *)
let portfolio_capital_reserves = aggregate (fun p -> p.Property.capex.Capex.capital_reserves)
let portfolio_tenant_improvements = aggregate (fun p -> p.Property.capex.Capex.tenant_improvements)
let portfolio_leasing_commissions = aggregate (fun p -> p.Property.capex.Capex.leasing_commissions)
let portfolio_total_capex = aggregate (fun p -> p.Property.capex.Capex.total)

(* Debt service line items *)
let portfolio_interest_expense = aggregate (fun p -> p.Property.debt.Debt.interest_expense)
let portfolio_principal_payment = aggregate (fun p -> p.Property.debt.Debt.principal_payment)
let portfolio_total_debt = aggregate (fun p -> p.Property.debt.Debt.total_debt_service)

(* Cash flow totals *)
let portfolio_noi = aggregate (fun p -> p.Property.noi)
let portfolio_cfbf = aggregate (fun p -> p.Property.cfbf)
let portfolio_cfaf = aggregate (fun p -> p.Property.cfaf)
(* =====================================================
   PORTFOLIO STATEMENT STRUCTURE
   ===================================================== *)

let portfolio_statement =
  let open Statement in
  group "Portfolio Totals"
    [
      group ~total:portfolio_gpr "Gross Potential Rent"
        [
          line "Base Rent" portfolio_base_rent;
          line "Parking Income" portfolio_parking;
          line "CAM Recoveries" portfolio_cam_recoveries;
          line "Other Income" portfolio_other_income;
        ];
      line "Less: Vacancy & Credit Loss" portfolio_vacancy_loss;
      line "Effective Gross Income" portfolio_egi;
      group ~total:portfolio_total_opex "Operating Expenses"
        [
          line "Property Taxes" portfolio_property_taxes;
          line "Insurance" portfolio_insurance;
          line "Utilities" portfolio_utilities;
          line "Repairs & Maintenance" portfolio_repairs;
          line "Property Management" portfolio_management;
          line "Janitorial" portfolio_janitorial;
          line "Landscaping" portfolio_landscaping;
          line "Security" portfolio_security;
        ];
      line "Net Operating Income (NOI)" portfolio_noi;
      group ~total:portfolio_total_capex "Capital Expenditures"
        [
          line "Capital Reserves" portfolio_capital_reserves;
          line "Tenant Improvements" portfolio_tenant_improvements;
          line "Leasing Commissions" portfolio_leasing_commissions;
        ];
      line "Cash Flow Before Financing" portfolio_cfbf;
      group ~total:portfolio_total_debt "Debt Service"
        [
          line "Interest Expense" portfolio_interest_expense;
          line "Principal Payment" portfolio_principal_payment;
        ];
      line "Cash Flow After Financing" portfolio_cfaf;
    ]

(* =====================================================
   OUTPUT
   ===================================================== *)

let print_statement item =
  let hdr p =
    Printf.sprintf "%14s" (CalendarLib.Printer.Date.sprint "%Y-%m-%d" p.Period.start_date)
  in
  let fmt v = Printf.sprintf "%14.0f" v in
  let print_row label values =
    Printf.printf "%-40s%s\n" label (String.concat "" (List.map fmt values))
  in
  Printf.printf "%-40s%s\n" "" (String.concat "" (List.map hdr periods));
  Printf.printf "%s\n" (String.make (40 + (14 * List.length periods)) '=');
  let indent = ref 0 in
  Statement.iter item
    ~line_fn:(fun label values -> print_row (String.make !indent ' ' ^ label) values)
    ~group_fn:(fun label total phase ->
      match phase with
      | `Enter ->
          Printf.printf "\n%s\n" (String.make !indent ' ' ^ label);
          indent := !indent + 2
      | `Exit ->
          Option.iter
            (fun t ->
              Printf.printf "%s\n" (String.make (40 + (14 * List.length periods)) '-');
              print_row (String.make (!indent - 2) ' ' ^ "Total " ^ label) t)
            total;
          print_newline ();
          indent := !indent - 2)

let print_portfolio_summary () =
  Printf.printf "PORTFOLIO SUMMARY\n";
  Printf.printf "=================\n\n";
  Printf.printf "Properties in Portfolio: %d\n\n" (List.length properties);
  let total_sf =
    List.fold_left (fun acc p -> acc +. p.Property.assumptions.building_sf) 0.0 properties
  in
  let total_purchase =
    List.fold_left (fun acc p -> acc +. p.Property.assumptions.purchase_price) 0.0 properties
  in
  let total_loan =
    List.fold_left
      (fun acc p -> acc +. (p.Property.assumptions.purchase_price *. p.Property.assumptions.ltv))
      0.0 properties
  in
  Printf.printf "Total Square Footage:    %.0f SF\n" total_sf;
  Printf.printf "Total Purchase Price:    $%.0f\n" total_purchase;
  Printf.printf "Total Loan Amount:       $%.0f\n" total_loan;
  Printf.printf "Weighted Avg LTV:        %.1f%%\n" (total_loan /. total_purchase *. 100.0);
  Printf.printf "\n"

let () =
  print_portfolio_summary ();
  Printf.printf "PORTFOLIO PRO FORMA PROJECTIONS\n";
  Printf.printf "==================================================\n\n";
  print_statement portfolio_statement
