(* Commercial Real Estate Pro Forma Model
   
   Demonstrates a simple pro forma model for a fictitious small office building.

   Property: Small Class B Office Building
   - 25,000 SF rentable area
   - 50 parking spaces
*)

(* =====================================================
   ASSUMPTIONS
   ===================================================== *)

(* Dates *)
let start_date = CalendarLib.Date.make 2023 1 1
let freq = Orcaset.Period.make_offset ~months:1 ()
let output_freq = Orcaset.Period.make_offset ~months:1 ()
let output_periods = 12
let yf = Orcaset.Yf.actual_360

(* Property Characteristics *)
let building_sf = 25000.0
let parking_spaces = 50

(* Revenue Assumptions *)
let base_rent_per_sf_year1 = 22.0 (* $22/SF/year *)
let base_rent_monthly = building_sf *. base_rent_per_sf_year1 /. 12.0
let rent_growth = 0.03 (* 3% annual rent growth *)
let parking_rate_monthly = 75.0 (* $75/space/month *)
let parking_monthly = float_of_int parking_spaces *. parking_rate_monthly
let cam_recovery_pct = 0.85 (* 85% of opex recovered from tenants *)

(* CAM estimate for first month based on prior year budget - roughly 85% of monthly opex *)
let cam_estimate_first = 24000.0 *. cam_recovery_pct /. 12.0 (* $24k/yr opex estimate * 85% *)
let other_income_monthly = 1500.0 (* Storage, signage, etc. *)
let vacancy_rate = 0.07 (* 7% vacancy/credit loss *)

(* Operating Expense Assumptions *)
let property_taxes_annual = 72000.0 (* $2.88/SF *)
let insurance_annual = 15000.0 (* $0.60/SF *)
let utilities_monthly = 6250.0 (* $3/SF/year common area *)
let repairs_monthly = 4167.0 (* $2/SF/year *)
let management_fee_pct = 0.04 (* 4% of EGI *)
let janitorial_monthly = 5208.0 (* $2.50/SF/year *)
let landscaping_monthly = 1250.0 (* $0.60/SF/year *)
let security_monthly = 2083.0 (* $1/SF/year *)
let expense_growth = 0.025 (* 2.5% annual expense growth *)

(* Debt Assumptions *)
let purchase_price = 4500000.0 (* $180/SF *)
let ltv = 0.70 (* 70% LTV *)
let loan_amount = purchase_price *. ltv
let interest_rate = 0.055 (* 5.5% fixed rate *)
let loan_term_years = 25
let loan_term_months = loan_term_years * 12

(* CapEx Assumptions *)
let reserve_pct = 0.03 (* 3% of EGI for reserves *)
let ti_per_sf_annual = 1.50 (* $1.50/SF/year TI *)
let leasing_commission_pct = 0.02 (* 2% of EGI *)

(* =====================================================
   MODEL
   ===================================================== *)

(* Build model with circular dependencies *)
let rec lazy_revenue =
  lazy
    (Revenue.make
       ~opex_total_lazy:(lazy (Lazy.force lazy_opex).Opex.total)
       ~start_date ~base_rent_first:base_rent_monthly ~rent_growth ~parking_monthly
       ~cam_recovery_pct ~cam_estimate_first ~other_income_monthly ~vacancy_rate ~freq ~yf)

and lazy_opex =
  lazy
    (Opex.make
       ~egi_lazy:(lazy (Lazy.force lazy_revenue).Revenue.effective_gross_income)
       ~start_date ~property_taxes_annual ~insurance_annual ~utilities_monthly ~repairs_monthly
       ~management_fee_pct ~janitorial_monthly ~landscaping_monthly ~security_monthly
       ~expense_growth ~freq ~yf)

and lazy_capex =
  lazy
    (Capex.make
       ~egi_lazy:(lazy (Lazy.force lazy_revenue).Revenue.effective_gross_income)
       ~start_date ~reserve_pct ~ti_per_sf_annual ~building_sf
       ~commission_pct:leasing_commission_pct ~freq)

let revenue = Lazy.force lazy_revenue
let opex = Lazy.force lazy_opex
let capex = Lazy.force lazy_capex

(* Debt service (not circular) *)
let debt =
  Debt.make ~loan_amount ~annual_rate:interest_rate ~term_months:loan_term_months ~start_date ~yf

(* Net Operating Income = EGI - Operating Expenses *)
let noi = Orcaset.Accrual.sum_seq revenue.effective_gross_income opex.total |> Seq.memoize

(* Cash Flow Before Financing = NOI - CapEx *)
let cfbf = Orcaset.Accrual.sum_seq noi capex.total |> Seq.memoize

(* Cash Flow After Financing = CFBF - Debt Service *)
let cfaf = Orcaset.Accrual.sum_seq cfbf debt.total_debt_service |> Seq.memoize

(* =====================================================
   STATEMENT STRUCTURE
   ===================================================== *)

let pro_forma_statement =
  let open Orcaset.Statement in
  group "Real Estate Pro Forma"
    [
      group ~total:revenue.gross_potential_rent "Gross Potential Rent"
        [
          line "Base Rent" revenue.base_rent;
          line "Parking Income" revenue.parking;
          line "CAM Recoveries" revenue.cam_recoveries;
          line "Other Income" revenue.other_income;
        ];
      line "Less: Vacancy & Credit Loss" revenue.vacancy_loss;
      line "Effective Gross Income" revenue.effective_gross_income;
      group ~total:opex.total "Operating Expenses"
        [
          line "Property Taxes" opex.property_taxes;
          line "Insurance" opex.insurance;
          line "Utilities" opex.utilities;
          line "Repairs & Maintenance" opex.repairs_maintenance;
          line "Property Management" opex.property_management;
          line "Janitorial" opex.janitorial;
          line "Landscaping" opex.landscaping;
          line "Security" opex.security;
        ];
      line "Net Operating Income (NOI)" noi;
      group ~total:capex.total "Capital Expenditures"
        [
          line "Capital Reserves" capex.capital_reserves;
          line "Tenant Improvements" capex.tenant_improvements;
          line "Leasing Commissions" capex.leasing_commissions;
        ];
      line "Cash Flow Before Financing" cfbf;
      group ~total:debt.total_debt_service "Debt Service"
        [
          line "Interest Expense" debt.interest_expense;
          line "Principal Payment" debt.principal_payment;
        ];
      line "Cash Flow After Financing" cfaf;
    ]

(* =====================================================
   OUTPUT
   ===================================================== *)

let print_statement ~periods item =
  let hdr p =
    Printf.sprintf "%14s" (CalendarLib.Printer.Date.sprint "%Y-%m-%d" p.Orcaset.Period.start_date)
  in
  let fmt v = Printf.sprintf "%14.0f" v in
  let print_row label seq =
    Printf.printf "%-35s%s\n" label
      (String.concat "" (List.map fmt (Orcaset.Accrual.accrue_periods periods seq)))
  in
  Printf.printf "%-35s%s\n" "" (String.concat "" (List.map hdr periods));
  Printf.printf "%s\n" (String.make (35 + (14 * List.length periods)) '=');
  let indent = ref 0 in
  Orcaset.Statement.iter item
    ~line_fn:(fun label seq -> print_row (String.make !indent ' ' ^ label) seq)
    ~group_fn:(fun label total phase ->
      match phase with
      | `Enter ->
          Printf.printf "\n%s\n" (String.make !indent ' ' ^ label);
          indent := !indent + 2
      | `Exit ->
          Option.iter
            (fun t ->
              Printf.printf "%s\n" (String.make (35 + (14 * List.length periods)) '-');
              print_row (String.make (!indent - 2) ' ' ^ "Total " ^ label) t)
            total;
          print_newline ();
          indent := !indent - 2)

let print_assumptions () =
  Printf.printf "PROPERTY ASSUMPTIONS\n";
  Printf.printf "====================\n";
  Printf.printf "Building Size:           %.0f SF\n" building_sf;
  Printf.printf "Parking Spaces:          %d\n" parking_spaces;
  Printf.printf "Purchase Price:          $%.0f ($%.0f/SF)\n" purchase_price
    (purchase_price /. building_sf);
  Printf.printf "\n";
  Printf.printf "REVENUE ASSUMPTIONS\n";
  Printf.printf "====================\n";
  Printf.printf "Year 1 Base Rent:        $%.2f/SF/year\n" base_rent_per_sf_year1;
  Printf.printf "Rent Growth:             %.1f%%/year\n" (rent_growth *. 100.0);
  Printf.printf "Parking Rate:            $%.0f/space/month\n" parking_rate_monthly;
  Printf.printf "CAM Recovery:            %.0f%% of OpEx\n" (cam_recovery_pct *. 100.0);
  Printf.printf "Vacancy/Credit Loss:     %.0f%%\n" (vacancy_rate *. 100.0);
  Printf.printf "\n";
  Printf.printf "DEBT ASSUMPTIONS\n";
  Printf.printf "====================\n";
  Printf.printf "Loan Amount:             $%.0f (%.0f%% LTV)\n" loan_amount (ltv *. 100.0);
  Printf.printf "Interest Rate:           %.2f%%\n" (interest_rate *. 100.0);
  Printf.printf "Term:                    %d years\n" loan_term_years;
  Printf.printf "Monthly Payment:         $%.2f\n"
    (Debt.calc_monthly_payment ~loan_amount ~annual_rate:interest_rate ~term_months:loan_term_months);
  Printf.printf "\n\n"

let () =
  print_assumptions ();
  Printf.printf "PRO FORMA CASH FLOW PROJECTION (5-Year)\n";
  Printf.printf "=======================================\n\n";
  let periods =
    Orcaset.Period.make_seq ~start_date ~offset:output_freq
    |> Seq.take output_periods |> List.of_seq
  in
  print_statement ~periods pro_forma_statement
