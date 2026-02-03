(* Portfolio Analysis - Real Estate Pro Forma
   
   Creates multiple properties with the same assumptions and
   prints the combined portfolio pro forma.
*)

module Accrual = Orcaset.Accrual
module Period = Orcaset.Period
module Statement = Orcaset.Statement

(* =====================================================
   CONFIGURATION
   ===================================================== *)

let num_properties = 10
let start_date = CalendarLib.Date.make 2025 1 1
let output_freq = Period.make_offset ~months:1 ()
let output_periods = 12

(* =====================================================
   CREATE PORTFOLIO
   ===================================================== *)

(* Create all properties with default assumptions *)
let property_names = List.init num_properties (fun i -> Printf.sprintf "Property %d" (i + 1))
let properties = List.map (fun name -> Property.make_default ~name ~start_date) property_names

(* =====================================================
   PORTFOLIO AGGREGATION
   ===================================================== *)

(* Sum multiple accrual sequences together *)
let sum_accruals seqs =
  match seqs with
  | [] -> failwith "Cannot sum empty list of accruals"
  | first :: rest ->
      List.fold_left (fun acc seq -> Accrual.sum_seq acc seq |> Seq.memoize) first rest

(* Portfolio totals - Revenue *)
let portfolio_base_rent =
  sum_accruals (List.map (fun p -> p.Property.revenue.Revenue.base_rent) properties)

let portfolio_parking =
  sum_accruals (List.map (fun p -> p.Property.revenue.Revenue.parking) properties)

let portfolio_cam_recoveries =
  sum_accruals (List.map (fun p -> p.Property.revenue.Revenue.cam_recoveries) properties)

let portfolio_other_income =
  sum_accruals (List.map (fun p -> p.Property.revenue.Revenue.other_income) properties)

let portfolio_gpr =
  sum_accruals (List.map (fun p -> p.Property.revenue.Revenue.gross_potential_rent) properties)

let portfolio_vacancy_loss =
  sum_accruals (List.map (fun p -> p.Property.revenue.Revenue.vacancy_loss) properties)

let portfolio_egi =
  sum_accruals (List.map (fun p -> p.Property.revenue.Revenue.effective_gross_income) properties)

(* Portfolio totals - Operating Expenses *)
let portfolio_property_taxes =
  sum_accruals (List.map (fun p -> p.Property.opex.Opex.property_taxes) properties)

let portfolio_insurance =
  sum_accruals (List.map (fun p -> p.Property.opex.Opex.insurance) properties)

let portfolio_utilities =
  sum_accruals (List.map (fun p -> p.Property.opex.Opex.utilities) properties)

let portfolio_repairs =
  sum_accruals (List.map (fun p -> p.Property.opex.Opex.repairs_maintenance) properties)

let portfolio_management =
  sum_accruals (List.map (fun p -> p.Property.opex.Opex.property_management) properties)

let portfolio_janitorial =
  sum_accruals (List.map (fun p -> p.Property.opex.Opex.janitorial) properties)

let portfolio_landscaping =
  sum_accruals (List.map (fun p -> p.Property.opex.Opex.landscaping) properties)

let portfolio_security = sum_accruals (List.map (fun p -> p.Property.opex.Opex.security) properties)
let portfolio_opex = sum_accruals (List.map (fun p -> p.Property.opex.Opex.total) properties)

(* Portfolio totals - NOI *)
let portfolio_noi = sum_accruals (List.map (fun p -> p.Property.noi) properties)

(* Portfolio totals - CapEx *)
let portfolio_capital_reserves =
  sum_accruals (List.map (fun p -> p.Property.capex.Capex.capital_reserves) properties)

let portfolio_tenant_improvements =
  sum_accruals (List.map (fun p -> p.Property.capex.Capex.tenant_improvements) properties)

let portfolio_leasing_commissions =
  sum_accruals (List.map (fun p -> p.Property.capex.Capex.leasing_commissions) properties)

let portfolio_capex = sum_accruals (List.map (fun p -> p.Property.capex.Capex.total) properties)

(* Portfolio totals - Cash Flow *)
let portfolio_cfbf = sum_accruals (List.map (fun p -> p.Property.cfbf) properties)

(* Portfolio totals - Debt *)
let portfolio_interest =
  sum_accruals (List.map (fun p -> p.Property.debt.Debt.interest_expense) properties)

let portfolio_principal =
  sum_accruals (List.map (fun p -> p.Property.debt.Debt.principal_payment) properties)

let portfolio_debt_service =
  sum_accruals (List.map (fun p -> p.Property.debt.Debt.total_debt_service) properties)

let portfolio_cfaf = sum_accruals (List.map (fun p -> p.Property.cfaf) properties)

(* =====================================================
   PORTFOLIO STATEMENT
   ===================================================== *)

let portfolio_statement =
  Statement.group "Real Estate Portfolio Pro Forma"
    [
      Statement.group ~total:portfolio_gpr "Gross Potential Rent"
        [
          Statement.line "Base Rent" portfolio_base_rent;
          Statement.line "Parking Income" portfolio_parking;
          Statement.line "CAM Recoveries" portfolio_cam_recoveries;
          Statement.line "Other Income" portfolio_other_income;
        ];
      Statement.line "Less: Vacancy & Credit Loss" portfolio_vacancy_loss;
      Statement.line "Effective Gross Income" portfolio_egi;
      Statement.group ~total:portfolio_opex "Operating Expenses"
        [
          Statement.line "Property Taxes" portfolio_property_taxes;
          Statement.line "Insurance" portfolio_insurance;
          Statement.line "Utilities" portfolio_utilities;
          Statement.line "Repairs & Maintenance" portfolio_repairs;
          Statement.line "Property Management" portfolio_management;
          Statement.line "Janitorial" portfolio_janitorial;
          Statement.line "Landscaping" portfolio_landscaping;
          Statement.line "Security" portfolio_security;
        ];
      Statement.line "Net Operating Income (NOI)" portfolio_noi;
      Statement.group ~total:portfolio_capex "Capital Expenditures"
        [
          Statement.line "Capital Reserves" portfolio_capital_reserves;
          Statement.line "Tenant Improvements" portfolio_tenant_improvements;
          Statement.line "Leasing Commissions" portfolio_leasing_commissions;
        ];
      Statement.line "Cash Flow Before Financing" portfolio_cfbf;
      Statement.group ~total:portfolio_debt_service "Debt Service"
        [
          Statement.line "Interest Expense" portfolio_interest;
          Statement.line "Principal Payment" portfolio_principal;
        ];
      Statement.line "Cash Flow After Financing" portfolio_cfaf;
    ]

(* =====================================================
   OUTPUT
   ===================================================== *)

let print_statement ~periods item =
  let hdr p =
    Printf.sprintf "%14s" (CalendarLib.Printer.Date.sprint "%Y-%m-%d" p.Period.start_date)
  in
  let fmt v = Printf.sprintf "%14.0f" v in
  let print_row label seq =
    Printf.printf "%-35s%s\n" label
      (String.concat "" (List.map fmt (Accrual.accrue_periods periods seq)))
  in
  Printf.printf "%-35s%s\n" "" (String.concat "" (List.map hdr periods));
  Printf.printf "%s\n" (String.make (35 + (14 * List.length periods)) '=');
  let indent = ref 0 in
  Statement.iter item
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

let print_portfolio_assumptions () =
  Printf.printf "PORTFOLIO ASSUMPTIONS\n";
  Printf.printf "=====================\n";
  Printf.printf "All properties use the same assumptions:\n\n";
  let a = Property.default_assumptions in
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
  Printf.printf "\n";
  Printf.printf "PORTFOLIO COMPOSITION\n";
  Printf.printf "=====================\n";
  Printf.printf "Number of Properties:    %d\n" (List.length properties);
  Printf.printf "Total Building SF:       %.0f SF\n"
    (a.building_sf *. float_of_int (List.length properties));
  Printf.printf "Total Purchase Price:    $%.0f\n"
    (a.purchase_price *. float_of_int (List.length properties));
  Printf.printf "Total Loan Amount:       $%.0f\n"
    (loan_amount *. float_of_int (List.length properties));
  Printf.printf "\n\n"

let () =
  print_portfolio_assumptions ();
  Printf.printf "PORTFOLIO PRO FORMA CASH FLOW PROJECTION (5-Year)\n";
  Printf.printf "=================================================\n\n";
  let periods =
    Period.make_seq ~start_date ~offset:output_freq |> Seq.take output_periods |> List.of_seq
  in
  print_statement ~periods portfolio_statement
