open Orcaset

let start_date = CalendarLib.Date.make 2025 1 1
let freq = Period.make_offset ~months:1 ()
let yf = Yf.actual_360
let initial_revenue = 1000.0
let revenue_growth_rate = 0.05
let cogs_pct = 0.30
let opex_monthly = 200.0
let tax_rate = 0.20
let capex_pct = 0.05
let depreciation_rate = 0.10
let initial_cash = 1000.0
let initial_ppe = 10000.0
let common_stock_amount = 5000.0

let rec income_statement =
  lazy
    (Income_statement.make ~start_date ~freq ~yf ~initial_revenue ~revenue_growth_rate ~cogs_pct
       ~opex_monthly ~tax_rate ~depreciation_rate ~initial_ppe
       ~capex_seq_lazy:(lazy (Lazy.force cash_flow_statement).Cash_flow_statement.capex))

and cash_flow_statement =
  lazy (Cash_flow_statement.make ~income_statement_lazy:income_statement ~capex_pct)

and balance_sheet =
  lazy
    (Balance_sheet.make ~start_date ~initial_cash ~initial_ppe ~common_stock_amount
       ~cash_flow_statement_lazy:cash_flow_statement ~income_statement_lazy:income_statement)

let income_stmt = Lazy.force income_statement
let cash_flow_stmt = Lazy.force cash_flow_statement
let balance_stmt = Lazy.force balance_sheet

let financial_statement =
  let open Statement in
  group "Financial Model"
    [
      group "Income Statement"
        [
          group ~total:income_stmt.Income_statement.gross_profit.total "Gross Profit"
            [
              line "Revenue" income_stmt.Income_statement.gross_profit.revenue;
              line "COGS" income_stmt.Income_statement.gross_profit.cogs;
            ];
          line "Opex" income_stmt.Income_statement.opex;
          line "Depreciation" income_stmt.Income_statement.depreciation;
          line "Tax" income_stmt.Income_statement.tax;
          line "Net Income" income_stmt.Income_statement.net_income;
        ];
      group "Cash Flow Statement"
        [
          group ~total:cash_flow_stmt.Cash_flow_statement.cf_ops "Operations"
            [
              line "Net Income Add Back" cash_flow_stmt.Cash_flow_statement.net_income_add_back;
              line "Depreciation Add Back" cash_flow_stmt.Cash_flow_statement.depreciation_add_back;
            ];
          group ~total:cash_flow_stmt.Cash_flow_statement.cf_invest "Investing"
            [ line "Capex" cash_flow_stmt.Cash_flow_statement.capex ];
          line "CF Financing" cash_flow_stmt.Cash_flow_statement.cf_finance;
          line "Net Cash Change" cash_flow_stmt.Cash_flow_statement.net_cash_change;
        ];
    ]

let balance_sheet_statement =
  let open Statement in
  group "Balance Sheet"
    [
      group ~total:balance_stmt.Balance_sheet.total_assets "Assets"
        [
          line "Cash" balance_stmt.Balance_sheet.cash;
          line "PPE Net" balance_stmt.Balance_sheet.ppe_net;
        ];
      group ~total:balance_stmt.Balance_sheet.total_liabilities_equity "Liabilities & Equity"
        [
          line "Common Stock" balance_stmt.Balance_sheet.common_stock;
          line "Retained Earnings" balance_stmt.Balance_sheet.retained_earnings;
        ];
      group "Check" [ line "Balance Check" balance_stmt.Balance_sheet.balance_check ];
    ]

(* Printing *)
let print_accrual_statement ~output_periods item =
  let hdr p = Printf.sprintf "%14s" (CalendarLib.Printer.Date.sprint "%Y-%m" p.Period.end_date) in
  let fmt v = Printf.sprintf "%14.2f" v in
  let print_row label seq =
    Printf.printf "%-30s%s\n" label
      (String.concat "" (List.map fmt (Accrual.accrue_periods output_periods seq)))
  in
  Printf.printf "%-30s%s\n" "" (String.concat "" (List.map hdr output_periods));
  Printf.printf "%s\n" (String.make (30 + (14 * List.length output_periods)) '-');
  let indent = ref 0 in
  Statement.iter item
    ~line_fn:(fun label seq -> print_row (String.make !indent ' ' ^ label) seq)
    ~group_fn:(fun label total phase ->
      match phase with
      | `Enter ->
          Printf.printf "%s%s:\n" (String.make !indent ' ') label;
          indent := !indent + 2
      | `Exit ->
          Option.iter (print_row (String.make (!indent - 2) ' ' ^ "Total " ^ label)) total;
          print_newline ();
          indent := !indent - 2)

let print_balance_statement ~output_periods item =
  let hdr p = Printf.sprintf "%14s" (CalendarLib.Printer.Date.sprint "%Y-%m" p.Period.end_date) in
  let fmt v = Printf.sprintf "%14.2f" v in
  let print_row label seq =
    let values =
      List.map (fun p -> Lazy.force (Balance_series.on seq p.Period.end_date).value) output_periods
    in
    Printf.printf "%-30s%s\n" label (String.concat "" (List.map fmt values))
  in
  Printf.printf "%-30s%s\n" "" (String.concat "" (List.map hdr output_periods));
  Printf.printf "%s\n" (String.make (30 + (14 * List.length output_periods)) '-');
  let indent = ref 0 in
  Statement.iter item
    ~line_fn:(fun label seq -> print_row (String.make !indent ' ' ^ label) seq)
    ~group_fn:(fun label total phase ->
      match phase with
      | `Enter ->
          Printf.printf "%s%s:\n" (String.make !indent ' ') label;
          indent := !indent + 2
      | `Exit ->
          Option.iter (print_row (String.make (!indent - 2) ' ' ^ "Total " ^ label)) total;
          print_newline ();
          indent := !indent - 2)

let () =
  let output_freq = Period.make_offset ~months:1 () in
  let output_periods =
    Period.make_seq ~start_date ~offset:output_freq |> Seq.take 12 |> List.of_seq
  in

  Printf.printf "\n";
  Printf.printf "================================================================================\n";
  Printf.printf "                     SIMPLE 3-STATEMENT FINANCIAL MODEL\n";
  Printf.printf "================================================================================\n";
  Printf.printf "\n";

  print_accrual_statement ~output_periods financial_statement;
  print_balance_statement ~output_periods balance_sheet_statement
