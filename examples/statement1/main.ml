open Orcaset

let start_date = CalendarLib.Date.make 2025 1 1
let freq = Period.make_offset ~months:1 ()
let yf = Yf.actual_360
let revenue_initial = 1000.0
let revenue_growth = 0.05
let cogs_pct = -0.30
let opex_monthly = -200.0
let tax_rate = 0.20
let depreciation_rate = 0.10
let capex_pct = -0.05
let initial_cash = 1000.0
let initial_ppe = 10000.0
let common_stock_amount = 5000.0
let initial_retained_earnings = initial_cash +. initial_ppe -. common_stock_amount

let rec income_stmt_lazy =
  lazy
    (Income_statement.make ~start_date ~freq ~yf ~revenue_initial ~revenue_growth ~cogs_pct
       ~opex_monthly ~tax_rate ~depreciation_rate ~ppe_net_lazy)

and cash_flow_lazy = lazy (Cash_flow.make ~start_date ~freq ~income_stmt_lazy ~capex_pct)

and balance_sheet_lazy =
  lazy
    (Balance_sheet.make ~start_date ~freq ~initial_cash ~initial_ppe ~common_stock_amount
       ~initial_retained_earnings ~cash_flow_lazy ~income_stmt_lazy)

and ppe_net_lazy = lazy (Lazy.force balance_sheet_lazy).Balance_sheet.ppe_net

let income_stmt = Lazy.force income_stmt_lazy
let cash_flow = Lazy.force cash_flow_lazy
let balance_sheet = Lazy.force balance_sheet_lazy
let output_freq = Period.make_offset ~months:1 ()
let periods = Period.make_seq ~start_date ~offset:output_freq |> Seq.take 12 |> List.of_seq

let income_statement =
  let open Statement in
  let gp = income_stmt.Income_statement.gross_profit in
  group "Income Statement"
    [
      group ~total:gp.Gross_profit.total "Gross Profit"
        [ line "Revenue" gp.Gross_profit.revenue; line "COGS" gp.Gross_profit.cogs ];
      line "Opex" income_stmt.Income_statement.opex;
      line "Depreciation" income_stmt.Income_statement.depreciation;
      line "Tax" income_stmt.Income_statement.tax;
      line "Net Income" income_stmt.Income_statement.net_income;
    ]

let cash_flow_statement =
  let open Statement in
  group "Cash Flow Statement"
    [
      group ~total:cash_flow.Cash_flow.cf_ops "CF from Operations"
        [
          line "Net Income Add Back" cash_flow.Cash_flow.net_income_add_back;
          line "Depreciation Add Back" cash_flow.Cash_flow.depreciation_add_back;
        ];
      group ~total:cash_flow.Cash_flow.cf_invest "CF from Investing"
        [ line "Capex" cash_flow.Cash_flow.capex ];
      line "CF from Financing" cash_flow.Cash_flow.cf_finance;
      line "Net Cash Change" cash_flow.Cash_flow.net_cash_change;
    ]

let balance_sheet_statement =
  let open Statement in
  group "Balance Sheet"
    [
      group ~total:balance_sheet.Balance_sheet.total_assets "Assets"
        [
          line "Cash" balance_sheet.Balance_sheet.cash;
          line "PPE Net" balance_sheet.Balance_sheet.ppe_net;
        ];
      group ~total:balance_sheet.Balance_sheet.total_liabilities_equity "Liabilities & Equity"
        [
          line "Common Stock" balance_sheet.Balance_sheet.common_stock;
          line "Retained Earnings" balance_sheet.Balance_sheet.retained_earnings;
        ];
      line "Balance Check" balance_sheet.Balance_sheet.balance_check;
    ]

let print_header () =
  let hdr p =
    Printf.sprintf "%14s" (CalendarLib.Printer.Date.sprint "%Y-%m-%d" p.Period.start_date)
  in
  Printf.printf "%-30s%s\n" "" (String.concat "" (List.map hdr periods))

let print_accrual_statement stmt =
  let indent = ref 0 in
  Statement.iter stmt
    ~line_fn:(fun label seq ->
      let values = Accrual.accrue_periods periods seq in
      let fmt v = Printf.sprintf "%14.2f" v in
      Printf.printf "%-30s%s\n"
        (String.make !indent ' ' ^ label)
        (String.concat "" (List.map fmt values)))
    ~group_fn:(fun label total phase ->
      match phase with
      | `Enter ->
          Printf.printf "%s\n" (String.make !indent ' ' ^ label);
          indent := !indent + 2
      | `Exit ->
          (match total with
          | Some seq ->
              let values = Accrual.accrue_periods periods seq in
              let fmt v = Printf.sprintf "%14.2f" v in
              Printf.printf "%-30s%s\n"
                (String.make (!indent - 2) ' ' ^ "Total " ^ label)
                (String.concat "" (List.map fmt values))
          | None -> ());
          indent := !indent - 2)

let print_balance_statement stmt =
  let indent = ref 0 in
  Statement.iter stmt
    ~line_fn:(fun label seq ->
      let values =
        List.map
          (fun p ->
            let bal = Balance.on seq p.Period.end_date in
            Lazy.force bal.Balance.amount)
          periods
      in
      let fmt v = Printf.sprintf "%14.2f" v in
      Printf.printf "%-30s%s\n"
        (String.make !indent ' ' ^ label)
        (String.concat "" (List.map fmt values)))
    ~group_fn:(fun label total phase ->
      match phase with
      | `Enter ->
          Printf.printf "%s\n" (String.make !indent ' ' ^ label);
          indent := !indent + 2
      | `Exit ->
          (match total with
          | Some seq ->
              let values =
                List.map
                  (fun p ->
                    let bal = Balance.on seq p.Period.end_date in
                    Lazy.force bal.Balance.amount)
                  periods
              in
              let fmt v = Printf.sprintf "%14.2f" v in
              Printf.printf "%-30s%s\n"
                (String.make (!indent - 2) ' ' ^ "Total " ^ label)
                (String.concat "" (List.map fmt values))
          | None -> ());
          indent := !indent - 2)

let () =
  print_header ();
  print_newline ();
  print_accrual_statement income_statement;
  print_newline ();
  print_accrual_statement cash_flow_statement;
  print_newline ();
  print_balance_statement balance_sheet_statement
