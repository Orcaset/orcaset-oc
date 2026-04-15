open Orcaset

(* --- Load CSV data -------------------------------------------------------- *)

let usage = "Usage: main <historical-is.csv> <historical-bs.csv> <historical-cf.csv>"

let is_path, bs_path, cf_path =
  if Array.length Sys.argv <> 4 then begin
    prerr_endline usage;
    exit 64
  end;
  (Sys.argv.(1), Sys.argv.(2), Sys.argv.(3))

let is_csv = Csv.quarterly_of_file is_path
let bs_csv = Csv.point_of_file bs_path
let cf_csv = Csv.quarterly_of_file cf_path

(* --- Create statements ------------------------------------ *)

let i, cf, bs =
  let rec ctx =
    {
      Types.i = lazy (Income.make is_csv ctx);
      cf = lazy (Cash_flow.make cf_csv bs_csv is_csv ctx);
      bs = lazy (Balance_sheet.make bs_csv ctx);
    }
  in
  (Lazy.force ctx.i, Lazy.force ctx.cf, Lazy.force ctx.bs)

(* --- Statement layout ----------------------------------------------------- *)

open Series.Stmt

let is_stmt =
  group
    [
      period_total i.net_earnings
        [
          period_total i.ebt
            [
              period_total i.ebit
                [
                  period_total i.gross_margin
                    [
                      period_total i.revenue
                        [
                          period_line i.net_product_sales; period_line i.rental_and_royalty_revenue;
                        ];
                      period_total i.cogs
                        [ period_line i.product_cogs; period_line i.rental_and_royalty_cost ];
                    ];
                  period_line i.sga;
                ];
              period_line i.other_income;
            ];
          period_line i.tax;
        ];
    ]

let cf_stmt =
  group
    [
      period_total cf.change_in_cash
        [
          period_total cf.cfo
            [
              period_line cf.net_earnings;
              period_line cf.depreciation;
              period_line cf.deferred_income_taxes;
              period_line cf.amortization_premiums;
              period_line cf.change_ar;
              period_line cf.change_other_receivables;
              period_line cf.change_inventories;
              period_line cf.change_prepaids;
              period_line cf.change_ap_and_accrued;
              period_line cf.change_income_taxes_payable;
              period_line cf.change_postretirement;
              period_line cf.change_deferred_comp;
            ];
          period_total cf.cfi
            [
              period_line cf.capex;
              period_line cf.purchases_trading;
              period_line cf.sales_trading;
              period_line cf.purchases_afs;
              period_line cf.sales_afs;
            ];
          period_total cf.cff
            [
              period_line cf.shares_repurchased;
              period_line cf.dividends_paid;
              period_line cf.proceeds_bank_loans;
              period_line cf.repayment_bank_loans;
            ];
          period_line cf.fx_effect;
        ];
    ]

let bs_stmt =
  group
    [
      point_total bs.total_assets
        [
          point_total bs.total_current_assets
            [
              point_line bs.cash;
              point_line bs.restricted_cash;
              point_line bs.investments_current;
              point_line bs.accounts_receivable;
              point_line bs.other_receivables;
              point_line bs.inventories;
              point_line bs.prepaid_expenses;
            ];
          point_line bs.net_ppe;
          point_total bs.total_other_assets
            [
              point_line bs.goodwill;
              point_line bs.trademarks;
              point_line bs.investments_lt;
              point_line bs.other_lt_assets;
              point_line bs.deferred_income_taxes_asset;
            ];
        ];
      point_total bs.total_liabilities_and_equity
        [
          point_total bs.total_current_liabilities
            [
              point_line bs.accounts_payable;
              point_line bs.bank_loans_current;
              point_line bs.dividends_payable;
              point_line bs.accrued_liabilities;
              point_line bs.postretirement_current;
              point_line bs.operating_lease_current;
              point_line bs.income_taxes_payable;
              point_line bs.deferred_compensation_current;
            ];
          point_total bs.total_noncurrent_liabilities
            [
              point_line bs.deferred_income_taxes_liability;
              point_line bs.postretirement_lt;
              point_line bs.industrial_dev_bond;
              point_line bs.uncertain_tax_positions;
              point_line bs.operating_lease_lt;
              point_line bs.deferred_comp_and_other;
            ];
          point_total bs.total_equity
            [
              point_total bs.total_shareholders_equity
                [
                  point_line bs.common_stock;
                  point_line bs.class_b_stock;
                  point_line bs.capital_in_excess;
                  point_line bs.retained_earnings;
                  point_line bs.accumulated_oci;
                  point_line bs.treasury_stock;
                ];
              point_line bs.noncontrolling_interests;
            ];
        ];
      point_line bs.balance_check;
    ]

(* --- Print ---------------------------------------------------------------- *)

let () =
  let print_periods =
    Period.make_seq ~start_date:(Date.make 2024 12 31)
      ~offset:(Offset.make ~months:3 ~month_end:true ())
    |> Seq.take 8
  in
  let periods = List.of_seq print_periods in
  print_string "=== INCOME STATEMENT ===\n\n";
  print_string (pp is_stmt periods);
  print_string "\n=== CASH FLOW STATEMENT ===\n\n";
  print_string (pp cf_stmt periods);
  print_string "\n=== BALANCE SHEET ===\n\n";
  print_string (pp bs_stmt periods)
