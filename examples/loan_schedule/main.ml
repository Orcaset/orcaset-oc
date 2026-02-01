(* Fixed-rate amortizing loan schedule example *)

let loan =
  Loan.make ~loan_amount:500000.0 ~annual_rate:0.065 ~term_months:360
    ~start_date:(CalendarLib.Date.make 2025 1 1) ~yf:Orcaset.Yf.thirty_360

let monthly_payment =
  let monthly_rate = loan.Loan.annual_rate /. 12.0 in
  let n = float_of_int loan.Loan.term_months in
  let factor = (1.0 +. monthly_rate) ** n in
  loan.Loan.loan_amount *. (monthly_rate *. factor) /. (factor -. 1.0)

let print_header () =
  Printf.printf "=== Fixed-Rate Amortizing Loan Schedule ===\n";
  Printf.printf "Loan Amount:     $%.2f\n" loan.Loan.loan_amount;
  Printf.printf "Annual Rate:     %.3f%%\n" (loan.Loan.annual_rate *. 100.0);
  Printf.printf "Term:            %d months (%.1f years)\n" loan.Loan.term_months
    (float_of_int loan.Loan.term_months /. 12.0);
  Printf.printf "Day Count:       30/360\n";
  Printf.printf "Monthly Payment: $%.2f\n" monthly_payment;
  Printf.printf "Start Date:      %s\n"
    (CalendarLib.Printer.Date.sprint "%Y-%m-%d" loan.Loan.start_date);
  Printf.printf "\n"

let print_schedule_header () =
  Printf.printf "%5s  %10s  %14s  %12s  %12s  %12s  %14s\n" "Month" "Date" "Beg Balance" "Payment"
    "Interest" "Principal" "End Balance";
  Printf.printf "%s\n" (String.make 89 '-')

let print_schedule () =
  let interest_list = List.of_seq loan.Loan.interest_pmt in
  let amort_list = List.of_seq loan.Loan.amort in
  let total_pmt_list = List.of_seq loan.Loan.total_pmt in
  let rec print_rows month interest_txns amort_txns total_txns =
    match (interest_txns, amort_txns, total_txns) with
    | [], [], [] -> ()
    | interest :: rest_i, amort :: rest_a, total :: rest_t ->
        let date = interest.Orcaset.Transaction.date in
        let interest_val = -.Lazy.force interest.Orcaset.Transaction.value in
        let principal_val = -.Lazy.force amort.Orcaset.Transaction.value in
        let payment_val = -.Lazy.force total.Orcaset.Transaction.value in
        let beg_balance =
          Orcaset.Balance_series.on loan.Loan.balance (CalendarLib.Date.prev date `Day)
        in
        let end_balance = Orcaset.Balance_series.on loan.Loan.balance date in
        Printf.printf "%5d  %10s  %14.2f  %12.2f  %12.2f  %12.2f  %14.2f\n" month
          (CalendarLib.Printer.Date.sprint "%Y-%m-%d" date)
          (Lazy.force beg_balance.Orcaset.Balance.value)
          payment_val interest_val principal_val
          (Lazy.force end_balance.Orcaset.Balance.value);
        print_rows (month + 1) rest_i rest_a rest_t
    | _ -> failwith "Mismatched transaction sequences"
  in
  print_rows 1 interest_list amort_list total_pmt_list

let print_confirm_principal_pmts () =
  let total_principal =
    Seq.fold_left
      (fun acc txn -> acc +. -.Lazy.force txn.Orcaset.Transaction.value)
      0.0 loan.Loan.amort
  in
  Printf.printf "\n=== Confirm Principal Payments ===\n";
  Printf.printf "Loan Amount: $%.2f\n" loan.Loan.loan_amount;
  Printf.printf "Total Repaid Principal: $%.2f\n" total_principal;
  Printf.printf "Difference: $%.2f\n" (loan.Loan.loan_amount -. total_principal);
  Printf.printf "\n"

let print_loan_balance date =
  let balance = Orcaset.Balance_series.on loan.Loan.balance date in
  Printf.printf "Balance on %s: $%.2f\n"
    (CalendarLib.Printer.Date.sprint "%Y-%m-%d" date)
    (Lazy.force balance.Orcaset.Balance.value)

let print_loan_balances () =
  let dates =
    [
      CalendarLib.Date.make 2025 1 1;
      CalendarLib.Date.make 2025 1 15;
      CalendarLib.Date.make 2028 2 4;
      CalendarLib.Date.make 2040 1 7;
    ]
  in
  Printf.printf "=== Loan Balance Queries ===\n";
  List.iter print_loan_balance dates;
  Printf.printf "\n"

let () =
  print_header ();
  print_loan_balances ();
  print_schedule_header ();
  print_confirm_principal_pmts ();
  print_schedule ()
