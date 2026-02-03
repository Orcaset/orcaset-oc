(* Debt Service Module for Commercial Real Estate Pro Forma
   
   Mortgage debt modeling:
   - Fixed-rate amortizing loan
   - Principal and interest payments
   - Loan balance tracking
*)

module Date = CalendarLib.Date
module Transaction = Orcaset.Transaction
module Balance_series = Orcaset.Balance_series
module Period = Orcaset.Period
module Accrual = Orcaset.Accrual

type yf = Date.t -> Date.t -> float

type t = {
  balance : Balance_series.t;
  interest_expense : Accrual.t Seq.t;
  principal_payment : Accrual.t Seq.t;
  total_debt_service : Accrual.t Seq.t;
  loan_amount : float;
  annual_rate : float;
  start_date : Date.t;
  term_months : int;
}

let calc_monthly_payment ~loan_amount ~annual_rate ~term_months =
  let monthly_rate = annual_rate /. 12.0 in
  let n = float_of_int term_months in
  let factor = (1.0 +. monthly_rate) ** n in
  loan_amount *. (monthly_rate *. factor) /. (factor -. 1.0)

let make ~loan_amount ~annual_rate ~term_months ~start_date ~yf =
  let monthly_payment = calc_monthly_payment ~loan_amount ~annual_rate ~term_months in
  let periods =
    Period.make_seq ~start_date ~offset:(Period.make_offset ~months:1 ())
    |> Seq.take term_months |> Seq.memoize
  in
  (* Build lazy circular references for loan amortization *)
  let rec lazy_balance_series =
    lazy
      (Balance_series.from_transactions ~initial_date:start_date
         ~initial_value:(lazy loan_amount)
         lazy_principal_txns)
  and lazy_interest_txns =
    lazy
      (Seq.map
         (fun period ->
           let beg_date = period.Period.start_date in
           let end_date = period.Period.end_date in
           let bal = Balance_series.on (Lazy.force lazy_balance_series) beg_date in
           let days_frac = yf beg_date end_date in
           let interest_value =
             -.Lazy.force bal.Orcaset.Balance.value *. annual_rate *. days_frac
           in
           Transaction.make ~date:end_date ~value:(lazy interest_value))
         periods
      |> Seq.memoize)
  and lazy_total_txns =
    lazy
      (Seq.map
         (fun period ->
           Transaction.make ~date:period.Period.end_date ~value:(lazy (-.monthly_payment)))
         periods)
  and lazy_principal_txns =
    lazy
      (Transaction.combine_seq ( -. ) (Lazy.force lazy_total_txns) (Lazy.force lazy_interest_txns)
      |> Seq.memoize)
  in
  let balance = Lazy.force lazy_balance_series in
  let interest_txns = Lazy.force lazy_interest_txns in
  let principal_txns = Lazy.force lazy_principal_txns in
  (* Convert transactions to accruals for consistent statement output *)
  let interest_expense =
    Seq.map2
      (fun period txn ->
        Accrual.make ~period
          ~value:(lazy (Lazy.force txn.Transaction.value))
          ~split_fn:Accrual.default_split_fn)
      periods interest_txns
    |> Seq.memoize
  in
  let principal_payment =
    Seq.map2
      (fun period txn ->
        Accrual.make ~period
          ~value:(lazy (Lazy.force txn.Transaction.value))
          ~split_fn:Accrual.default_split_fn)
      periods principal_txns
    |> Seq.memoize
  in
  let total_debt_service = Accrual.sum_seq interest_expense principal_payment |> Seq.memoize in
  {
    balance;
    interest_expense;
    principal_payment;
    total_debt_service;
    loan_amount;
    annual_rate;
    start_date;
    term_months;
  }
