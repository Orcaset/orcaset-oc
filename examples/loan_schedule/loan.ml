(* Fixed-rate loan amortization model
   Uses configurable day count convention *)

module Date = CalendarLib.Date

type yf = Date.t -> Date.t -> float

type t = {
  balance : Orcaset.Balance_series.t;
  interest_pmt : Orcaset.Transaction.t Seq.t;
  amort : Orcaset.Transaction.t Seq.t;
  total_pmt : Orcaset.Transaction.t Seq.t;
  loan_amount : float;
  annual_rate : float;
  start_date : Date.t;
  term_months : int;
  yf : yf;
}

let calc_monthly_payment ~loan_amount ~annual_rate ~term_months =
  let monthly_rate = annual_rate /. 12.0 in
  let n = float_of_int term_months in
  let factor = (1.0 +. monthly_rate) ** n in
  loan_amount *. (monthly_rate *. factor) /. (factor -. 1.0)

let make ~loan_amount ~annual_rate ~term_months ~start_date ~yf =
  let monthly_payment = calc_monthly_payment ~loan_amount ~annual_rate ~term_months in
  let interest_periods =
    Orcaset.Period.make_seq ~start_date ~offset:(Orcaset.Period.make_offset ~months:1 ())
    |> Seq.take term_months
  in
  let rec lazy_balance_seq =
    lazy
      (Orcaset.Balance_series.from_transactions ~initial_date:start_date
         ~initial_value:(lazy loan_amount)
         lazy_amort)
  and lazy_interest_seq =
    lazy
      (Seq.map
         (fun period ->
           let beg_date = period.Orcaset.Period.start_date in
           let end_date = period.Orcaset.Period.end_date in
           let bal = Orcaset.Balance_series.on (Lazy.force lazy_balance_seq) beg_date in
           let days_frac = yf beg_date end_date in
           let interest_value =
             -.Lazy.force bal.Orcaset.Balance.value *. annual_rate *. days_frac
           in
           Orcaset.Transaction.make ~date:end_date ~value:(lazy interest_value))
         interest_periods
      |> Seq.memoize)
  and lazy_total_payment =
    lazy
      (Seq.map
         (fun period ->
           Orcaset.Transaction.make ~date:period.Orcaset.Period.end_date
             ~value:(lazy (-.monthly_payment)))
         interest_periods)
  and lazy_amort =
    lazy
      (Orcaset.Transaction.combine_seq ( -. ) (Lazy.force lazy_total_payment)
         (Lazy.force lazy_interest_seq)
      |> Seq.memoize)
  in
  let interest_pmt = Lazy.force lazy_interest_seq in
  let amort = Lazy.force lazy_amort in
  let total_pmt = Orcaset.Transaction.combine_seq ( +. ) amort interest_pmt in
  let balance =
    Orcaset.Balance_series.from_transactions ~initial_date:start_date
      ~initial_value:(lazy loan_amount)
      (lazy amort)
  in
  { balance; interest_pmt; amort; total_pmt; loan_amount; annual_rate; start_date; term_months; yf }
