# Account Balance

This example models an interest bearing account. It shows how to work point-in-time values as well as over-time values ("span" series). The model has the following structure.

* **Balance** - The account balance with an initial value of 1,000.
* **Interest** - Interest accruing on the principal balance at a rate of 5% annually, compounding quarterly.

The output shows quarterly values from Dec. 21, 2025 through the following year for the balance and interest income.

The output also shows the quarter-to-date balance and accrued interest as of March 15, 2026. Orcaset automatically handles mid-period interpolation, meaning users can query any date or date range without changing the model.

## Model

The full model is just 12 lines of code, excluding assumption definitions and print outs.

```ocaml
let rec interest =
  Span_series.Unfold
    {
      id = new_id ();
      (* Define `balance` as a dependency. *)
      deps = (fun () -> Deps.point_dep balance);
      (* `interest` unfolds over periods. Set the initial period below. *)
      init = initial_period;
      (* The step function returns a (intereste accrual, next period) tuple. 
         Dependency readers from `deps` are passed to the step function at execution. *)
      step =
        (fun get_balance period ->
          let interest () =
            let starting_balance = get_balance ~date:(Period.start period) ~default:0.0 in
            let yf = year_frac (Period.end_ period) (Period.start period) in
            starting_balance *. yf *. annual_interest_rate
          in
          Some (step ~period ~split:proportional_split interest, Period.next qtr_offset period));
    }

(* The balance is calculated summing the initial balance with accumulated interest. *)
and balance = Point_series.Accum { id = new_id (); init = initial_balance; changes = interest }
```

## Output

Output tables for balances and interest accruals.

```txt
                     2025-12-31   2026-03-31   2026-06-30   2026-09-30   2026-12-31
           Balance      1000.00      1012.50      1025.30      1038.40      1051.67

                     2026-03-31   2026-06-30   2026-09-30   2026-12-31
  Interest accrual        12.50        12.80        13.10        13.27
```

Output for query at mid-period date.

```txt
As of 2026-03-15
  Balance:              1010.28
  QTD interest accrual: 10.28
```