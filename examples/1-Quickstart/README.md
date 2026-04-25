# Orcaset Quickstart

This example shows how to build a minimal model with Orcaset. The model has three line items with the following structure:

```text
Profit: Sum of revenue and expenses
 ├── Revenue: Initial revenue of 1,000 in Q1 2026 growing at 3% each quarter thereafter
 └── Expenses: -50% of revenue
```

## Model

The full model is defined in 12 lines of code which can be queried for values over any arbitrary date range.

```ocaml
let rec revenue =
  Span_series.Unfold
    {
      id = new_id ();
      (* Declare revenue as a dependency of itself. *)
      deps = (fun () -> Deps.span_dep revenue);
      (* Revenue unfolds over periods. Set the initial period. *)
      init = initial_period;
      (* The step function returns a (revenue accrual, next period) tuple. 
         Dependency readers from `deps` are passed to the step function at execution. *)
      step =
        (fun query_rev period ->
          let value () =
            (* If the current period is the start period, return the initial value. *)
            if period = initial_period then initial_value
            else
              (* Otherwise, look up the revenue over the prior period and grow it by the quarterly rate. *)
              let prior_qtr_rev =
                query_rev ~period:(Period.prev lookback period) ~reduce:(Deps.reduce ( +. ) 0.0)
              in
              prior_qtr_rev *. (1. +. qtrly_growth_rate)
          in
          Some (step ~period ~split:proportional_split value, Period.next qtr_offset period));
    }

(* Expenses are defined as a constant (negative) percent of revenue. *)
let expenses = Span_series.scale (-.expense_margin) revenue
(* Sum revenue and expense to get profit. *)
let profit = Span_series.sum revenue expenses
```

## Output

Table for quarterly values over the first 8 quarters.

```txt
         2026-03-31  2026-06-30  2026-09-30  2026-12-31  2027-03-31  2027-06-30  2027-09-30  2027-12-31
Revenue     1000.00     1030.00     1060.90     1092.73     1125.51     1159.27     1194.05     1229.87
Expenses    -500.00     -515.00     -530.45     -546.36     -562.75     -579.64     -597.03     -614.94
Profit       500.00      515.00      530.45      546.36      562.75      579.64      597.03      614.94
```

We can also query over abitrary date ranges without changing the model since Orcaset will automatically interpolate partial periods.

The query below spans multiple quarters with boundary dates at mid-quarter points. Even though revenue (and subsequently expenses and profit) are defined over calendar quarter intervals, Orcaset will automatically interpolate and sum values. Mid-period interpolation is determined by the `~split` value in the step function. In this case, it is proportional by days.

```txt
Accrued totals from 2026-04-15 to 2026-09-15
Revenue:  1748.15
Expenses: -874.07
Profit:   874.07
```
