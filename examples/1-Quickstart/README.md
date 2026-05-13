# Orcaset Quickstart

This example introduces orcaset by way of example. It builds a minimal model with three line items in the following structure:

```text
Profit: Sum of revenue and expenses
 ├── Revenue: Initial revenue of 1,000 in Q1 2026 growing at 3% each quarter thereafter
 └── Expenses: -50% of revenue
```

## Model

The full model is defined in around 20 lines of code and can be queried for values over any arbitrary date range.

```ocaml
let sum_agg = Series.Agg.sum
let sp = Series.Spans.pack

let rec seq_take n seq () =
  if n <= 0 then Seq.Nil
  else match seq () with Seq.Nil -> Seq.Nil | Seq.Cons (x, rest) -> Seq.Cons (x, seq_take (n - 1) rest)

let revenue =
  Series.Spans.unfold_rec ~label:"Revenue" ~agg:sum_agg
    (* Revenue unfolds over periods. Set the initial period. *)
    ~init:initial_period
    (* The step function returns a (revenue accrual, next period) tuple. 
       Formulas can query other series, including this series through `self`. *)
    ~cells:(fun ~self period ->
      let formula =
        (* If the current period is the start period, return the initial value. *)
        if Period.equal period initial_period then Series.Formula.pure (Some 1_000.0)
        (* Otherwise, look up the revenue over the prior period and grow it by the quarterly rate. *)
        else
          let open Series.Formula in
          let+ prior_revenue =
            span_query self ~period:(Period.prev qtr_lookback period)
          in
          Option.map (fun prior_revenue -> prior_revenue *. 1.03) prior_revenue
      in
      Some
        ( Series.Spans.cell ~period ~split:Split.daily formula, Period.next qtr_offset period ))
    ()

(* Expenses are defined as a constant (negative) percent of revenue. *)
let expenses = Series.Spans.scale ~label:"Expenses" (-0.50) revenue
(* Sum revenue and expense to get profit. *)
let profit = Series.Spans.sum ~label:"Profit" ~agg:sum_agg [ sp revenue; sp expenses ]
```

## Output

Table for quarterly values over the first 8 quarters.

```ocaml
let () =
  (* Create a series of periods to query over *)
  let output_periods = 8 in
  let periods =
    Period.make_seq ~start:initial_period_start ~offset:qtr_offset
    |> seq_take output_periods |> List.of_seq
  in
  (* Create a statement and evaluate it over the periods *)
  let stmt = Stmt.span_total profit [ Stmt.span_line revenue; Stmt.span_line expenses ] in
  let resolved = Stmt.eval_periods periods stmt in
  (* Print the statement using a fixed-width formatter *)
  Printf.printf "\n%s\n\n" (Stmt.fixed_width resolved)

(* 
            2026-03-31  2026-06-30  2026-09-30  2026-12-31  2027-03-31  2027-06-30  2027-09-30  2027-12-31
  Revenue      1000.00     1030.00     1060.90     1092.73     1125.51     1159.27     1194.05     1229.87
  Expenses     -500.00     -515.00     -530.45     -546.36     -562.75     -579.64     -597.03     -614.94
            ----------  ----------  ----------  ----------  ----------  ----------  ----------  ----------
Profit          500.00      515.00      530.45      546.36      562.75      579.64      597.03      614.94 *)
```

We can also query over arbitrary date ranges without changing the model since Orcaset will automatically interpolate partial periods.

The query below spans multiple quarters with boundary dates at mid-quarter points. Even though revenue (and subsequently expenses and profit) are defined over calendar quarter intervals, Orcaset will automatically interpolate and sum values. Mid-period interpolation is determined by the `~split` value in the step function. In this case, it is proportional by days.

```ocaml
let () =
  let query_period = Period.make (Date.make 2026 4 15) (Date.make 2026 9 15) in
  let cache = Series.make_cache () in
  let profit_opt = Series.query_span cache profit ~period:query_period in
  let profit = Option.map (Printf.sprintf "%.2f") profit_opt |> Option.value ~default:"n/a" in
  Printf.printf "Total profit over the period %s: %s\n" (Period.to_string query_period) profit

(* Total profit over the period 2026-04-15..2026-09-15: 874.07 *)
```
