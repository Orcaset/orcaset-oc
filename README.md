# Orcaset - Financial Modeling for AI

Orcaset is a programmatic financial modeling framework designed to safely orchestrate analysis at scale. It gives computers and AI systems a structured, deterministic, and transparent way to evaluate financial scenarios.

* **Build and Update with Confidence** - Strongly typed design protects against invalid models and helps users navigate models at scale. Confidently modify models without breaking them.
* **Integrate with any System** - Orcaset runs in an open environment, making it easy to integrate with other systems. Fetch data securely from web APIs, databases, documents, or any other source.
* **Scale Effortlessly** - Layered design allows users to quickly build simple analyses or scale to complex, portfolio-scale analysis.
* **Transparent and Deterministic** - Calculations are open and traceable. Models can be version controlled, audited, and shared. No black boxes.
* **Limitless Scenarios** - Programmatic models aren't limited by two-dimensional grids. Run high-dimensional sensitivity and evaluate complex, conditional scenarios with ease.

## Installation

Add to an existing `opam` switch with:

```sh
opam pin add orcaset https://github.com/Orcaset/orcaset-oc.git
```

## Getting Started

*See the [Orcaset Examples](https://github.com/Orcaset/orcaset-oc-examples) repository for complete working models.*

Orcaset models can be built incrementally. They can start as ad-hoc scripts and evolve into highly structured models as they grow. Users coming from spreadsheets will find that Orcaset components map naturally to familiar modeling concepts.

### Create a Line Item

Orcaset models are built around series of data over time representing financial line items. They are analogous to rows in a financial statement. The series are lazy evaluated and may be infinite.

The most basic way to create a series of values is by using OCaml's built-in `Seq` module.

```ocaml
(* Infinite sequence starting at 1000 and growing by 10% each period *)
let revenue = Seq.iterate (fun prev_rev -> prev_rev *. 1.10) 1000.0
```

Rather than iterating over raw numbers, you can add temporal context by associating values in a sequence with a date. Orcaset provides three types to work with time-based values:

* `Accrual`: Flow *between* two dates, consisting of a period, lazy value, and split function
* `Transaction`: Flow on a *specific* date, consisting of a date and lazy value
* `Balance`: Snapshot on a specific date, consisting of a date and lazy value

```ocaml
open Orcaset

let start_date = CalendarLib.Date.make 2025 1 1
let initial_value = 1000.0

(* Create a sequence of Accruals starting on 2025-01-01 at 1000 
and growing at 10% annually, compounding monthly *)
let revenue = Seq.unfold
  (fun (period_start, current_value) ->
    (* Create an Accrual for each period *)
    let period_end = Date.add period_start (Date.Period.month 1) in
    let accrual = Accrual.make
      ~period:(Period.make ~start_date:period_start ~end_date:period_end)
      ~value:(lazy current_value)
      ~split_fn:Accrual.default_split_fn
    in
    (* Return the current accrual and inputs to create the next accrual *)
    Some (accrual, (period_end, current_value *. (1.0 +. 0.10 /. 12.0))))
  (* Starting inputs *)
  (start_date, initial_value)
```

`revenue` is now a sequence of `Accrual.t` values, where each node in the sequence similar to a cell in a spreadsheet. However, unlike a spreadsheet where time periods must be the same across rows and are typically a fixed interval, periods in accruals *between* sequences and even *within* a sequence can vary in duration. Orcaset automatically interpolates values when combining sequences with mismatched periods. This flexibility allows users to model real-world financials more accurately.

> **Note**: Orcaset does require sequences to be in chronological order without overlapping periods.

### Composing Line Items

Transform and combine sequences to create multi-line models.

```ocaml
(* Expense accruals defined as 30% of revenue *)
let expenses = Seq.map (fun rev -> Accrual.map (fun v -> v *. -0.30) rev) revenue

(* Sum of revenue and expenses *)
let income = Accrual.sum_seq revenue expenses
```

The model now has three interconnected sequences for `revenue`, `expenses`, and `income`. As the model grows, you can keep it organized by grouping related line items into modules.

```ocaml
module Income = struct
  type t = { revenue : Accrual.t Seq.t; expenses : Accrual.t Seq.t; total : Accrual.t Seq.t }

  let make ~revenue_seq ~expenses_seq =
    let total = Accrual.sum_seq revenue_seq expenses_seq in
    { revenue = revenue_seq; expenses = expenses_seq; total }
end
```

**Note on Balance Types**

While you *could* represent series of balances using the `Seq` module like we did for accruals, they would be hard to work with reliably. You would either have to create dense (i.e. daily) sequences or guarantee that the program only queries balances on dates that exist in the sequence. Instead, Orcaset provide the `Balance_series` module for working with series of balances. It allows users to define series of balances based on a starting value and a series of flows that determine how the balance changes over time. It automatically handles calculating balances for arbitrary dates when queried.

```ocaml
(* Constant 50.0 increase each month. *)
let interest = Seq.map
  (fun period -> 
    Accrual.make
      ~period
      ~value:(lazy 50.0)
      ~split_fn:Accrual.default_split_fn)
  (Period.make_seq
    ~start_date:(Date.make 2025 1 1)
    ~offset:(Period.make_offset ~months:1 ()))

(* Account balance starts at 1000.0 and grows by `interest` *)
let balance = Balance_series.from_flow
  ~initial_date:(Date.make 2025 1 1)
  ~initial_value:(lazy 1000.0)
  ~sum_between:(fun ~start_date ~end_date -> Accrual.accrue start_date end_date interest)
```

We can query `balance` for any future date, and it will calculate the interpolated balance including accrued interest to that date.

```ocaml
Balance_series.on balance (CalendarLib.Date.make 2026 12 31) |> Balance.to_string
(* "Date: 2026-12-31, Amount: 2198.39" *)
```

### Working with Mutually-Dependent Line Items

Orcaset uses lazy evaluation to handle mutually dependent sequences. For example, suppose revenue starts at 100 and grows based on the prior period's marketing costs, while marketing costs are a percentage of current period revenue:

```ocaml
let initial_period = Period.make ~start_date:(Date.make 2025 1 1) ~end_date:(Date.make 2025 2 1)
let initial_revenue = 100.0
let marketing_multiplier = 12.0
let marketing_pct = 0.10
let monthly = Period.make_offset ~months:1 ()

let rec revenue = lazy (Seq.cons
  (* Initial revenue accrual *)
  (Accrual.make ~period:initial_period
    ~value:(lazy initial_revenue)
    ~split_fn:Accrual.default_split_fn)
  (* Future revenue accruals based on marketing costs *)
  (fun () ->
    Seq.map
      (fun prior_marketing_cost ->
        Accrual.make
          ~period: (Period.add_offset monthly prior_marketing_cost.Accrual.period)
          ~value:
            (lazy
              (1.0 +. (marketing_multiplier *. Lazy.force prior_marketing_cost.Accrual.value)))
          ~split_fn:Accrual.default_split_fn)
      (Lazy.force marketing_costs) ()))
(* Define marketing costs as a percentage of revenue *)
and marketing_costs =
  lazy (Seq.map (fun rev -> Accrual.map (fun v -> v *. marketing_pct) rev) (Lazy.force revenue))
```

For mutually dependent sequences to resolve correctly, there must be a starting point for at least one sequence, and no circular dependencies within the same time period.

### Memoizing Sequences

Consider a report where we want to show the total transaction value for sales on a quarterly basis. A sale could occur on any given day. In code, this looks like:

```ocaml
let start_date = CalendarLib.Date.make 2025 1 1
let sale_transactions = Seq.unfold ... (* sequence of sales transactions *)

let quarterly_periods = Period.make_seq
  ~start_date
  ~offset:(Period.make_offset ~months:3 ())

let quarterly_sales = Seq.map
  (fun period ->
    let total_sales = Transaction.sum_over sale_transactions ~start_date:period.start_date ~end_date:period.end_date)
  quarterly_periods
```

While this code works, it is inefficient. For each quarterly period, it iterates over the sequence of sale transactions from the beginning and recalculates sale values each time. For complex models or long sequences, this pattern can become a performance bottleneck.

The easiest solution if code execution becomes too slow is memoizing the underlying sequences.

```ocaml
let quarterly_sales = Seq.map
  (fun period ->
    let total_sales = Transaction.sum_over sale_transactions ~start_date:period.start_date ~end_date:period.end_date)
  (* Map over the memoized sequence of transactions. *)
  (Seq.memoize quarterly_periods)  
```

You should take care not to memoize sequences with side effects, but sequences should generally be pure functions.

### Structuring Output with Statement

The `Statement` module structures outputs into hierarchical views:

```ocaml
let stmt =
  let open Statement in
  group "Income Statement"
    [
      line "Revenue" revenue;
      group ~total:opex.total "Operating Expenses"
        [
          line "COGS" opex.cogs;
          line "Salaries" opex.salaries;
        ];
      line "Net Income" net_income;
    ]
```

Sequences can be added to multiple statements to build different views of the same underlying data.

You can also uses `Statement` to create structured and formatted output by iterating over the groups and lines in a statement:

```ocaml
Statement.iter stmt
  ~line_fn:(fun label accrual_seq ->
    Seq.iter
      (fun accrual ->
        let value = Lazy.force accrual.Accrual.value in
        Printf.printf "%s | %s to %s | %.2f\n"
          label
          (CalendarLib.Printer.Date.sprint "%Y-%m-%d" accrual.Accrual.period.start_date)
          (CalendarLib.Printer.Date.sprint "%Y-%m-%d" accrual.Accrual.period.end_date)
          value)
      accrual_seq)
  ~group_fn:(fun _ _ _ -> ())
```

### Next Steps

See the [`Orcaset Examples`](https://github.com/Orcaset/orcaset-oc-examples) repository for complete working models.

## License

Orcaset is licensed under the Server Side Public License v1. See the LICENSE file for details.

