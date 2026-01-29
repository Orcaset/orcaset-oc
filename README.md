# Orcaset - Financial Modeling for AI

Orcaset is a programmatic financial modeling tool designed to effectively orchestrate analysis at scale. It gives AI systems a structured, deterministic, and efficient way to model financial scenarios.

* **Update with Confidence** - OCaml's strong, flexible type system gives users confidence to quickly update models and helps agentic systems verify correctness.
* **Integrate with any System** - Orcaset runs in an open, standard OCaml environment, making it easy to fetch data from APIs, databases, or other sources.
* **Scale Effortlessly** - Layered design allows users to quickly build simple analyses or scale to complex, portfolio-scale analysis.
* **Transparent and Deterministic** - Models are human-readable and auditable, ensuring transparency in financial analysis.

## Installation

Add to an existing `opam` switch with:

```sh
opam pin add orcaset https://github.com/orcaset/orcaset.git
```

## Getting Started

Orcaset models can be built incrementally. They can start as ad-hoc scripts and evolve into highly structured models as they grow. Users coming from spreadsheets will find that Orcaset's abstractions map naturally to familiar modeling concepts.

### Create a Line Item

Orcaset models are built around sequences of data over time representing financial line items or rows in a financial statement. Orcaset uses OCaml's standard `Seq` module to represent these sequences, which may be infinite and are evaluated lazily.

```ocaml
(* Infinite sequence starting at 1000 and growing by 10% each period *)
let revenue = Seq.iterate (fun prev_rev -> prev_rev *. 1.10) 1000.0
```

Rather than iterating over raw numbers, you can associate values in a sequence with time. Orcaset provides three types to work with time-based sequences:

* `Accrual`: Flow *between* two dates
* `Transaction`: Flow on a *specific* date
* `Balance`: Snapshot on a specific date

```ocaml
open Orcaset

let start_date = CalendarLib.Date.make 2025 1 1
let initial_value = 1000.0

(* Starts on 2025-01-01 at 1000 and grows at 10% annually, compounding monthly *)
let revenue = Seq.unfold
  (fun (period_start, prior_value) ->
    let next_date = CalendarLib.Date.add period_start (CalendarLib.Date.Period.month 1) in
    let accrual = Accrual.make
      ~period:(Period.make ~start_date:period_start ~end_date:next_date)
      ~value:(lazy prior_value)
      ~split_fn:Accrual.default_split_fn
    in
    Some (accrual, (next_date, prior_value *. (1.0 +. 0.10 /. 12.0))))
  (start_date, initial_value)
```

### Composing Line Items

Transform and combine sequences to create multi-line models.

```ocaml
(* Calculate expenses as 30% of revenue *)
let expenses = Seq.map (fun rev -> Accrual.map (fun v -> v *. -0.30) rev) revenue

(* Sum line item sequences *)
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

### Working with Co-Dependent Line Items

Orcaset uses lazy evaluation to handle mutually dependent sequences. For example, suppose revenue starts at 100 and grows based on the prior period's marketing costs, while marketing costs are a percentage of current period revenue:

```ocaml
open CalendarLib
let initial_period = Period.make ~start_date:(Date.make 2025 1 1) ~end_date:(Date.make 2025 2 1)
let initial_revenue = 100.0
let marketing_multiplier = 12.0
let marketing_pct = 0.10

let rec revenue = Seq.cons 
  (* Initial revenue accrual *)
  (Accrual.make
    ~period:initial_period
    ~value:(lazy initial_revenue)
    ~split_fn:Accrual.default_split_fn)
  (* Future revenue accruals based on marketing costs *)
  (Seq.map
    (fun prior_marketing_cost -> 
    Accrual.make 
      ~period:(Period.add_offset (Period.make_offset ~months:1 ()) prior_marketing_cost.period) 
      ~value:(lazy (1.0 +. marketing_multiplier *. (Lazy.force prior_marketing_cost.value)))
      ~split_fn:Accrual.default_split_fn
  )
    marketing_costs)
(* Define marketing costs as a percentage of revenue *)
and marketing_costs = Seq.map
  (fun rev ->
    Accrual.map (fun v -> v *. marketing_pct) rev)
  revenue
```

For mutually dependent sequences to resolve correctly, there must be a starting point for at least one sequence, and no circular dependencies within the same time period.

### Memoizing Sequences

There may be a mismatch between timing of events in related sequences. For example, interest on a revolving credit facility might accrue on a quarterly basis, while the outstanding balance changes on any given day. A natural way to model interest would be `interest_rate * average balance over the past quarter`. In code, this looks like:

```ocaml
let start_date = CalendarLib.Date.make 2025 1 1
let interest_rate = 0.05
let balances = Seq.of_list [ (* ... balances ... *) ]

let interest = 
  let interest_periods = Period.make_seq ~start_date ~offset:(Period.make_offset ~months:3 ()) in
  Seq.map
    (fun period ->
      let avg_balance = Balance.avg Yf.actual_360 period.start_date period.end_date balances in
      Accrual.make
        ~period
        ~value:(lazy (avg_balance *. interest_rate *. (Yf.actual_360 period.start_date period.end_date)))
        ~split_fn:Accrual.default_split_fn)
    interest_periods
```

To calculate the average balance over each period, OCaml scans all elements in the `balances` sequence from the start through `period.end_date` for each interest accrual. This repeated traversal can grow quickly as models scale.

An easy way to improve performance in these scenarios is to memoize the sequence so that each element is only computed once:

```ocaml
let balances = Seq.of_list [ (* ... balances ... *) ] |> Seq.memoize
```

This pattern is also common when printing values for a sequence over a range of periods. Memoizing a sequence before iteration can significantly improve performance:

```ocaml
let () =
  let revenue = revenue |> Seq.memoize in
  let periods = Period.make_seq ~start_date:(CalendarLib.Date.make 2025 1 1) ~offset:(Period.make_offset ~months:1 ()) in
  (* ...iterate over periods and print revenue accruals *)
```

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

See the `/examples` directory for complete working models.

## License

Orcaset is licensed under the Server Side Public License v1. See the LICENSE file for details.

