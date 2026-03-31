# Orcaset - Financial Modeling for AI

Orcaset is a financial modeling framework designed to safely orchestrate analysis at any scale. 

Orcaset uses strong typing and runtime safety checks to prevent users and agents from accidentally creating malformed models. Summing over currencies without conversion, deleting line items that are still referenced, or other common spreadsheet errors are caught immediately. Strong protections give end-users confidence that large-scale modifications won't break their models.

* **Build and Update with Confidence** - Strong typing prevents invalid models and helps agents navigate large deep dependencies. Confidently modify models without breaking them.
* **Transparent and Deterministic** - Calculations are open and transparent. Build and inspect dependency trees to audit model calculations. No black boxes.
* **Efficient Construction** - Save tokens by writing formulas once per line item, not once per cell. Reuse model components across models and scenarios.

## Installation

Add to an `opam` switch.

```sh
opam pin add orcaset https://github.com/Orcaset/orcaset-oc.git
```

## Example Usage

Orcaset models are constructed by defining and combining line item formulas. Values are materialized by querying over dates. Intermediate values are internally cached during each query to efficiently resolve outputs for complex models. Direct and indirect circular dependencies are resolved iteratively, similar to how they are resolved in a spreadsheet.

### Create a simple model

Create a simple model with revenue, expenses, and income.

* Revenue: Grows at a constant annual rate
* Expenses: Fixed percentage of revenue
* Income: Sum of revenue and expenses

```ocaml
open Orcaset
module S = Series.Make ()

let start = Date.make 2025 12 31
let offset = Offset.make ~months:3 ~month_end:true ()

let revenue : [ `USD ] S.Period.t =
  S.Period.const_ann_growth ~label:"Revenue" ~start ~value:1000.0 ~rate:0.12 ~offset
    ~yf:Yf.actual_360
let expenses = S.Period.map ~label:"Expenses" (fun r -> r *. -0.30) (lazy revenue)
let income = S.Period.sum ~label:"Income" (lazy revenue) (lazy expenses)
```

Line items can be optionally tagged with a currency (or other unit). Trying to combine line items with different units without explicit conversion results in a compile-time error. In this example, revenue is marked as USD which causes expenses and income to also be inferred as USD.

Line items exist within a generative series module which enforces isolation between scopes.

### Query values

Materialize and evaluate a model by querying over a date range.

```ocaml
let query_period = Period.make (Date.make 2026 1 15) (Date.make 2026 6 15)

(* Print out total accrued value. *)
let () =
  let income_cells = S.Period.query query_period [ income ] |> List.hd in
  let total = Seq.fold_left (fun acc cell -> acc +. Eval.one (PeriodCell cell)) 0.0 income_cells in
  Printf.printf "Total income over %s: %.2f\n" (Period.to_string query_period) total
  (* Total income over 2026-01-15..2026-06-15: 1184.94 *)
```

Notice that the query periods can be any arbitrary dates. Orcaset will automatically interpolate partial underlying periods. In this example, accrual periods are defined on calendar quarters, but the query period bounds are mid-month.

## License

Orcaset is licensed under the Server Side Public License v1. See the LICENSE file for details.

