# Orcaset - Financial Modeling for AI

Orcaset is a financial modeling framework designed to safely orchestrate analysis at any scale. 

Orcaset uses strong typing and runtime safety checks to prevent users and agents from accidentally creating malformed models. Summing over currencies without conversion, deleting line items that are still referenced, or other common spreadsheet errors are caught immediately. Strong protections give end-users confidence that large-scale modifications won't cause hidden errors.

* **Build and Update with Confidence** - Strong typing prevents invalid models and helps agents navigate large deep dependencies. Confidently modify models without breaking them.
* **Transparent and Deterministic** - Calculations are open and transparent. Build and inspect dependency trees to audit model calculations. No black boxes.
* **Efficient Construction** - Save tokens by writing formulas once per line item, not once per cell. Reuse model components across models and scenarios.

## Installation

Add to an `opam` switch.

```sh
opam pin add orcaset https://github.com/Orcaset/orcaset-oc.git
```

## Example Usage

Orcaset models are constructed by defining and combining line item formulas. Values are materialized by querying over dates. Intermediate values are internally cached during each query to efficiently resolve outputs over repeated lookups. Direct and indirect circular dependencies are resolved iteratively, similar to how they are resolved in a spreadsheet.

### Create a simple model

Create a simple model with revenue, expenses, and income matching the following definition:

* **Revenue**: Grows at a constant annual rate
* **Expenses**: Fixed percentage of revenue
* **Income**: Sum of revenue and expenses

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

Line items can be optionally tagged with a currency (or other unit). Trying to combine line items with different units results in a compile-time error unless they are explicitly converted. In this example, revenue is marked as `USD` which causes expenses and income to also be inferred as `USD`.

Line items exist within a generative series module which enforces isolation between scopes.

### Query values

Materialize and evaluate a model by querying over dates. `S.Period.query` retrieves unevaluated cells for a list of bounding periods, and `S.eval_many` runs the fixed-point solver to produce an `eval_result` whose values carry the phantom currency tag as `'c amount`.

```ocaml
let () =
  let query_period = Period.make (Date.make 2026 1 15) (Date.make 2026 2 15) in
  let query_result = S.Period.query [ query_period ] income in
  let eval_result = S.eval_many query_result in
  match eval_result with
  | [ S.Period { label; value = Amount v; _ } ] ->
      Printf.printf "%s for %s: %f\n" label (Period.to_string query_period) v
  | _ -> ()

(* Income for 2026-01-15..2026-02-15: 241.111111 *)
```

Notice that the query periods can cover arbitrary dates. Orcaset will automatically interpolate partial periods. In this example, accrual periods are defined on calendar quarters, but the query bounds are mid-month.

## License

Orcaset is licensed under the Server Side Public License v1. See the LICENSE file for details.

