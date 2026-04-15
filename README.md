# Orcaset - Financial Modeling for AI

Orcaset is a financial modeling framework designed to safely orchestrate analysis at any scale. 

Orcaset uses strong typing and runtime safety checks to prevent users and agents from accidentally creating malformed models. Summing over currencies without conversion, deleting line items that are still referenced, and many other common spreadsheet errors are caught immediately. Strong protections give end-users confidence that large-scale modifications do not cause hidden errors.

* **Build and Update with Confidence** - Strong typing prevents invalid models and helps agents navigate deep formula dependencies. Confidently modify models without breaking them.
* **Transparent and Deterministic** - Calculations are open and transparent. Build and inspect dependency trees to audit model calculations. No black boxes.
* **Efficient Construction** - Save tokens by writing formulas once per line item, not once per cell. Re-use components across models and scenarios.

## Installation

Add to an `opam` switch.

```sh
opam pin add orcaset https://github.com/Orcaset/orcaset-oc.git
```

## Example Usage

Orcaset models are constructed by defining and combining line item formulas. Values are materialized by querying over dates. Values are internally cached during each query to efficiently resolve outputs. Direct and indirect circular dependencies are resolved iteratively, similar to how they are resolved in a spreadsheet.

### Creating a simple model

Create a simple model with revenue, expenses, and income matching the following definition:

* **Revenue**: Grows at a constant annual rate
* **Expenses**: Fixed percentage of revenue
* **Income**: Sum of revenue and expenses

```ocaml
open Orcaset

let start = Date.make 2025 12 31
let offset = Offset.make ~months:3 ~month_end:true ()

let revenue : [ `USD ] Series.Period.t =
  Series.Period.const_ann_growth ~label:"Revenue" ~start ~value:1000.0 ~rate:0.12 ~offset
    ~yf:Yf.actual_360

let expenses = Series.Period.map ~label:"Expenses" (fun r -> r *. -0.30) (lazy revenue)
let income = Series.Period.sum ~label:"Income" [ lazy revenue; lazy expenses ]
```

Line items can be optionally tagged with a unit (e.g. currencies). Trying to combine line items with different units results in a compile-time error unless they are explicitly converted. In this example, revenue is marked as `USD` which causes expenses and income to also be inferred as `USD`.

### Querying values

Materialize and evaluate a model by querying over dates. Values for specific dates or periods can be queried directly from a series, or they can be composed into structured statements and printed in a tabular format. The example below groups revenue and expenses into sub-lines beneath income and prints out four non-calendar month periods.

```ocaml
let start_date = Date.make 2026 1 15
let offset = Offset.make ~months:1 ()
let query_periods = Period.make_seq ~start_date ~offset |> Seq.take 4

let stmt =
  Series.Stmt.period_total income
    [ Series.Stmt.period_line revenue; Series.Stmt.period_line expenses ]

let () = print_string (Series.Stmt.pp stmt (List.of_seq query_periods))

(* Printed output:

Period end    2026-02-15  2026-03-15  2026-04-15  2026-05-15
------------------------------------------------------------
  Revenue            344         311         347         339
  Expenses         (103)        (93)       (104)       (102)
              ----------  ----------  ----------  ----------
Income               241         218         243         237
*)
```

Query periods can cover arbitrary dates. Orcaset will automatically interpolate partial periods. In this example, accrual periods are defined on calendar quarters, but the query bounds are mid-month.

## Tracing dependencies

All calculations are fully traceable. You can inspect dependencies at both the line level and the individual queried number level.

The code below prints out a line-level dependency graph in DOT format.

```ocaml
let () =
  Graph.pp_dot Format.std_formatter [ Series.period_to_graph income ];
  Format.pp_print_flush Format.std_formatter ()
```

Rendering the graph to a image produces the chart below.

![Orcaset Example Line-Level Dependency Graph](./images/readme-series-dependencies.png)

The arrows show the relationships between line items:

* **Revenue**: Feeds directly into both expenses and income
* **Expenses**: Depends on revenue and feeds into income
* **Income**: Depends on both revenue and expenses

The parenthetical notes the series variant (i.e. the type of formula).

## License

Orcaset is licensed under the Server Side Public License v1. See the LICENSE file for details.

