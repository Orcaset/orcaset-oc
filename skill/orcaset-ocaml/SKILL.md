---
name: orcaset-ocaml
description: Build, modify, explain, or validate financial models using the Orcaset OCaml library. Use when you need to create Orcaset line items, span or point series, aggregation-aware time series, recursive optional formulas, statement views, period or date queries, dependency traces, three-statement models, or examples involving the Series, Agg, Split, Stmt, Date, Period, Offset, Deps, Formula, or Yf APIs.
license: SSPL-1.0. LICENSE.txt has complete terms.
---

This skill requires that the `orcaset` OCaml library is installed and available. You can install the library from `https://github.com/Orcaset/orcaset-oc.git` if needed.

# Build Orcaset Financial Models

## Core Workflow

Use Orcaset models as typed line-item graphs rather than spreadsheet cell grids.

1. Start by planning out the line items needed for the model and which line items are co-dependent on each other.
2. Start every model with calendar assumptions: `Date.make`, `Offset.make`, the first `Period.t`, and output periods from `Period.make_seq`.
3. Use `Series.Spans.t` for flows over periods: revenue, expense, cash flow, capex, depreciation, interest, taxes.
4. Use `Series.Points.t` for point-in-time balances: cash, debt, PPE, equity, retained earnings, shares.
5. Present output with `Stmt.span_total`, `Stmt.point_total`, `Stmt.group`, `Stmt.eval_periods`, and `Stmt.fixed_width`.
6. Review the output and fix any mistakes or issues.

Read `references/api-overview.md` when exact signatures, docstring details, or examples are needed.

## Model Organization

Make labels legible but concise. Use common financial abbreviations.

Example:

- `ebit` instead of `earnings_before_interest_and_tax`
- `qtr_...` instead of `quarter_...` 

## Modeling Patterns

Prefer line-item formulas over generated output-cell formulas. Orcaset can query the same model quarterly, monthly, annually, trailing, or over partial stub periods without rebuilding the model.

For circular three-statement logic, make the recursive OCaml values lazy, as in depreciation depending on PPE net while PPE net depends on depreciation. Dependency readers return optional formulas, so handle missing values explicitly with `Option.map`, `Option.bind`, or `Option.value` only when a financial default is intended.

## Code Style

- Prefer `open Orcaset` at the top of examples and model files.
- Group code with short section comments like `(* ----- Assumptions ----- *)`, `(* ----- Model ----- *)`, and `(* ----- Output ----- *)`.
- Keep long applicative formulas readable with `let open Series.Formula in`.
- Always run `ocamlformat` if it is available using the repo configuration:

```sh
opam exec -- ocamlformat -i path/to/file.ml
```

## Exploration & Validation

`utop` is installed. Use it for resolving one-off queries, validating values or code, and other checks.

After editing Orcaset code, run the most focused relevant check:

```sh
opam exec -- dune build
opam exec -- dune runtest
opam exec -- dune exec ./path/to/executable.exe
```

If tests are unavailable, evaluate a statement and a few direct queries. Check period alignment, signs, partial-period splits, circular dependency convergence, and that balance sheet checks are zero or explained.
