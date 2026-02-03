# Orcaset - Financial Models in OCaml

This project includes a library for creating financial statement models in OCaml and usage examples.

`/lib` contains the main library code for building and manipulating financial models.

## Coding Style

- Always check work by running `opam exec -- dune build` to make sure outputs compile.
- Use `ocamlformat` for consistent code formatting. Run `opam exec -- dune fmt --auto` to format code automatically.
- Write *minimal* comments only when necessary to explain non-obvious logic. Prefer clear, self-explanatory code.
- Logical groupings of line items should be encapsulated in OCaml modules (where `module.t` is lazily created and passed to other modules if there are mutual dependencies between line item groups).

## Orcaset Philosophy

Orcaset treats financial models as compositions of lazy, infinite time series. Each line item is a sequence of accruals that can depend on other line items, be combined through arithmetic operations, and be queried for specific time ranges. The framework handles period alignment automatically, letting you focus on business logic rather than date arithmetic.

Key principles:
- **Laziness by default** - Sequences are computed on demand, enabling infinite series and circular dependencies
- **Period alignment is automatic** - When combining sequences, the library handles splitting and merging at period boundaries
- **Composition over inheritance** - Build complex models by combining simple line items

---

## Core Concepts

Line items are generally represented as sequences of values over time.

- Line items for flows over a period of time (e.g. revenue accruals, expense accruals) - `Accrual.t Seq.t`
- Line items for point in time flows (e.g. cash received on a specific date) - `Transaction.t Seq.t`
- Line items for balances at a point in time (e.g. cash balance, asset value) - `Balance.t Seq.t`
  - NOTE: Balance line items are generally modeled as `Balance_series.t` and materialized on specific dates using `Balance_series.on`, `Balance_series.at_dates`, `Balance_series.at_periods`, etc.

### Accrual Line Item

An accrual line item is an `Accrual.t Seq.t` - a lazy sequence of accruals. Each accrual is either:
- A **Simple** accrual with a `period`, lazy `value`, and `split_fn`
- A **Combined** accrual that tracks the lineage of combined values (created via `Accrual.combine` or arithmetic operations)

Use accessor functions `Accrual.period` and `Accrual.value` to retrieve properties.

An accrual line item is fundamentally just a sequence. You can construct one manually with `Seq.unfold` to see the mechanics:

```ocaml
(* Manual line item construction with Seq.unfold. Creates a sequence of monthly accruals of 5000.0. *)
let revenue : Accrual.t Seq.t =
  Seq.unfold
    (fun start ->
      (* Build period from current start date *)
      let end_date = CalendarLib.Date.add start (CalendarLib.Date.Period.month 1) in
      let period = Period.make ~start_date:start ~end_date in
      let accrual = Accrual.make ~period ~value:(lazy 5000.0) ~split_fn:Accrual.default_split_fn in
      (* Return (element, next_state); None would terminate the sequence *)
      Some (accrual, end_date))
    start_date
```

**Invariant: Accrual periods in a sequence must be non-overlapping, and strictly increasing.** This is the fundamental assumption of the library. Always produce well-formed sequences. Functions may not produce correct results if this expectation is violated.

### Creating Periods

Periods are simply a (start_date, end_date) pair.

```ocaml
(* Single period *)
let period = Period.make 
  ~start_date:(CalendarLib.Date.make 2025 1 1)
  ~end_date:(CalendarLib.Date.make 2025 2 1)

(* Offset for stepping through time *)
let monthly = Period.make_offset ~months:1 ()
let quarterly = Period.make_offset ~quarters:1 ()
let annual = Period.make_offset ~years:1 ()

(* Infinite sequence of periods *)
let periods = Period.make_seq ~start_date ~offset:monthly
```

### Creating Accruals

```ocaml
(* Single accrual *)
let accrual = Accrual.make 
  ~period 
  ~value:(lazy 1000.0) 
  ~split_fn:Accrual.default_split_fn

(* Growing sequence (infinite) *)
let growing = Accrual.const_annual_growth_seq
  ~start_date ~initial_value:1000.0 ~rate:0.10 ~freq:monthly

(* Manual sequence construction from a sequence of periods *)
let custom_seq = 
  Seq.map 
    (fun period -> Accrual.make ~period ~value:(lazy 500.0) ~split_fn:Accrual.default_split_fn)
    (Period.make_seq ~start_date ~offset:monthly)

(* Convenience function for creating a sequence that grows at a constant annual rate (uses an actual/360 day count internally) *)
let const_growth_seq = Accrual.const_annual_growth_seq
  ~start_date ~initial_value:2000.0 ~rate:0.05 ~freq:quarterly
```

The `default_split_fn` allocates value evenly across days in the period. You can define custom split functions for more complex allocation logic (e.g. actual/360 day count, 30/360 day count, evenly across calendar months, etc.).

### Working with Balances

Balances are point-in-time snapshots.

```ocaml
(* Single balance *)
let balance = Balance.make 
  ~date:(CalendarLib.Date.make 2025 1 1)
  ~amount:(lazy 10000.0)
```

Create time series of balances that derive from accruals or transactions using `Balance_series.from_accruals` or `Balance_series.from_transactions`. DO NOT MANUALLY CONSTRUCT SEQUENCES OF BALANCES SINCE SEQUENCES WOULD NEED TO BE TOO DENSE TO CAPTURE ALL POSSIBLE QUERY DATES.

```ocaml
(* Create balance series from accrual sequence. 
    Starts at 1000 on 2025-01-01 and changes based on `const_growth_seq` thereafter. *)
let const_growth_seq = Accrual.const_annual_growth_seq
  ~start_date ~initial_value:2000.0 ~rate:0.05 ~freq:quarterly

let balance_from_accruals = 
  Balance_series.from_accruals 
    ~initial_date:(CalendarLib.Date.make 2025 1 1)
    ~initial_amount:(lazy 1000.0)
    const_growth_seq

(* Create balance series from transaction sequence. 
    Starts at 500 on 2025-01-01 and changes based on transactions thereafter. *)
let txn_seq = Seq.of_list [
  Transaction.make 
    ~date:(CalendarLib.Date.make 2025 2 1)
    ~amount:(lazy 300.0);
  Transaction.make 
    ~date:(CalendarLib.Date.make 2025 3 15)
    ~amount:(lazy -100.0);
]

let balance_from_txns = 
  Balance_series.from_transactions 
    ~initial_date:(CalendarLib.Date.make 2025 1 1)
    ~initial_amount:(lazy 500.0)
    txn_seq
```

Create balance series from other types of inputs using `Balance_series.from_flow`.

Combine balance series using `Balance_series.combine` or arithmetic operations.

---

## Creating Line Items

### Independent Composition

When line items have no circular dependencies, they can be composed directly:

```ocaml
(* Transform a line item *)
let cogs = Seq.map (fun rev -> Accrual.map (fun v -> v *. -0.30) rev) revenue

(* Sum multiple line items *)
let total_revenue = Accrual.sum_seq software_rev services_rev

(* Chain multiple sums *)
let total_opex = Accrual.sum_seq (Accrual.sum_seq cogs salaries) rent
```

### Dependent Composition

When a line item depends on another, pass the dependency as a parameter:

```ocaml
module Opex = struct
  type t = { cogs : Accrual.t Seq.t; total : Accrual.t Seq.t }

  let make ~revenue_seq ~cogs_pct =
    let cogs = Seq.map (fun rev -> Accrual.map (fun v -> v *. cogs_pct) rev) revenue_seq in
    { cogs; total = cogs }
end

(* Usage *)
let opex = Opex.make ~revenue_seq:revenue ~cogs_pct:(-0.30)
```

### Mutual Dependencies (Circular References between Line Items)

When two line items depend on each other (e.g. revenue depends on expenses and expenses depend on revenue), naive construction leads to infinite recursion. To solve this, use mutually dependent **lazy** sequences. Wrap each sequence in an **outer `lazy`** that defers construction until the sequence is actually needed.

>  Key Constraint: When sequence A references sequence B, it must only access elements of B that correspond to **already-computed** elements of A. Typically: A[N] depends on B[N-1], and B[N-1] is derived from A[N-1]. *This pattern requires that at least one of the sequences has a base seed value to anchor the recursion.*

#### Mutual Dependency Pattern

```ocaml
let rec lazy_a =
  lazy (
    let initial = ... in
    let step last ->
      (* access lazy_b for PREVIOUS period only *)
      let next_value = compute_from (Lazy.force lazy_b) (Accrual.period last) in
      Some (make_element next_value, ...)
    in
    Seq.cons initial (Seq.unfold step initial)
  )

and lazy_b =
  lazy (Seq.map (fun a -> derive_from a) (Lazy.force lazy_a))
```

#### Mutual Dependency Example: Revenue and Expenses

Revenue[N] = Expenses[N-1] * -2.5, where Expenses = Revenue[N] * -0.5

```ocaml
let rec lazy_revenue =
  let initial_accrual =
    Accrual.make ~period:start_period ~value:(lazy 1000.0) ~split_fn:Accrual.default_split_fn
  in
  let step prior_accrual =
    let prior_period = Accrual.period prior_accrual in
    let next_period = Period.add_offset (Period.make_offset ~months:1 ()) prior_period in
    let next_value =
      lazy (Accrual.accrue prior_period.start_date prior_period.end_date (Lazy.force lazy_expenses)
            *. -2.5)
    in
    let next_accrual = Accrual.make ~period:next_period ~value:next_value ~split_fn:Accrual.default_split_fn in
    Some (next_accrual, next_accrual)
  in
  lazy (Seq.cons initial_accrual (Seq.unfold step initial_accrual))

and lazy_expenses =
  lazy (Seq.map (fun rev -> Accrual.map (fun v -> v *. -0.5) rev) (Lazy.force lazy_revenue))
```

---

## Grouping Line Items with Modules

Use OCaml modules to group related line items into logical units. Each module represents a section or subsection of a financial statement (e.g., Revenue, Opex, Income) and exposes its line items through a record type.

### Module Structure Pattern

```ocaml
module Revenue = struct
  type t = {
    software : Accrual.t Seq.t;
    services : Accrual.t Seq.t;
    total : Accrual.t Seq.t;
  }

  let make ~start_period ~software_first ~software_growth ~services_first ~freq =
    let software = ... in
    let services = ... in
    let total = Accrual.sum_seq software services in
    { software; services; total }
end
```

### Guidelines

1. **One module per logical grouping** - Group line items that belong together conceptually (all revenue streams, all operating expenses, etc.)
2. **Expose all line items** - Include both component line items and computed totals in the record type
3. **Dependencies as parameters** - Pass cross-module dependencies through `make` rather than referencing globals
4. **Lazy for circular deps** - Use `lazy` wrappers when modules have circular dependencies
5. **Only pass required sequences** - Generally, prefer passing only the (lazy) sequence dependencies rather than entire modules to minimize coupling. Mutually dependent modules should be tied together at the top or parent level using `let rec ... and ...` with lazy wrappers.

---

## Structuring Output with Statement

The `Statement` module is an optional layer for organizing line items into a presentable hierarchy. While modules group line items for *computation*, `Statement` groups them for *output*—defining how a financial statement should be displayed.

This separation keeps calculation logic independent from presentation. You can create multiple statement views (e.g., a summary vs. detailed breakdown) from the same underlying model data.

### Statement Structure

A statement is a polymorphic tree of items (`'a item`), allowing you to mix different sequence types (e.g. Accrual, Balance, Transaction) in the same statement:
- **Line** - A labeled sequence (a single row in the output)
- **Group** - A labeled collection of items with an optional total

```ocaml
let stmt =
  let open Statement in
  group "Income Statement"
    [
      line "Revenue" revenue_seq;
      group ~total:opex.total "Operating Expenses"
        [
          line "COGS" opex.cogs;
          line "Salaries" opex.salaries;
        ];
      line "Net Income" net_income_seq;
    ]
```

### Core Functions

- `line label seq` - Create a line item
- `group ?total label items` - Create a group with optional total
- `fold ~line_fn ~group_fn stmt` - Fold over the statement tree
- `iter ~line_fn ~group_fn stmt` - Iterate over the statement tree
- `lines stmt` - Extract all line items as `(label, seq) list`
- `to_list stmt` - Flatten to `(label, seq option) list` including groups

The module provides only structural operations. Users handle their own iteration and printing based on sequence types.


## Performance Optimizations

Since sequences are O(n) to traverse, accessing distant periods repeated can be costly. Traversals may be explicit (e.g., iterating over a sequence) or implicit (e.g., calling `Accrual.accrue` which scans the sequence).

Use `Seq.memoize` to cache computed elements of a sequence for faster repeated access:

```ocaml
let memoized_seq = Seq.memoize original_seq
```

> Note: Sequences with side effects (e.g., printing during construction) should not be memoized, as memoization will skip re-evaluation of side effects.

If execution of a model is slow or hangs, try to identify and memoize frequently accessed sequences.