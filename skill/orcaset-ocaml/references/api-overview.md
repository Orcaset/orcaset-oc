# Orcaset API Overview

Use this reference when building financial models with the current Orcaset OCaml API. Orcaset models are typed line-item graphs: spans model values over periods, points model values at dates, and statements/query functions materialize values for requested periods or dates.

## Contents

- [`Date`](#date)
- [`Offset`](#offset)
- [`Period`](#period)
- [`Yf`](#yf)
- [`Split`](#split)
- [`Series.Agg`](#seriesagg)
- [`Series.Spans`](#seriesspans)
- [`Series.Points`](#seriespoints)
- [`Series.Deps` and `Series.Formula`](#seriesdeps-and-seriesformula)
- [Querying and Dependency Inspection](#querying-and-dependency-inspection)
- [`Stmt`](#stmt)
- [Practical Patterns](#practical-patterns)
- [Common Pitfalls](#common-pitfalls)

## Minimal Setup

```ocaml
open Orcaset

let d y m day = Date.make y m day
let p s e = Period.make s e
let month = Offset.make ~months:1 ~month_end:true ()
let month_back = Offset.make ~months:(-1) ~month_end:true ()
let sum_agg = Series.Agg.sum
```

Use `Series.Agg.sum` for ordinary flow lines and totals. Choose a different `Agg.t` for span lines that should consolidate multiple source spans by average, min, max, or time-weighted average when queried over longer periods.

## `Date`

Purpose: immutable Gregorian dates in the proleptic Gregorian calendar.

Important signatures:

```ocaml
type t
val make : int -> int -> int -> t
val lower_bound : t
val upper_bound : t
val year : t -> int
val month : t -> int
val day : t -> int
val compare : t -> t -> int
val equal : t -> t -> bool
val ( < ) : t -> t -> bool
val ( <= ) : t -> t -> bool
val ( > ) : t -> t -> bool
val ( >= ) : t -> t -> bool
val min : t -> t -> t
val max : t -> t -> t
val diff : t -> t -> int
val add_days : int -> t -> t
val weekday : t -> int
val is_leap_year : int -> bool
val days_in_month : t -> int
val shift : Offset.t -> t -> t
val hash : t -> int
val to_string : t -> string
val pp : Format.formatter -> t -> unit
```

- `Date.make year month day` raises `Invalid_argument` for invalid month or day values.
- `Date.diff d0 d1` returns `d0 - d1` in calendar days.
- `Date.shift offset d` applies month-like components before day/week components; `Offset.month_end = true` snaps the result to the last day of the month.
- `Date.to_string d` formats as `YYYY-MM-DD`.

## `Offset`

Purpose: composite calendar offsets for shifting dates and periods.

```ocaml
type t = {
  days : int;
  weeks : int;
  months : int;
  quarters : int;
  years : int;
  month_end : bool;
}

val make :
  ?days:int ->
  ?weeks:int ->
  ?months:int ->
  ?quarters:int ->
  ?years:int ->
  ?month_end:bool ->
  unit ->
  t
```

- `Offset.make ()` defaults numeric components to `0` and `month_end` to `false`.
- Use negative offsets for lookbacks, for example `Offset.make ~months:(-1) ~month_end:true ()`.
- Use `~month_end:true` for monthly, quarterly, or yearly financial model periods anchored to month ends.

## `Period`

Purpose: define '(start date, end date) periods.

```ocaml
type t
val make : Date.t -> Date.t -> t
val unbounded : t
val start : t -> Date.t
val end_ : t -> Date.t
val days : t -> int
val to_tuple : t -> Date.t * Date.t
val contains : Date.t -> t -> bool
val next : Offset.t -> t -> t
val prev : Offset.t -> t -> t
val shift : Offset.t -> t -> t
val make_seq : start:Date.t -> offset:Offset.t -> t Seq.t
val seq_to_dates : t Seq.t -> Date.t Seq.t
val list_to_dates : t list -> Date.t list
val equal : t -> t -> bool
val hash : t -> int
val to_string : t -> string
val pp : Format.formatter -> t -> unit
```

- `Period.make start end_` performs no validation; reversed or zero-length periods are possible.
- `Period.contains d p` is true when `Period.start p <= d && d < Period.end_ p`.
- `Period.next offset p` returns a period with dates `Period.end_ p` and `Date.shift offset (Period.end_ p)`
- `Period.prev offset p` returns a period with dates `Date.shift offset (Period.start p)` and `Period.start p`; notice that the offset is ADDED to the period's start date. Use an offset with NEGATIVE values to look backwards, a positive value offset with overlap with the current period.
- `Period.make_seq ~start ~offset` returns an infinite sequence of contiguous periods.

Example:

```ocaml
let start = Date.make 2025 12 31
let qtr = Offset.make ~quarters:1 ~month_end:true ()
let periods = Period.make_seq ~start ~offset:qtr |> Seq.take 8 |> List.of_seq
```

Prefer using month end dates to represend months and calendar quarters unless instructed otherwise.

Example:

```ocaml
let january_2025 = Period.make (Date.make 2024 12 31) (Date.make 2025 1 31)
let q4_2026 = Period.make (Date.make 2026 9 30) (Date.make 2026 12 31)
```

## `Yf`

Purpose: year fraction calculations for day count conventions.

```ocaml
val act_360 : Date.t -> Date.t -> float
val act_act : Date.t -> Date.t -> float
val thirty_360 : Date.t -> Date.t -> float
val cmonthly : Date.t -> Date.t -> float
```

- Use year fractions for annual growth, interest, depreciation, churn, or contract rates that need to scale by period length.
- Use `cmonthly` to scale using equal calendar months of 1/12 each with partial months pro rated by days in the month.

Example:

```ocaml
let yf = Yf.cmonthly (Period.start period) (Period.end_ period)
let value = prior_revenue *. (1. +. (annual_growth *. yf))
```

## `Split`

Purpose: strategies for allocating a span value when it is clipped or aligned at an interior date.

```ocaml
type part
val part : value:(float -> float) -> part
val value : part -> float -> float

type t = period:Period.t -> date:Date.t -> part * part
val daily : t
val const : t
val cmonthly : t
val act_360 : t
```

- `Split.daily` allocates by calendar days. Use it for normal flows such as revenue, expenses, cash flow, capex, depreciation, and taxes.
- `Split.cmonthly` interpolates values by equal calendar months according to the `Yf.cmonthly` convention. Use it where values should be split evenly across calendar months or quarters.
- `Split.const` assigns the original value to each clipped side. Use it for rates, assumptions, and stock-like span values that should not prorate.
- `Split.act_360` allocate by the corresponding year fraction convention.
- `Split.part ~value` builds one side of a custom split function.

Custom split example:

```ocaml
let front_loaded_split : Split.t =
 fun ~period:_ ~date:_ ->
  ( Split.part ~value:(fun value -> value *. 0.75),
    Split.part ~value:(fun value -> value *. 0.25) )
```

## `Series.Agg`

Purpose: aggregation functions for consolidating a span series' clipped samples over a query period.

```ocaml
module Agg : sig
  type sample = { period : Period.t; value : float }
  type t

  val make : (sample option list -> float option) -> t
  val reduce : t -> sample option list -> float option
  val sum : t
  val min : t
  val max : t
  val average : t
  val time_weighted_average : (Date.t -> Date.t -> float) -> t
end
```

- A sample is one defined clipped span value. Missing query coverage and undefined cell values appear as `None`.
- Built-in aggregations ignore missing samples and return `None` for empty or all-missing inputs.
- `Series.Agg.sum` is the default choice for flow lines and statement totals.
- `Series.Agg.average` is useful for simple unweighted averages.
- `Series.Agg.time_weighted_average Yf.act_360`, `Yf.act_act`, or `Yf.cmonthly` is useful for span-modeled rates, utilization, user counts, headcount, and other values that should be averaged over time.
- `Series.Agg.make` is available when domain-specific consolidation logic is required.

## `Series.Spans`

Purpose: values allocated over periods.

Important signatures:

```ocaml
module Spans : sig
  type unfold_cell
  type t

  val neg : ?label:string -> t -> t
  val scale : ?label:string -> float -> t -> t
  val sum : ?label:string -> agg:Agg.t -> t list -> t
  val sub : ?label:string -> agg:Agg.t -> t -> t -> t
  val mul : ?label:string -> agg:Agg.t -> t list -> t
  val div : ?label:string -> agg:Agg.t -> t -> t -> t

  val const : ?label:string -> split:Split.t -> agg:Agg.t -> period:Period.t -> float -> t
  val of_list : ?label:string -> split:Split.t -> agg:Agg.t -> (Period.t * float) list -> t
  val map : ?label:string -> (float -> float) -> t -> t
  val map2 :
    ?label:string -> agg:Agg.t -> t -> t -> (float option -> float option -> float option) -> t
  val mapn : ?label:string -> agg:Agg.t -> t list -> (float option list -> float option) -> t
  val extend : agg:Agg.t -> t -> t -> t
  val clipped : after:Date.t -> until:Date.t -> t -> t
  val after : Date.t -> t -> t
  val until : Date.t -> t -> t

  val unfold :
    ?label:string ->
    agg:Agg.t ->
    deps:(unit -> 'readers Deps.t) ->
    init:'state ->
    cells:('readers -> 'state -> (unfold_cell * 'state) option) ->
    unit ->
    t

  val unfold_from :
    ?label:string ->
    agg:Agg.t ->
    deps:(unit -> 'readers Deps.t) ->
    cells:('readers -> Period.t -> (unfold_cell * Period.t) option) ->
    t ->
    t

  val unfold_rec :
    ?label:string ->
    agg:Agg.t ->
    deps:(t -> 'readers Deps.t) ->
    init:'state ->
    cells:('readers -> 'state -> (unfold_cell * 'state) option) ->
    unit ->
    t

  val cell : period:Period.t -> split:Split.t -> float option Formula.t -> unfold_cell
  val label : t -> string option
  val agg : t -> Agg.t
  val with_agg : agg:Agg.t -> t -> t
end
```

- `Spans.const ?label ~split ~agg ~period value` creates one span value over `period`.
- `Spans.of_list ?label ~split ~agg cells` yields cells in list order; it does not validate ordering, gaps, or overlaps.
- `Spans.sum ~agg` and `Spans.sub ~agg` treat a missing side as `0.0` when another side is present; a row with no present inputs is `None`.
- `Spans.mul ~agg` and `Spans.div ~agg` return `None` when any required side is missing.
- `Spans.map2 ~agg` and `Spans.mapn ~agg` align inputs at common boundaries and pass missing entries as `None`; the mapping function decides whether to return `Some value` or `None`.
- `Spans.extend ~agg actuals forecast` yields actuals, then continues with forecast, clipping the first overlapping forecast span when needed.
- `Spans.clipped`, `Spans.after`, `Spans.until`, `Spans.neg`, `Spans.scale`, and `Spans.map` preserve or inherit aggregation from their source series.
- `Spans.with_agg ~agg series` changes how future span queries consolidate samples.
- `Spans.cell` takes a `float option Formula.t`: use `Some value` for defined values and `None` for missing values.
- Make sure aggregation methods are consistent with split methods.

Self-recursive forecast example:

```ocaml
let revenue =
  Series.Spans.unfold_rec ~label:"Revenue" ~agg:sum_agg
    ~deps:(fun self -> Series.Deps.span_dep self)
    ~init:initial_period
    ~cells:(fun read_revenue period ->
      let formula =
        if Period.equal period initial_period then Series.Formula.pure (Some 1_000.0)
        else
          let open Series.Formula in
          let+ prior = read_revenue ~period:(Period.prev month_back period) in
          Option.map (fun prior -> prior *. 1.03) prior
      in
      Some (Series.Spans.cell ~period ~split:Split.daily formula, Period.next month period))
    ()
```

Historicals plus forecast using `extend`:

```ocaml
let historical_revenue =
  Series.Spans.of_list ~label:"Revenue" ~split:Split.daily ~agg:sum_agg
    [ (Period.make (d 2025 1 1) (d 2025 4 1), 120.0) ]

let revenue = Series.Spans.extend ~agg:sum_agg historical_revenue forecast_revenue
```

## `Series.Points`

Purpose: values observed at dates.

```ocaml
module Points : sig
  type t

  val neg : ?label:string -> t -> t
  val scale : ?label:string -> float -> t -> t
  val sum : ?label:string -> t list -> t
  val sub : ?label:string -> t -> t -> t
  val mul : ?label:string -> t list -> t
  val div : ?label:string -> t -> t -> t
  val const : ?label:string -> period:Period.t -> float -> t
  val of_list : ?label:string -> (Date.t * float) list -> t
  val map : ?label:string -> (float -> float) -> t -> t
  val map2 : ?label:string -> t -> t -> (float option -> float option -> float option) -> t
  val mapn : ?label:string -> t list -> (float option list -> float option) -> t
  val accum : ?label:string -> init:float -> Spans.t -> t
  val label : t -> string option
end
```

- `Points.const ?label ~period value` returns `value` at dates contained by `period`.
- `Points.of_list ?label values` is sparse and exact; it does not validate ordering or duplicate dates.
- `Points.sum` and `Points.sub` ignore missing sides when another side is present.
- `Points.mul` and `Points.div` return `None` when any required side is missing.
- `Points.accum ?label ~init changes` starts at `init` and accumulates cumulative span values in `changes`.

Balance roll-forward example:

```ocaml
let total_cf =
  Series.Spans.sum ~label:"Total cash flow" ~agg:sum_agg
    [ operating_cf; investing_cf; financing_cf ]

let cash = Series.Points.accum ~label:"Cash" ~init:initial_cash total_cf
```

## `Series.Deps` and `Series.Formula`

`Deps` declares line-level dependencies for `Spans.unfold` and `Spans.unfold_rec`. Formula readers record cell-level queries that are resolved only when a cell is evaluated.

```ocaml
module Deps : sig
  type span_reader = period:Period.t -> float option Formula.t
  type point_reader = date:Date.t -> float option Formula.t
  type _ t

  val none : unit t
  val span_dep : Spans.t -> span_reader t
  val point_dep : Points.t -> point_reader t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

module Formula : sig
  type 'a t

  type packed_query =
    | Span_query_item of { series : Spans.t; period : Period.t }
    | Point_query_item of { series : Points.t; date : Date.t }

  val pure : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val queries : 'a t -> packed_query list
end
```

- `Deps.span_dep series` produces a reader that aggregates the dependency over the requested period using that dependency series' own `Agg.t`.
- `Deps.point_dep series` produces a reader that returns the point value at the requested date.
- Both readers return `float option Formula.t`; missing source data propagates as `None`.
- Use `Series.Formula.pure (Some value)` for literal defined cell values and `Series.Formula.pure None` for literal missing cell values.
- Use `Option.map`, `Option.bind`, or an explicit `Option.value ~default` inside formulas. Defaults are financial assumptions, not API requirements.

Dependency example:

```ocaml
let gross_profit =
  Series.Spans.unfold ~label:"Gross profit" ~agg:sum_agg
    ~deps:(fun () ->
      let open Series.Deps in
      let+ read_revenue = span_dep revenue
      and+ read_cogs = span_dep cost_of_revenue in
      (read_revenue, read_cogs))
    ~init:first_period
    ~cells:(fun (read_revenue, read_cogs) period ->
      let formula =
        let open Series.Formula in
        let+ r = read_revenue ~period
        and+ c = read_cogs ~period in
        match (r, c) with
        | Some r, Some c -> Some (r +. c)
        | Some r, None -> Some r
        | None, Some c -> Some c
        | None, None -> None
      in
      Some (Series.Spans.cell ~period ~split:Split.daily formula, Period.next month period))
    ()
```

## Querying and Dependency Inspection

```ocaml
type _ series =
  | Point_series : Points.t -> [ `Point ] series
  | Span_series : Spans.t -> [ `Span ] series

val label : 'a series -> string option
type packed_series = Series : 'a series -> packed_series
type dependency = { series : packed_series; dependencies : dependency list; is_back_edge : bool }
val dependencies : 'a series -> dependency list

type series_cache
exception Evaluation_did_not_converge of { iterations : int; tolerance : float; max_delta : float }
val make_cache : unit -> series_cache
val query_span_samples : series_cache -> Spans.t -> period:Period.t -> Agg.sample option list
val query_span : series_cache -> Spans.t -> period:Period.t -> float option
val query_point : series_cache -> Points.t -> date:Date.t -> float option
```

- Prefer using the `Stmt` module over direct queries.
- Use the same cache for related groups of queries.

## `Stmt`

Purpose: statement-style output trees.

```ocaml
type evaluated =
  | Span_values of (Period.t * float option) list
  | Point_values of (Date.t * float option) list

type stmt

type resolved =
  | RGroup of resolved list
  | RTotal of { label : string option; values : evaluated option; children : resolved list }
  | RLine of { label : string option; values : evaluated option }

val group : stmt list -> stmt
val span_line : Series.Spans.t -> stmt
val span_lines : Series.Spans.t list -> stmt list
val span_total : Series.Spans.t -> stmt list -> stmt
val point_line : Series.Points.t -> stmt
val point_lines : Series.Points.t list -> stmt list
val point_total : Series.Points.t -> stmt list -> stmt
val eval_periods : Period.t list -> stmt -> resolved
val eval_dates : Date.t list -> stmt -> resolved
val fixed_width : resolved -> string
```

- `Stmt.eval_periods periods stmt` evaluates span rows over each period and point rows at all unique period start/end dates.
- `Stmt.eval_dates dates stmt` evaluates point rows; span rows keep labels but have `values = None`.
- `Stmt.fixed_width resolved` renders a quick text table. Span values use period end dates as headers; point values use point dates.
- `Stmt.span_total total children` and `Stmt.point_total total children` take statement children, usually from `Stmt.span_lines`, `Stmt.point_lines`, nested totals, or individual `Stmt.*_line` calls.

Statement example:

```ocaml
let income_stmt =
  Stmt.span_total net_income
    [
      Stmt.span_total gross_profit (Stmt.span_lines [ revenue; cost_of_revenue ]);
      Stmt.span_line opex;
      Stmt.span_line depreciation;
      Stmt.span_line income_tax;
      Stmt.span_line net_income;
    ]

let balance_sheet_stmt =
  Stmt.group
    [
      Stmt.point_total total_assets (Stmt.point_lines [ cash; ppe_net ]);
      Stmt.point_total total_equity_liabilities
        (Stmt.point_lines [ common_stock; retained_earnings ]);
      Stmt.point_line bs_check;
    ]

let total_stmt = Stmt.group [ income_stmt; cash_flow_stmt; balance_sheet_stmt ]
let resolved = Stmt.eval_periods periods total_stmt
let () = Printf.printf "\n%s\n\n" (Stmt.fixed_width resolved)
```

## Practical Patterns

### Simple Forecast

```ocaml
let expenses = Series.Spans.scale ~label:"Expenses" (-0.50) revenue
let profit = Series.Spans.sum ~label:"Profit" ~agg:sum_agg [ revenue; expenses ]
let stmt = Stmt.span_total profit (Stmt.span_lines [ revenue; expenses ])
```

### Span Aggregation Choices

```ocaml
let bookings =
  Series.Spans.of_list ~label:"Bookings" ~split:Split.daily ~agg:Series.Agg.sum booking_cells

let active_users =
  Series.Spans.of_list ~label:"Active users" ~split:Split.const
    ~agg:(Series.Agg.time_weighted_average Yf.act_act)
    user_count_cells

let peak_capacity =
  Series.Spans.of_list ~label:"Peak capacity" ~split:Split.const ~agg:Series.Agg.max capacity_cells
```

### Three-Statement Circularity

Use lazy recursive values when two model lines depend on each other:

```ocaml
let rec lazy_depreciation =
  lazy
    (Series.Spans.unfold ~label:"Depreciation"
       ~deps:(fun () -> Series.Deps.point_dep (Lazy.force lazy_ppe_net))
       ~init:first_period
       ~cells:(fun read_ppe_net period ->
         let formula =
           let open Series.Formula in
           let+ ppe_net = read_ppe_net ~date:(Period.start period) ~default:0.0 in
           -.ppe_net *. monthly_depreciation_rate
         in
         Some
           ( Series.Spans.cell ~period ~split:Series.proportional_split formula,
             Period.next month period ))
       ())

and lazy_ppe_change =
  lazy
    (let capitalized_capex = Series.Spans.scale (-1.0) capex in
     Series.Spans.sum ~label:"PPE change" [ capitalized_capex; Lazy.force lazy_depreciation ])

and lazy_ppe_net =
  lazy (Series.Points.accum ~label:"PPE net" ~init:initial_ppe (Lazy.force lazy_ppe_change))
```

### Partial Period Queries

Partial-period behavior is controlled by each span's `Split.t`; multi-span consolidation is controlled by each span's `Agg.t`.

```ocaml
let stub = Period.make (d 2026 4 15) (d 2026 9 15)
let profit_stub = Series.query_span cache profit ~period:stub
```

## Common Pitfalls

- Do not use `Series.Points.t` for income statement or cash flow lines.
- Do not use `Split.const` for ordinary flow lines that should prorate across partial periods.
- Do not use `Series.Agg.sum` for span-modeled rates or counts that should average over time.
- Do not assume `Period.make` rejects reversed dates.
- Do not assume `Spans.of_list` or `Points.of_list` validates ordering, gaps, overlaps, or duplicates.
- Do not use `Stmt.eval_dates` when span values are required; use `Stmt.eval_periods`.
- Do not create a new cache for every related direct query.
- Do not leave circular model lines eager when OCaml requires laziness.
- Do not ignore `Series.Evaluation_did_not_converge` when recursive formulas are unstable.
- Do not pass a positive offset to `Period.prev`; use a negative offset (e.g. month_back) to look backward.