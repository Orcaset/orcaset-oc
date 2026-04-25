open Orcaset

(* --- Helpers -------------------------------------------------------------- *)

let c csv label name = Series.Period.of_seq ~label (List.to_seq (Csv.quarterly_cells csv name))

let take_last n xs =
  let rec drop k ys =
    match (k, ys) with
    | 0, _ | _, [] -> ys
    | k, _ :: tail -> drop (k - 1) tail
  in
  let len = List.length xs in
  drop (max 0 (len - n)) xs

let average xs =
  match xs with
  | [] -> 0.0
  | _ -> List.fold_left ( +. ) 0.0 xs /. Float.of_int (List.length xs)

let trailing_ratio numerators denominators =
  let pairs = List.combine numerators denominators |> take_last 4 in
  let valid = List.filter (fun (_, denominator) -> not (Float.equal denominator 0.0)) pairs in
  average (List.map (fun (numerator, denominator) -> numerator /. denominator) valid)

(** Negate a historical series (for sign-convention: costs stored as positive in CSV). *)
let neg_c csv label name =
  let raw = c csv (label ^ " (raw)") name in
  Series.Period.map ~label (fun x -> -.x) (lazy raw)

(** Generic YoY-growth forecast: grow each quarter by [annual_growth] relative to the same quarter
    one year prior. *)
let yoy_forecast ~label ~annual_growth hist =
  let forecast last_period =
    let quarterly = Offset.make ~months:3 ~month_end:true () in
    let yoy_lookback = Offset.make ~months:(-12) () in
    Series.Period.unfold_self ~label:(label ^ " (forecast)") ~init:last_period
      ~cells:(fun prev_period ->
        let cur = Period.next quarterly prev_period in
        let lookback_period = Period.shift yoy_lookback cur in
        let next_cell =
          Series.Period.step ~period:cur
            (let open Series.Period.Query in
             let+ prev = self ~period:lookback_period ~reduce:Series.Period.reduce_sum in
             prev)
            (fun prev -> prev *. (1.0 +. annual_growth))
        in
        Some (next_cell, cur))
  in
  Series.Period.extend ~label hist forecast

(** Forecast a cost line as a negative percentage of a driver series. *)
let pct_of_revenue_forecast ~label ~pct hist revenue =
  let forecast period =
    let future_revenue =
      Series.Period.after ~label:(label ^ " driver") (Period.end_date period) (lazy revenue)
    in
    Series.Period.map ~label:(label ^ " (forecast)") (fun rev -> -.rev *. pct) (lazy future_revenue)
  in
  Series.Period.extend ~label hist forecast

(* --- Revenue components --------------------------------------------------- *)

(** Split function that preserves a cell's value on both sides of a split. Unlike
    {!Period_cell.proportional_split}, this keeps the rate/ratio unchanged when a period is
    subdivided — appropriate for intensive quantities like growth rates. *)
let rec constant_split period value_fn split_date =
  ( {
      Period_cell.period = Period.make (Period.start_date period) split_date;
      f = value_fn;
      split = constant_split;
    },
    {
      Period_cell.period = Period.make split_date (Period.end_date period);
      f = value_fn;
      split = constant_split;
    } )

(** Annual revenue-growth assumption applied YoY to net product sales: 10% for 2026, 5% for 2027,
    2.5% per year thereafter. Defined as three explicit cells spanning calendar years; the
    {!constant_split} split function ensures each sub-period (e.g. a forecast quarter) inherits the
    enclosing year's rate. *)
let revenue_growth_rate =
  let cell start_year end_year rate =
    let period = Period.make (Date.make start_year 12 31) (Date.make end_year 12 31) in
    Period_cell.const period (fun () -> rate) constant_split
  in
  let cells =
    [
      cell 2025 2026 0.10;
      cell 2026 2027 0.05;
      cell 2027 2999 0.025;
    ]
  in
  Series.Period.of_seq ~label:"Revenue Growth Rate" (List.to_seq cells)

(** Forecast net product sales: grow each quarter by the YoY rate read from
    {!revenue_growth_rate}. *)
let make_net_product_sales csv =
  let hist = c csv "Net Product Sales (hist)" "Net product sales" in
  let forecast last_period =
    let quarterly = Offset.make ~months:3 ~month_end:true () in
    let yoy_lookback = Offset.make ~months:(-12) () in
    Series.Period.unfold ~label:"Net Product Sales (forecast)"
      ~deps:(Series.Period.dep_period (lazy revenue_growth_rate))
      ~init:last_period
      ~cells:(fun rate_dep prev_period ->
        let cur = Period.next quarterly prev_period in
        let lookback_period = Period.shift yoy_lookback cur in
        let next_cell =
          Series.Period.step ~period:cur
            (let open Series.Period.Query in
             let+ prev = self ~period:lookback_period ~reduce:Series.Period.reduce_sum
             and+ rate = period rate_dep ~period:cur ~reduce:Series.Period.reduce_sum in
             (prev, rate))
            (fun (prev, rate) -> prev *. (1.0 +. rate))
        in
        Some (next_cell, cur))
  in
  Series.Period.extend ~label:"Net Product Sales" hist forecast

(** Forecast rental and royalty revenue: grow at 2% YoY. *)
let make_rental_and_royalty_revenue csv =
  let hist = c csv "Rental and Royalty Revenue (hist)" "Rental and royalty revenue" in
  yoy_forecast ~label:"Rental and Royalty Revenue" ~annual_growth:0.02 hist

(* --- Cost components ------------------------------------------------------ *)

(** Forecast product COGS as a percentage of net product sales. *)
let make_product_cogs csv net_product_sales =
  let hist = neg_c csv "Product COGS (hist)" "Product cost of goods sold" in
  pct_of_revenue_forecast ~label:"Product COGS" ~pct:0.65 hist net_product_sales

(** Forecast rental and royalty cost as a percentage of rental and royalty revenue. *)
let make_rental_and_royalty_cost csv rental_and_royalty_revenue =
  let hist = neg_c csv "Rental and Royalty Cost (hist)" "Rental and royalty cost" in
  pct_of_revenue_forecast ~label:"Rental and Royalty Cost" ~pct:0.30 hist rental_and_royalty_revenue

(* --- SGA: negative, percentage of revenue ---------------------------------- *)

let make_sga csv revenue =
  let hist = neg_c csv "SGA (hist)" "Selling, marketing and administrative expenses" in
  let sga_ratio =
    trailing_ratio
      (Csv.quarterly_find csv "Selling, marketing and administrative expenses" |> List.map Option.get)
      (Csv.quarterly_find csv "Total revenue" |> List.map Option.get)
  in
  let forecast last_period =
    let hist_end_date = Period.end_date last_period in
    let future_revenue = Series.Period.after ~label:"SGA Driver" hist_end_date (lazy revenue) in
    Series.Period.map ~label:"SGA (forecast)" (fun rev -> -.rev *. sga_ratio) (lazy future_revenue)
  in
  Series.Period.extend ~label:"SGA" hist forecast

(* --- Other Income --------------------------------------------------------- *)

(** Forecast other income at ~$5M per quarter (flat). *)
let make_other_income csv =
  let hist = c csv "Other Income (hist)" "Other income, net" in
  let forecast last_period =
    let quarterly = Offset.make ~months:3 ~month_end:true () in
    Series.Period.unfold_self ~label:"Other Income (forecast)" ~init:last_period
      ~cells:(fun prev_period ->
        let cur = Period.next quarterly prev_period in
        let next_cell = Series.Period.const ~period:cur (fun () -> 5_000_000.0) in
        Some (next_cell, cur))
  in
  Series.Period.extend ~label:"Other Income" hist forecast

(* --- Tax: negative, effective rate on EBT --------------------------------- *)

let make_tax csv ebt =
  let hist = neg_c csv "Income Tax (hist)" "Provision for income taxes" in
  let tax_rate = 0.25 in
  let forecast last_period =
    let hist_end_date = Period.end_date last_period in
    let future_ebt = Series.Period.after ~label:"Future EBT" hist_end_date (lazy ebt) in
    Series.Period.map ~label:"Income Tax (forecast)" (fun e -> -.e *. tax_rate) (lazy future_ebt)
  in
  Series.Period.extend ~label:"Income Tax" hist forecast

(* --- Public interface ----------------------------------------------------- *)

let make csv (_ctx : Types.ctx) =
  (* Revenue components *)
  let net_product_sales = make_net_product_sales csv in
  let rental_and_royalty_revenue = make_rental_and_royalty_revenue csv in
  let revenue =
    Series.Period.sum ~label:"Total Revenue"
      [ lazy net_product_sales; lazy rental_and_royalty_revenue ]
  in
  (* Cost components *)
  let product_cogs = make_product_cogs csv net_product_sales in
  let rental_and_royalty_cost = make_rental_and_royalty_cost csv rental_and_royalty_revenue in
  let cogs =
    Series.Period.sum ~label:"Total Costs" [ lazy product_cogs; lazy rental_and_royalty_cost ]
  in
  (* Gross margin components *)
  let product_gross_margin =
    Series.Period.sum ~label:"Product Gross Margin" [ lazy net_product_sales; lazy product_cogs ]
  in
  let rental_and_royalty_gross_margin =
    Series.Period.sum ~label:"Rental and Royalty Gross Margin"
      [ lazy rental_and_royalty_revenue; lazy rental_and_royalty_cost ]
  in
  let gross_margin =
    Series.Period.sum ~label:"Total Gross Margin"
      [ lazy product_gross_margin; lazy rental_and_royalty_gross_margin ]
  in
  (* Operating expenses and below *)
  let sga = make_sga csv revenue in
  let ebit = Series.Period.sum ~label:"Earnings from Operations" [ lazy gross_margin; lazy sga ] in
  let other_income = make_other_income csv in
  let ebt =
    Series.Period.sum ~label:"Earnings before Income Taxes" [ lazy ebit; lazy other_income ]
  in
  let tax = make_tax csv ebt in
  let net_earnings = Series.Period.sum ~label:"Net Earnings" [ lazy ebt; lazy tax ] in
  {
    Types.net_product_sales;
    rental_and_royalty_revenue;
    revenue;
    product_cogs;
    rental_and_royalty_cost;
    cogs;
    product_gross_margin;
    rental_and_royalty_gross_margin;
    gross_margin;
    sga;
    ebit;
    other_income;
    ebt;
    tax;
    net_earnings;
  }
