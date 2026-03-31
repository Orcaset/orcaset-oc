open Orcaset
module S = Series.Make ()

(* A three-statement financial model: Income Statement, Cash Flow Statement, and Balance Sheet.
   Demonstrates circular dependencies (depreciation <-> PPE) and point series accumulation for balance
   sheet items. *)

let start_date = Date.make 2025 12 31
let offset = Offset.make ~months:1 ~month_end:true ()
let revenue_lookback = Offset.make ~months:(-1) ~month_end:true ()
let months = Period.make_seq ~start_date ~offset
let initial_period = Period.make start_date (Date.shift offset start_date)

(* --- Income Statement -------------------------------------------------------- *)

(* Revenue: $1,000/month initial, 5% annual growth using Actual/360 day count. *)
let revenue_step period =
  let period_start, period_end = Period.to_tuple period in
  let yf = Yf.actual_360 period_start period_end in
  S.Period.Step
    {
      period;
      queries =
        [ Self { period = Period.shift revenue_lookback period; reduce = S.Period.reduce_sum } ];
      f =
        (fun values ->
          match values with [ last_value ] -> last_value *. ((1.0 +. 0.05) ** yf) | _ -> 0.0);
    }

let revenue =
  let rec generate_cells last_period () =
    let current_period = Period.shift offset last_period in
    Seq.Cons (revenue_step current_period, generate_cells current_period)
  in
  S.Period.unfold ~label:"Revenue" ~deps:[]
    (Seq.cons
       (S.Period.Seed { period = initial_period; f = (fun () -> 1000.0) })
       (generate_cells initial_period))

(* COGS: 30% of revenue. *)
let cogs = S.Period.map ~label:"COGS" (fun r -> r *. -0.30) (lazy revenue)

(* Gross Profit: Revenue + COGS. *)
let gross_profit = S.Period.sum ~label:"Gross Profit" (lazy revenue) (lazy cogs)

(* Opex: constant -$200/month. *)
let opex =
  S.Period.unfold ~label:"Opex" ~deps:[]
    (Seq.map (fun period -> S.Period.Seed { period; f = (fun () -> -200.0) }) months)

(* Capex: 5% of revenue (negative = cash outflow). *)
let capex = S.Period.map ~label:"Capex" (fun r -> r *. -0.05) (lazy revenue)

(* Depreciation and PPE Net: circular dependency.
   Depreciation = prior PPE Net x (10% / 12), as an expense (negative).
   PPE Net starts at $10,000, increases by capex additions, decreases by depreciation. *)
let rec depreciation =
  lazy
    (S.Period.unfold ~label:"Depreciation" ~deps:[ Point_dep ppe_net ]
       (Seq.map
          (fun period ->
            S.Period.Step
              {
                period;
                queries = [ Point_dep { index = 0; date = Period.start_date period } ];
                f = (fun values -> match values with [ ppe ] -> ppe *. (-0.10 /. 12.0) | _ -> 0.0);
              })
          months))

and ppe_net =
  lazy
    (let neg_capex = S.Period.map ~label:"Neg Capex" (fun x -> -.x) (lazy capex) in
     let ppe_change = S.Period.sum ~label:"PPE Change" (lazy neg_capex) depreciation in
     S.Point.accum ~label:"PPE Net" ~start_date ~initial_value:10000.0 (lazy ppe_change))

let depreciation = Lazy.force depreciation
let ppe_net = Lazy.force ppe_net

(* Earnings before tax: Gross Profit + Opex + Depreciation. *)
let ebt =
  let gp_opex = S.Period.sum ~label:"GP + Opex" (lazy gross_profit) (lazy opex) in
  S.Period.sum ~label:"EBT" (lazy gp_opex) (lazy depreciation)

(* Tax: 20% of EBT (negative). *)
let tax = S.Period.map ~label:"Tax" (fun x -> x *. -0.20) (lazy ebt)

(* Net Income: EBT + Tax. *)
let net_income = S.Period.sum ~label:"Net Income" (lazy ebt) (lazy tax)

(* --- Cash Flow Statement ----------------------------------------------------- *)

(* Depreciation add back: reverses the non-cash depreciation charge. *)
let dep_add_back = S.Period.map ~label:"Dep Add Back" (fun x -> -.x) (lazy depreciation)

(* CF Operations: Net Income + Depreciation Add Back. *)
let cf_operations = S.Period.sum ~label:"CF Operations" (lazy net_income) (lazy dep_add_back)

(* CF Financing: constant $0. *)
let cf_financing =
  S.Period.unfold ~label:"CF Financing" ~deps:[]
    (Seq.map (fun period -> S.Period.Seed { period; f = (fun () -> 0.0) }) months)

(* Net Cash Change: CF Operations + Capex + CF Financing. *)
let net_cash_change =
  let ops_plus_capex = S.Period.sum ~label:"Ops + Capex" (lazy cf_operations) (lazy capex) in
  S.Period.sum ~label:"Net Cash Change" (lazy ops_plus_capex) (lazy cf_financing)

(* --- Balance Sheet ----------------------------------------------------------- *)

(* Cash: starts at $1,000, accumulates net cash change. *)
let cash = S.Point.accum ~label:"Cash" ~start_date ~initial_value:1000.0 (lazy net_cash_change)

(* Common Stock: constant $5,000. *)
let common_stock = S.Point.const ~label:"Common Stock" 5000.0

(* Retained Earnings: starts at $6,000 (initial assets - common stock), accumulates net income. *)
let retained_earnings =
  S.Point.accum ~label:"Retained Earnings" ~start_date ~initial_value:6000.0 (lazy net_income)

(* --- Evaluation & Output ----------------------------------------------------- *)

let num_months = 12

let () =
  (* Materialize all period series with shared cache. *)
  let all_period_series =
    [
      revenue;
      cogs;
      gross_profit;
      opex;
      depreciation;
      ebt;
      tax;
      net_income;
      dep_add_back;
      cf_operations;
      capex;
      cf_financing;
      net_cash_change;
    ]
  in
  let period_seqs = S.Period.to_seq all_period_series in
  let period_cells_by_series =
    List.map (fun seq -> seq |> Seq.take num_months |> List.of_seq) period_seqs
  in
  (* Extract end dates from the first series for point queries. *)
  let end_dates =
    match period_cells_by_series with
    | first :: _ -> List.map (fun c -> Period.end_date (Period_cell.period c)) first
    | _ -> assert false
  in
  (* Query point series at each period end date. *)
  let cash_cells = S.Point.query_many end_dates cash in
  let ppe_cells = S.Point.query_many end_dates ppe_net in
  let common_stock_cells = S.Point.query_many end_dates common_stock in
  let re_cells = S.Point.query_many end_dates retained_earnings in
  (* Build evaluation groups: one group per period containing all cells for that column. *)
  let rows =
    List.init num_months (fun i ->
        let period_row =
          List.map
            (fun series_cells -> Eval.PeriodCell (List.nth series_cells i))
            period_cells_by_series
        in
        let point_row =
          List.filter_map
            (Option.map (fun c -> Eval.PointCell c))
            [
              List.nth cash_cells i;
              List.nth ppe_cells i;
              List.nth common_stock_cells i;
              List.nth re_cells i;
            ]
        in
        period_row @ point_row)
  in
  let results = Eval.many rows in
  (* Line item labels and section structure. *)
  let labels =
    [
      ("Income Statement", "");
      ("  Revenue", "period");
      ("  COGS", "period");
      ("  Gross Profit", "period");
      ("  Opex", "period");
      ("  Depreciation", "period");
      ("  EBT", "period");
      ("  Tax", "period");
      ("  Net Income", "period");
      ("Cash Flow Statement", "");
      ("  Dep Add Back", "period");
      ("  CF Operations", "period");
      ("  Capex", "period");
      ("  CF Financing", "period");
      ("  Net Cash Change", "period");
      ("Balance Sheet", "");
      ("  Cash", "point");
      ("  PPE Net", "point");
      ("  Common Stock", "point");
      ("  Retained Earnings", "point");
      ("  Balance Check", "check");
    ]
  in
  (* Extract values by line item (transpose results). *)
  let num_value_items = 13 + 4 in
  let values_by_item =
    List.init num_value_items (fun item_idx ->
        List.map (fun period_values -> List.nth period_values item_idx) results)
  in
  (* Compute balance check: (Cash + PPE) - (Common Stock + Retained Earnings). *)
  let balance_check =
    let cash_vals = List.nth values_by_item 13 in
    let ppe_vals = List.nth values_by_item 14 in
    let cs_vals = List.nth values_by_item 15 in
    let re_vals = List.nth values_by_item 16 in
    List.map2
      (fun (c, p) (s, r) -> c +. p -. s -. r)
      (List.combine cash_vals ppe_vals) (List.combine cs_vals re_vals)
  in
  (* Print table. *)
  let cw = 12 in
  Printf.printf "%-22s" "";
  List.iter (fun d -> Printf.printf " %*s" cw (Date.to_string d)) end_dates;
  Printf.printf "\n";
  Printf.printf "%s\n" (String.make (22 + (num_months * (cw + 1))) '-');
  let value_idx = ref 0 in
  List.iter
    (fun (label, kind) ->
      match kind with
      | "" -> Printf.printf "\n%s\n" label
      | "check" ->
          Printf.printf "%-22s" label;
          List.iter (fun v -> Printf.printf " %*.2f" cw v) balance_check;
          Printf.printf "\n"
      | _ ->
          Printf.printf "%-22s" label;
          let vals = List.nth values_by_item !value_idx in
          List.iter (fun v -> Printf.printf " %*.2f" cw v) vals;
          Printf.printf "\n";
          incr value_idx)
    labels
