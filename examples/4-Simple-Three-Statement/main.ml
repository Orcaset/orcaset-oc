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
  S.Period.step ~period
    (S.Period.Query.self ~period:(Period.shift revenue_lookback period) ~reduce:S.Period.reduce_sum)
    (fun last_value -> last_value *. ((1.0 +. 0.05) ** yf))

let revenue =
  let rec generate_cells last_period () =
    let current_period = Period.shift offset last_period in
    Seq.Cons (revenue_step current_period, generate_cells current_period)
  in
  S.Period.unfold_self ~label:"Revenue" ~cells:(fun () ->
      Seq.cons
        (S.Period.const ~period:initial_period (fun () -> 1000.0))
        (generate_cells initial_period))

(* COGS: 30% of revenue. *)
let cogs = S.Period.map ~label:"COGS" (fun r -> r *. -0.30) (lazy revenue)

(* Gross Profit: Revenue + COGS. *)
let gross_profit = S.Period.sum ~label:"Gross Profit" (lazy revenue) (lazy cogs)

(* Opex: constant -$200/month. *)
let opex =
  S.Period.unfold_self ~label:"Opex" ~cells:(fun () ->
      Seq.map (fun period -> S.Period.const ~period (fun () -> -200.0)) months)

(* Capex: 5% of revenue (negative = cash outflow). *)
let capex = S.Period.map ~label:"Capex" (fun r -> r *. -0.05) (lazy revenue)

(* Depreciation and PPE Net: circular dependency.
   Depreciation = prior PPE Net x (10% / 12), as an expense (negative).
   PPE Net starts at $10,000, increases by capex additions, decreases by depreciation. *)
let rec depreciation =
  lazy
    (S.Period.unfold ~label:"Depreciation" ~deps:(S.Period.dep_point ppe_net) ~cells:(fun ppe_net ->
         Seq.map
           (fun period ->
             S.Period.step ~period
               (S.Period.Query.point ppe_net ~date:(Period.start_date period))
               (function Some ppe -> ppe *. (-0.10 /. 12.0) | None -> 0.0))
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
  S.Period.unfold_self ~label:"CF Financing" ~cells:(fun () ->
      Seq.map (fun period -> S.Period.const ~period (fun () -> 0.0)) months)

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

(* Total Assets: Cash + PPE Net. *)
let total_assets = S.Point.sum ~label:"Total Assets" (lazy cash) (lazy ppe_net)

(* Total Equity: Common Stock + Retained Earnings. *)
let total_equity = S.Point.sum ~label:"Total Equity" (lazy common_stock) (lazy retained_earnings)

(* Balance Check: Total Assets - Total Equity (should be zero). *)
let balance_check = S.Point.sub ~label:"Balance Check" (lazy total_assets) (lazy total_equity)

(* --- Output ----------------------------------------------------------------- *)

let num_periods = 6
let query_periods = List.of_seq (Seq.take num_periods months)
let period_end_dates = List.map Period.end_date query_periods
let balance_dates = start_date :: period_end_dates

let extract_value (r : _ S.eval_result) =
  match r with
  | Period { value = Amount v; _ } -> v
  | Point { point = Some (_, Amount v); _ } -> v
  | Point { point = None; _ } -> 0.0

let format_number v =
  let s = Printf.sprintf "%.0f" (Float.abs v) in
  let len = String.length s in
  let buf = Buffer.create (len + (len / 3)) in
  String.iteri
    (fun i c ->
      if i > 0 && (len - i) mod 3 = 0 then Buffer.add_char buf ',';
      Buffer.add_char buf c)
    s;
  let formatted = Buffer.contents buf in
  if v < -0.5 then "(" ^ formatted ^ ")" else formatted

let lw = 24
let cw = 14
let pad_right n s = if String.length s >= n then s else s ^ String.make (n - String.length s) ' '
let pad_left n s = if String.length s >= n then s else String.make (n - String.length s) ' ' ^ s

let print_table title dates rows =
  Printf.printf "\n=== %s ===\n\n" title;
  Printf.printf "%s" (pad_right lw "");
  List.iter (fun d -> Printf.printf "%s" (pad_left cw (Date.to_string d))) dates;
  print_newline ();
  Printf.printf "%s\n" (String.make (lw + (cw * List.length dates)) '-');
  List.iter
    (fun (label, values) ->
      Printf.printf "%s" (pad_right lw label);
      List.iter (fun v -> Printf.printf "%s" (pad_left cw (format_number v))) values;
      print_newline ())
    rows

let eval_period_rows series =
  List.map
    (fun s ->
      (S.Period.label s, List.map extract_value (S.eval_many (S.Period.query query_periods s))))
    series

let eval_point_rows series =
  List.map
    (fun s ->
      (S.Point.label s, List.map extract_value (S.eval_many (S.Point.query_many balance_dates s))))
    series

let () =
  print_table "Income Statement" period_end_dates
    (eval_period_rows [ revenue; cogs; gross_profit; opex; depreciation; ebt; tax; net_income ]);
  print_table "Cash Flow Statement" period_end_dates
    (eval_period_rows [ cf_operations; capex; cf_financing; net_cash_change ]);
  print_table "Balance Sheet" balance_dates
    (eval_point_rows
       [ cash; ppe_net; total_assets; common_stock; retained_earnings; total_equity; balance_check ]);
  print_newline ()
