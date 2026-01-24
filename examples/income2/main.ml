(* Model parameters *)
let period_start = CalendarLib.Date.make 2025 1 1
let period_end = CalendarLib.Date.make 2025 2 1
let start_period = Orcaset.Period.make ~start_date:period_start ~end_date:period_end
let freq = Orcaset.Period.make_offset ~months:1 ()

(* Revenue parameters *)
let recurring_first = 10000.0
let recurring_growth = 0.05
let non_recurring_first = 2000.0
let non_recurring_drift = 50.0
let non_recurring_volatility = 500.0
let seed = 42

(* Cost of revenue parameters *)
let recurring_cost_pct = -0.30
let non_recurring_cost_pct = -0.40

(* Admin parameters *)
let admin_first = -1500.0
let admin_rate = 0.03

(* Year fraction convention *)
let yf = Orcaset.Yf.actual_360

(* Build the model *)
let revenue =
  Revenue.make ~start_period ~recurring_first ~recurring_growth ~non_recurring_first
    ~non_recurring_drift ~non_recurring_volatility ~seed ~freq ~yf

let cost_of_revenue =
  Cost_of_revenue.make ~revenue ~recurring_pct:recurring_cost_pct
    ~non_recurring_pct:non_recurring_cost_pct

let gross_profit =
  Gross_profit.make ~revenue_total:revenue.Revenue.total
    ~cost_of_revenue_total:cost_of_revenue.Cost_of_revenue.total

let admin =
  Admin.make ~start_date:period_start ~initial_value:admin_first ~rate:admin_rate ~freq ~yf

let operating_income = Operating_income.make ~gross_profit ~admin

(* Build the income statement using Statement module *)
let income_statement =
  let open Orcaset.Statement in
  group "Income Statement"
    [
      group ~total:cost_of_revenue.Cost_of_revenue.total "Cost of Revenue"
        [
          group ~total:revenue.Revenue.total "Revenue"
            [
              line "Recurring" revenue.Revenue.recurring;
              line "Non-Recurring" revenue.Revenue.non_recurring;
            ];
          line "Recurring" cost_of_revenue.Cost_of_revenue.recurring;
          line "Non-Recurring" cost_of_revenue.Cost_of_revenue.non_recurring;
        ];
      line "Gross Profit" gross_profit;
      group "Operating Expenses" [ line "Admin" admin ];
      line "Operating Income" operating_income;
    ]

(* Generate periods for display *)
let periods =
  Orcaset.Period.make_seq ~start_date:period_start ~offset:freq |> Seq.take 12 |> List.of_seq

let format_float ?(width = 10) value =
  let abs_value = Float.abs value in
  let sign = if value < 0.0 then "-" else "" in
  let int_part = Int64.of_float abs_value in
  let rec add_commas n acc =
    if Int64.compare n 1000L < 0 then Int64.to_string n :: acc
    else
      let remainder = Int64.rem n 1000L in
      let quotient = Int64.div n 1000L in
      add_commas quotient (Printf.sprintf "%03Ld" remainder :: acc)
  in
  let formatted =
    if Int64.compare int_part 0L = 0 then "0" else String.concat "," (add_commas int_part [])
  in
  let s = sign ^ formatted in
  let pad = max 0 (width - String.length s) in
  String.make pad ' ' ^ s

let print_statement ~periods item =
  let col_width = 12 in
  let format_header period =
    let start_str = CalendarLib.Printer.Date.sprint "%Y-%m-%d" period.Orcaset.Period.start_date in
    let pad = max 0 (col_width - String.length start_str) in
    String.make pad ' ' ^ start_str
  in
  Printf.printf "%-30s" "";
  List.iter (fun p -> Printf.printf "%s" (format_header p)) periods;
  print_newline ();
  let rec print_item ~indent item =
    match item with
    | Orcaset.Statement.Line { label; seq } ->
        let values = Orcaset.Accrual.accrue_periods periods seq in
        Printf.printf "%-30s" (String.make indent ' ' ^ label);
        List.iter (fun v -> Printf.printf "%s" (format_float ~width:col_width v)) values;
        print_newline ()
    | Orcaset.Statement.Group { label; items; total } ->
        Printf.printf "%-30s\n" (String.make indent ' ' ^ label);
        List.iter (print_item ~indent:(indent + 2)) items;
        (match total with
        | Some seq ->
            let values = Orcaset.Accrual.accrue_periods periods seq in
            Printf.printf "%-30s" (String.make indent ' ' ^ "Total " ^ label);
            List.iter (fun v -> Printf.printf "%s" (format_float ~width:col_width v)) values;
            print_newline ()
        | None -> ());
        print_newline ()
  in
  print_item ~indent:0 item

let () =
  Printf.printf "=== INCOME STATEMENT ===\n\n";
  print_statement ~periods income_statement;
  Printf.printf "\n=== LINE ITEMS ===\n";
  let lines = Orcaset.Statement.lines income_statement in
  List.iter (fun (label, _) -> Printf.printf "- %s\n" label) lines
