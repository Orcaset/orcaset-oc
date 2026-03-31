open Orcaset
module S = Series.Make ()

(* Basic financial model example with three line items: revenue, expenses, and income.
   Revenue: Grows at a constant monthly rate using Unfold
   Expenses: Fixed percentage of revenue
   Income: Sum of revenue and expenses
 *)

let initial_value = 1000.0
let growth_rate = 0.1
let expense_margin = -0.6
let initial_start_date = Date.make 2025 12 31
let offset = Offset.make ~months:1 ~month_end:true ()
let initial_period = Period.make initial_start_date (Date.shift offset initial_start_date)

let revenue_step curr_period last_period =
  S.Period.Step
    {
      period = curr_period;
      queries = [ Self { period = last_period; reduce = S.Period.reduce_sum } ];
      f = (fun values -> List.fold_left (fun acc v -> acc +. v) 0.0 values *. (1.0 +. growth_rate));
    }

let revenue =
  let rec generate_cells last_period () =
    let current_period = Period.shift offset last_period in
    Seq.Cons (revenue_step current_period last_period, generate_cells current_period)
  in
  S.Period.unfold ~label:"Revenue" ~deps:[]
    (Seq.cons
       (S.Period.Seed { period = initial_period; f = (fun () -> initial_value) })
       (generate_cells initial_period))

let expenses = S.Period.map ~label:"Expenses" (fun r -> r *. expense_margin) (lazy revenue)
let income = S.Period.sum ~label:"Income" (lazy revenue) (lazy expenses)

let () =
  let n = 6 in
  let seqs = S.Period.to_seq [ revenue; expenses; income ] in
  let cells = List.map (fun s -> s |> Seq.take n |> List.of_seq) seqs in
  let periods = List.map (fun c -> Period_cell.period c) (List.hd cells) in
  let results = Eval.many (List.map (fun row -> List.map (fun c -> Eval.PeriodCell c) row) cells) in
  (* Header: label column + end dates *)
  Printf.printf "%-12s" "";
  List.iter (fun p -> Printf.printf " %12s" (Date.to_string (Period.end_date p))) periods;
  print_newline ();
  Printf.printf "%s\n" (String.make (12 + (n * 13)) '-');
  (* Rows *)
  let labels = [ "Revenue"; "Expenses"; "Income" ] in
  List.iter2
    (fun label values ->
      Printf.printf "%-12s" label;
      List.iter (fun v -> Printf.printf " %12.2f" v) values;
      print_newline ())
    labels results

(* 
               2026-01-31   2026-02-28   2026-03-31   2026-04-30   2026-05-31   2026-06-30
------------------------------------------------------------------------------------------
Revenue           1000.00      1100.00      1210.00      1331.00      1464.10      1610.51
Expenses          -600.00      -660.00      -726.00      -798.60      -878.46      -966.31
Income             400.00       440.00       484.00       532.40       585.64       644.20 *)
