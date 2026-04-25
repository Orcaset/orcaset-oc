open Orcaset
open Series

(* ----- Assumptions ----- *)
let initial_period = Period.make (Date.make 2025 12 31) (Date.make 2026 3 31)
let initial_value = 1000.0
let qtrly_growth_rate = 0.03
let expense_margin = 0.5
let qtr_offset = Offset.make ~quarters:1 ~month_end:true ()
let lookback = Offset.make ~quarters:(-1) ~month_end:true ()

(* ----- Model ----- *)
let rec revenue =
  Span_series.Unfold
    {
      id = new_id ();
      init = initial_period;
      deps = (fun () -> Deps.span_dep revenue);
      step =
        (fun query_rev period ->
          let value () =
            if period = initial_period then initial_value
            else
              let prior_qtr_rev =
                query_rev ~period:(Period.prev lookback period) ~reduce:(Deps.reduce ( +. ) 0.0)
              in
              prior_qtr_rev *. (1. +. qtrly_growth_rate)
          in
          Some (step ~period ~split:proportional_split value, Period.next qtr_offset period));
    }

let expenses = Span_series.scale (-.expense_margin) revenue
let profit = Span_series.sum revenue expenses

(* ----- Output ----- *)
let () =
  let quarter_count = 8 in
  let cache = make_cache () in
  let periods =
    Period.make_seq ~start:(Period.start initial_period) ~offset:qtr_offset
    |> Seq.take quarter_count |> List.of_seq
  in
  let col_labels = List.map (fun per -> Date.to_string (Period.end_ per)) periods in
  let values_for series =
    List.map
      (fun per -> query_span cache series ~period:per ~reduce:(Deps.reduce ( +. ) 0.0))
      periods
  in
  let r_row = values_for revenue in
  let e_row = values_for expenses in
  let p_row = values_for profit in
  let w_label = 10 in
  let w_num = 11 in
  let pad s w = Printf.sprintf "%*s" w s in
  let pad_f x = Printf.sprintf "%*.2f" w_num x in
  print_string (pad "" w_label);
  List.iter (fun lab -> print_string (pad lab (w_num + 1))) col_labels;
  print_endline "";
  let row name vals =
    print_string (pad name w_label);
    List.iter (fun v -> print_string (pad (pad_f v) (w_num + 1))) vals;
    print_endline ""
  in
  row "Revenue" r_row;
  row "Expenses" e_row;
  row "Profit" p_row;
  print_endline "";
  let accrued_period = Period.make (Date.make 2026 4 15) (Date.make 2026 9 15) in
  let accrued_value series =
    query_span cache series ~period:accrued_period ~reduce:(Deps.reduce ( +. ) 0.0)
  in
  let accrued_revenue = accrued_value revenue in
  let accrued_expenses = accrued_value expenses in
  let accrued_profit = accrued_value profit in
  Printf.printf "Accrued totals from 2026-04-15 to 2026-09-15\n";
  Printf.printf "Revenue:  %.2f\n" accrued_revenue;
  Printf.printf "Expenses: %.2f\n" accrued_expenses;
  Printf.printf "Profit:   %.2f\n" accrued_profit
