open Orcaset
open Series

(* ----- Assumptions ----- *)
let initial_date = Date.make 2025 12 31
let initial_balance = 1000.0
let annual_interest_rate = 0.05
let year_frac dt1 dt2 = float_of_int (Date.diff dt1 dt2) /. 360.0
let qtr_offset = Offset.make ~quarters:1 ~month_end:true ()
let initial_period = Period.make initial_date (Date.shift qtr_offset initial_date)

(* ----- Model ----- *)
let rec interest =
  Span_series.Unfold
    {
      id = new_id ();
      deps = (fun () -> Deps.point_dep balance);
      init = initial_period;
      step =
        (fun get_balance period ->
          let interest () =
            let yf = year_frac (Period.end_ period) (Period.start period) in
            let starting_balance = get_balance ~date:(Period.start period) ~default:0.0 in
            starting_balance *. yf *. annual_interest_rate
          in
          Some (step ~period ~split:proportional_split interest, Period.next qtr_offset period));
    }

and balance = Point_series.Accum { id = new_id (); init = initial_balance; changes = interest }

(* ----- Output ----- *)
let () =
  let forecast_quarters = 4 in
  let cache = make_cache () in
  let periods =
    Period.make_seq ~start:initial_date ~offset:qtr_offset
    |> Seq.take forecast_quarters |> List.of_seq
  in
  let dates = initial_date :: List.map Period.end_ periods in
  let query_point series date = query_point cache series ~date ~default:0.0 in
  let query_span series period = query_span cache series ~period ~reduce:(Deps.reduce ( +. ) 0.0) in
  let w_label = 18 in
  let w_num = 12 in
  let pad s w = Printf.sprintf "%*s" w s in
  let pad_f x = Printf.sprintf "%*.2f" w_num x in
  let print_header labels =
    print_string (pad "" w_label);
    List.iter (fun label -> print_string (pad label (w_num + 1))) labels;
    print_endline ""
  in
  let print_row name values =
    print_string (pad name w_label);
    List.iter (fun value -> print_string (pad (pad_f value) (w_num + 1))) values;
    print_endline ""
  in
  print_header (List.map Date.to_string dates);
  print_row "Balance" (List.map (query_point balance) dates);
  print_endline "";
  print_header (List.map (fun period -> Date.to_string (Period.end_ period)) periods);
  print_row "Interest accrual" (List.map (query_span interest) periods);
  print_endline "";
  let as_of = Date.make 2026 3 15 in
  let qtd_period = Period.make initial_date as_of in
  Printf.printf "As of %s\n" (Date.to_string as_of);
  Printf.printf "  Balance:              %.2f\n" (query_point balance as_of);
  Printf.printf "  QTD interest accrual: %.2f\n" (query_span interest qtd_period)
