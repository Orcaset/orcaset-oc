open Orcaset
module S = Series.Make ()

(* This example models an account balance that grows over time based on 
   monthly compounding interest. The balance can be queried at any 
   date for the current value, including accrued interest. *)

let start_date = Date.make 2025 12 31
let offset = Offset.make ~months:1 ~month_end:true ()
let rate = 0.03
let periods = Period.make_seq ~start_date ~offset

(* Interest: each month's interest = balance at period start * rate. *)
let interest_step period =
  S.Period.Step
    {
      period;
      queries = [ Point_dep { index = 0; date = Period.start_date period } ];
      f = (fun values -> match values with [ v ] -> v *. rate | _ -> 0.0);
    }

let rec interest =
  lazy
    (S.Period.unfold ~label:"Interest" ~deps:[ Point_dep balance ] (Seq.map interest_step periods))

(* Balance: initial value + accumulated interest over time. *)
and balance = lazy (S.Point.accum ~label:"Balance" ~start_date ~initial_value:100.0 interest)

(* Print out periods. *)
let () =
  let period_count = 12 in
  let interest_cells =
    match S.Period.to_seq [ Lazy.force interest ] with
    | [ seq ] -> seq |> Seq.take period_count |> List.of_seq
    | _ -> assert false
  in
  let balance_dates =
    List.concat_map
      (fun cell ->
        let period = Period_cell.period cell in
        [ Period.start_date period; Period.end_date period ])
      interest_cells
  in
  let balance_cells = S.Point.query_many balance_dates (Lazy.force balance) in
  let rec build_rows interest_cells balance_cells =
    match (interest_cells, balance_cells) with
    | ic :: rest_ic, start_bc :: end_bc :: rest_bc ->
        let row =
          Eval.PeriodCell ic
          :: List.filter_map (Option.map (fun c -> Eval.PointCell c)) [ start_bc; end_bc ]
        in
        (Period_cell.period ic, row) :: build_rows rest_ic rest_bc
    | _ -> []
  in
  let rows = build_rows interest_cells balance_cells in
  let groups = List.map snd rows in
  let value_groups = Eval.many groups in
  Printf.printf "%-25s %16s %16s %16s\n" "Period" "Start Balance" "Interest" "End Balance";
  Printf.printf "%s\n" (String.make 73 '-');
  List.iter2
    (fun (period, _) values ->
      match values with
      | [ interest_v; start_v; end_v ] ->
          Printf.printf "%-25s %16.2f %16.2f %16.2f\n" (Period.to_string period) start_v interest_v
            end_v
      | _ -> ())
    rows value_groups

(* 
Period                       Start Balance         Interest      End Balance
-------------------------------------------------------------------------
2025-12-31..2026-01-31              100.00             3.00           103.00
2026-01-31..2026-02-28              103.00             3.09           106.09
2026-02-28..2026-03-31              106.09             3.18           109.27
2026-03-31..2026-04-30              109.27             3.28           112.55
2026-04-30..2026-05-31              112.55             3.38           115.93
2026-05-31..2026-06-30              115.93             3.48           119.41
2026-06-30..2026-07-31              119.41             3.58           122.99
2026-07-31..2026-08-31              122.99             3.69           126.68
2026-08-31..2026-09-30              126.68             3.80           130.48
2026-09-30..2026-10-31              130.48             3.91           134.39
2026-10-31..2026-11-30              134.39             4.03           138.42
2026-11-30..2026-12-31              138.42             4.15           142.58 *)
