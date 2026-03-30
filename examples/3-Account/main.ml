open Orcaset
module S = Series.Make ()

(* An account that earns 3% monthly interest on its balance, compounding each month. The balance is
   a point series (queryable at any date) that accumulates interest from a period series. The
   interest series references the balance at the start of each period, creating a circular
   dependency that the evaluation engine resolves automatically. *)

let start_date = Date.make 2025 12 31
let offset = Offset.make ~months:1 ~month_end:true ()
let rate = 0.03

(* Monthly periods starting from start_date *)
let periods = Period.make_seq ~start_date ~offset

(* Interest: each month's interest = balance at period start * rate. *)
let rec interest =
  lazy
    (S.Period.unfold ~label:"Interest" ~deps:[ Point_dep balance ]
       (Seq.map
          (fun period ->
            S.Period.Step
              {
                period;
                queries = [ Point_dep { index = 0; date = Period.start_date period } ];
                f = (fun values -> match values with [ v ] -> v *. rate | _ -> 0.0);
              })
          periods))

(* Balance: initial value + accumulated interest over time. *)
and balance = lazy (S.Point.accum ~label:"Balance" ~start_date ~initial_value:100.0 interest)

let () =
  let interest_cells =
    match S.Period.to_seq [ Lazy.force interest ] with
    | [ seq ] -> seq |> Seq.take 120 |> List.of_seq
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
