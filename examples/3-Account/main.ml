open Orcaset

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
    (Series.Period.unfold ~deps:[ Point_dep balance ]
       (Seq.map
          (fun period ->
            Series.Period.Step
              {
                period;
                queries = [ Point_dep { index = 0; date = Period.start_date period } ];
                f = (fun values -> match values with [ v ] -> v *. rate | _ -> 0.0);
              })
          periods))

(* Balance: initial value + accumulated interest over time. *)
and balance = lazy (Series.Point.accum ~start_date ~initial_value:100.0 interest)

let () =
  let interest_cells =
    match Series.Period.to_seq [ Lazy.force interest ] with
    | [ seq ] -> seq |> Seq.take 120
    | _ -> assert false
  in
  Printf.printf "%-25s %16s %16s %16s\n" "Period" "Start Balance" "Interest" "End Balance";
  Printf.printf "%s\n" (String.make 73 '-');
  Seq.iter
    (fun cell ->
      let period = Period_cell.period cell in
      let _, interest_value = Eval.eval_period cell in
      let start_balance =
        match Series.Point.query (Period.start_date period) (Lazy.force balance) with
        | Some c -> Eval.eval_point c |> snd
        | None -> 0.0
      in
      let end_balance =
        match Series.Point.query (Period.end_date period) (Lazy.force balance) with
        | Some c -> Eval.eval_point c |> snd
        | None -> 0.0
      in
      Printf.printf "%-25s %16.2f %16.2f %16.2f\n" (Period.to_string period) start_balance
        interest_value end_balance)
    interest_cells
