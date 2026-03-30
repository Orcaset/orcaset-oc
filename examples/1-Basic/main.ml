open Orcaset
module S = Series.Make ()

(* Use Unfold to create a series that starts with an initial value and grows monthly by querying its own last monthly value and 
multiplying by the growth rate. *)

let initial_value = 1000.0
let growth_rate = 0.1
let initial_start_date = Date.make 2024 12 31
let offset = Offset.make ~months:1 ~month_end:true ()
let initial_period = Period.make initial_start_date (Date.shift offset initial_start_date)

let revenue =
  let rec generate_cells last_period () =
    let current_period = Period.shift offset last_period in
    Seq.Cons
      ( S.Period.Step
          {
            period = current_period;
            queries = [ Self { period = last_period; reduce = S.Period.reduce_sum } ];
            f =
              (fun values ->
                match values with [ last_value ] -> last_value *. (1.0 +. growth_rate) | _ -> 0.0);
          },
        generate_cells current_period )
  in
  S.Period.unfold ~label:"Revenue" ~deps:[]
    (Seq.cons
       (S.Period.Seed { period = initial_period; f = (fun () -> initial_value) })
       (generate_cells initial_period))

let costs = S.Period.map ~label:"Costs" (fun r -> r *. -0.6) (lazy revenue)
let profit = S.Period.sum ~label:"Profit" (lazy revenue) (lazy costs)

let () =
  match S.Period.to_seq [ profit ] with
  | [ profit_seq ] ->
      Seq.iter
        (fun cell ->
          let period = Period_cell.period cell in
          let value = Eval.one (Eval.PeriodCell cell) in
          Printf.printf "%s: %.2f\n" (Period.to_string period) value)
        (profit_seq |> Seq.take 6)
  | _ -> assert false
