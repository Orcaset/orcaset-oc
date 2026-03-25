open Orcaset

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
      ( Series.Period.Step
          {
            period = current_period;
            queries = [ Self { period = last_period; reduce = Series.Period.reduce_sum } ];
            f =
              (fun values ->
                match values with [ last_value ] -> last_value *. (1.0 +. growth_rate) | _ -> 0.0);
          },
        generate_cells current_period )
  in
  Series.Period.unfold ~deps:[]
    (Seq.cons
       (Series.Period.Seed { period = initial_period; f = (fun () -> initial_value) })
       (generate_cells initial_period))

let costs = Series.Period.map (fun r -> r *. -0.6) (lazy revenue)
let profit = Series.Period.sum revenue costs

let () =
  match Series.Period.to_seq [ profit ] with
  | [ profit_seq ] ->
      Seq.iter
        (fun cell ->
          let period = Period_cell.period cell in
          let value = Eval.eval_period cell |> snd in
          Printf.printf "%s: %.2f\n" (Period.to_string period) value)
        (profit_seq |> Seq.take 6)
  | _ -> assert false
