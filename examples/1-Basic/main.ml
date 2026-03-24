open Orcaset

(* Use Unfold to create a series that starts with an initial value and grows monthly by querying its own last monthly value and 
multiplying by the growth rate. *)
let revenue =
  let initial_value = 1000.0 in
  let growth_rate = 0.1 in
  let initial_start_date = Date.make 2024 12 31 in
  let offset = Offset.make ~months:1 ~month_end:true () in
  let initial_period = Period.make initial_start_date (Date.shift offset initial_start_date) in
  let initial_cell = Cell.const initial_period (fun () -> initial_value) Cell.proportional_split in
  Series.unfold ~deps:[] (fun self_query _dep_queries ->
      let rec generate_cells last_period () =
        let prior_cell = self_query last_period in
        let current_period = Period.shift offset last_period in
        Seq.Cons
          ( Cell.deps current_period (prior_cell |> List.of_seq) (fun prior_values ->
                match prior_values with
                | [ last_value ] -> last_value *. (1.0 +. growth_rate)
                | _ -> 0.0),
            generate_cells current_period )
      in
      Seq.cons initial_cell (generate_cells initial_period))

let costs = Series.map (fun r -> r *. -0.6) (lazy revenue)
let profit = Series.sum revenue costs

let () =
  match Series.to_seq [ profit ] with
  | [ profit_seq ] ->
      Seq.iter
        (fun cell ->
          let period = Cell.cell_period cell in
          let value = Cell.eval cell |> snd in
          Printf.printf "%s: %.2f\n" (Period.to_string period) value)
        (profit_seq |> Seq.take 6)
  | _ -> assert false
