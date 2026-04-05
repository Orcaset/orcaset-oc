[@@@warning "-32"]

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

(* TODO: restore output once eval_many is available *)
