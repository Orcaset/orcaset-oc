open Orcaset
module S = Series.Make ()

(* Basic financial model example with three line items: revenue, expenses, and income.
   Revenue: Grows at a constant monthly rate using Unfold
   Expenses: Fixed percentage of revenue
   Income: Sum of revenue and expenses
 *)

let initial_value = 1000.0
let expense_margin = -0.6
let initial_start_date = Date.make 2025 12 31
let offset = Offset.make ~months:3 ~month_end:true ()
let growth_rate = 0.25
let initial_period = Period.make initial_start_date (Date.shift offset initial_start_date)

let revenue_step curr_period last_period =
  S.Period.step ~period:curr_period
    (S.Period.Query.self ~period:last_period ~reduce:S.Period.reduce_sum) (fun last_value ->
      let yf = Yf.cmonthly (Period.start_date curr_period) (Period.end_date curr_period) in
      let growth = (1.0 +. growth_rate) ** yf in
      last_value *. growth)

let revenue =
  let rec generate_cells last_period () =
    let current_period = Period.next offset last_period in
    Seq.Cons (revenue_step current_period last_period, generate_cells current_period)
  in
  S.Period.unfold_self ~label:"Revenue" ~cells:(fun () ->
      Seq.cons
        (S.Period.const ~period:initial_period (fun () -> initial_value))
        (generate_cells initial_period))

let expenses = S.Period.map ~label:"Expenses" (fun r -> r *. expense_margin) (lazy revenue)
let income = S.Period.sum ~label:"Income" [ lazy revenue; lazy expenses ]

(* ---- Output --- *)
let num_periods = 6

let query_periods =
  let query_offset = Offset.make ~months:1 ~month_end:true () in
  List.of_seq
    (Seq.take num_periods (Period.make_seq ~start_date:initial_start_date ~offset:query_offset))

let () =
  (* Write dependency graph for income to a dot file *)
  let oc = open_out "examples/1-Basic/income_deps.dot" in
  let ppf = Format.formatter_of_out_channel oc in
  Graph.pp_dot ~label:S.label_of_id ppf [ S.period_to_graph income ];
  Format.pp_print_flush ppf ();
  close_out oc;

  let open S.Stmt in
  let stmt = period_total income [ period_line revenue; period_line expenses ] in
  print_string (pp stmt query_periods)
