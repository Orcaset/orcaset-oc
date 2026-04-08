[@@@warning "-32"]

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
let interest_step balance period =
  S.Period.step ~period
    (S.Period.Query.point_or ~default:0.0 balance ~date:(Period.start_date period))
    (fun v -> v *. rate)

let rec interest =
  lazy
    (S.Period.unfold ~label:"Interest" ~deps:(S.Period.dep_point balance) ~cells:(fun balance ->
         Seq.map (interest_step balance) periods))

(* Balance: initial value + accumulated interest over time. *)
and balance = lazy (S.Point.accum ~label:"Balance" ~start_date ~initial_value:100.0 interest)

(* ── Output ───────────────────────────────────────────────────────────────── *)

let num_periods = 6
let query_periods = List.of_seq (Seq.take num_periods periods)

let () =
  let open S.Stmt in
  let stmt = group [ period_line (Lazy.force interest); point_line (Lazy.force balance) ] in
  print_string (pp stmt query_periods)
