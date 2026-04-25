open Orcaset

(* This example models an account balance that compounds monthly. Period interest
   flows are computed from the opening balance, then accumulated into an as-of
   point balance that can be queried on any date. *)

let start_date = Date.make 2025 12 31
let offset = Offset.make ~months:1 ~month_end:true ()
let rate = 0.03
let periods = Period.make_seq ~start_date ~offset
let principal = lazy (Series.Point.const ~label:"Principal" 100.0)

(* Each month's interest is based on the balance at the start of that month. *)
let interest_step balance period =
  Series.Period.step ~period
    (Series.Period.Query.point_or ~default:0.0 balance ~date:(Period.start_date period))
    (fun opening_balance -> opening_balance *. rate)

(* Create a step function that accumulates total interest over time. *)
let accrued_interest_step interest period =
  Series.Point.step ~period
    (Series.Point.Query.period interest ~period ~reduce:Series.Period.reduce_sum)
    Fun.id

(* Define line items. *)
let rec interest =
  lazy
    (Series.Period.unfold_seq ~label:"Interest" ~deps:(Series.Period.dep_point balance)
       ~cells:(fun balance -> Seq.map (interest_step balance) periods))

and accrued_interest =
  lazy
    (Series.Point.unfold_seq ~label:"Accrued interest" ~deps:(Series.Point.dep_period interest)
       ~cells:(fun interest -> Seq.map (accrued_interest_step interest) periods))

and balance = lazy (Series.Point.sum ~label:"Balance" principal accrued_interest)

(* ── Output ───────────────────────────────────────────────────────────────── *)

let num_periods = 6
let query_periods = List.of_seq (Seq.take num_periods periods)

let () =
  let open Series.Stmt in
  let stmt =
    group
      [
        period_line (Lazy.force interest);
        point_line (Lazy.force accrued_interest);
        point_line (Lazy.force balance);
      ]
  in
  print_string (pp stmt query_periods)
