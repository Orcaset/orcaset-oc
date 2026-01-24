module Accrual = Orcaset.Accrual
module Period = Orcaset.Period

type t = { recurring : Accrual.t Seq.t; non_recurring : Accrual.t Seq.t; total : Accrual.t Seq.t }

(* Random walk with drift for non-recurring revenue *)
let make_random_walk_seq ~start_period ~initial_value ~drift ~volatility ~seed ~freq =
  let rng = Random.State.make [| seed |] in
  let first_accrual =
    Accrual.make ~period:start_period ~value:(lazy initial_value) ~split_fn:Accrual.default_split_fn
  in
  let periods = Period.make_seq ~start_date:start_period.end_date ~offset:freq in

  Seq.cons first_accrual (fun () ->
      Seq.unfold
        (fun (prev_value, period_seq) ->
          match period_seq () with
          | Seq.Nil -> None
          | Seq.Cons (period, rest) ->
              (* Generate random shock: normal distribution approximated by sum of uniforms *)
              let shock =
                let sum_uniforms =
                  List.init 12 (fun _ -> Random.State.float rng 1.0) |> List.fold_left ( +. ) 0.0
                in
                (sum_uniforms -. 6.0) *. volatility
              in
              let next_value = prev_value +. drift +. shock in
              let accrual =
                Accrual.make ~period ~value:(lazy next_value) ~split_fn:Accrual.default_split_fn
              in
              Some (accrual, (next_value, rest)))
        (initial_value, periods) ())

let make ~start_period ~recurring_first ~recurring_growth ~non_recurring_first ~non_recurring_drift
    ~non_recurring_volatility ~seed ~freq ~yf =
  let recurring =
    Accrual.const_annual_growth_seq ~start_date:start_period.Period.start_date
      ~initial_value:recurring_first ~rate:recurring_growth ~freq ~yf
  in
  let non_recurring =
    make_random_walk_seq ~start_period ~initial_value:non_recurring_first ~drift:non_recurring_drift
      ~volatility:non_recurring_volatility ~seed ~freq
    |> Seq.memoize
  in
  let total = Accrual.sum_seq recurring non_recurring in
  { recurring; non_recurring; total }
