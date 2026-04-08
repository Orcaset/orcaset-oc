open Orcaset
module S = Series.Make ()

type historical_value = { period : Period.t; value : float }

let historical_units =
  [
    { period = Period.make (Date.make 2024 12 31) (Date.make 2025 3 31); value = 1_000. };
    { period = Period.make (Date.make 2025 3 31) (Date.make 2025 6 30); value = 1_100. };
    { period = Period.make (Date.make 2025 6 30) (Date.make 2025 9 30); value = 1_200. };
    { period = Period.make (Date.make 2025 9 30) (Date.make 2025 12 31); value = 1_300. };
    { period = Period.make (Date.make 2025 12 31) (Date.make 2026 3 31); value = 1_400. };
  ]

let historical_revenue =
  [
    { period = Period.make (Date.make 2024 12 31) (Date.make 2025 3 31); value = 10_000. };
    { period = Period.make (Date.make 2025 3 31) (Date.make 2025 6 30); value = 11_000. };
    { period = Period.make (Date.make 2025 6 30) (Date.make 2025 9 30); value = 12_000. };
    { period = Period.make (Date.make 2025 9 30) (Date.make 2025 12 31); value = 13_000. };
  ]

let forecast_stride = Offset.make ~months:3 ~month_end:true ()
let forecast_lookback = Offset.make ~months:(-3) ~month_end:true ()
let to_cell h = Period_cell.const h.period (fun () -> h.value) Period_cell.proportional_split

let units : [ `Units ] S.Period.t =
  let units_hist : [ `Units ] S.Period.t =
    S.Period.of_seq ~label:"Units (historical)" (List.to_seq historical_units |> Seq.map to_cell)
  in
  let growth_step rate period =
    S.Period.step ~period
      (S.Period.Query.self
         ~period:(Period.shift forecast_lookback period)
         ~reduce:S.Period.reduce_sum)
      (fun prev -> prev *. (1.0 +. rate))
  in
  let units_forecast last_period =
    S.Period.unfold_self ~label:"Units (forecast)" ~cells:(fun () ->
        let rec forecast n prev_period () =
          let p = Period.next forecast_stride prev_period in
          let step = if n < 4 then growth_step 0.05 p else growth_step 0.02 p in
          Seq.Cons (step, forecast (n + 1) p)
        in
        forecast 0 last_period)
  in
  S.Period.extend ~label:"Units" units_hist units_forecast

let revenue : [ `USD ] S.Period.t =
  let hist =
    S.Period.of_seq ~label:"Revenue (historical)" (List.to_seq historical_revenue |> Seq.map to_cell)
  in
  let conversion = S.Period.convert ~label:"Units to revenue" (fun _ value -> value) (lazy units) in
  let forecast last_period =
    S.Period.unfold ~label:"Revenue (forecast)"
      ~deps:(S.Period.dep_period (lazy conversion))
      ~cells:(fun conversion ->
        let rec forecast prev_period () =
          let p = Period.next forecast_stride prev_period in
          let step =
            S.Period.step ~period:p
              (S.Period.Query.period conversion ~period:p ~reduce:S.Period.reduce_sum) (fun u ->
                u *. 10.0)
          in
          Seq.Cons (step, forecast p)
        in
        forecast last_period)
  in
  S.Period.extend ~label:"Revenue" hist forecast

(* ---- Print output --- *)

let output_periods =
  Period.make_seq ~start_date:(Date.make 2024 12 31) ~offset:forecast_stride |> Seq.take 6

let () =
  let periods = List.of_seq output_periods in
  let print_results results =
    List.iter
      (fun r ->
        match r with
        | S.Period { label; period; value = Amount v; _ } ->
            Printf.printf "%s for %s: %f\n" label (Period.to_string period) v
        | _ -> ())
      results
  in
  print_results (S.eval_many (S.Period.query periods units));
  print_results (S.eval_many (S.Period.query periods revenue))
