open Orcaset
module S = Series.Make ()

type historical_value = { period : Period.t; value : float }

let historical_units =
  [
    { period = Period.make (Date.make 2024 12 31) (Date.make 2025 3 31); value = 1_000. };
    { period = Period.make (Date.make 2025 3 31) (Date.make 2025 6 30); value = 1_100. };
    { period = Period.make (Date.make 2025 6 30) (Date.make 2025 9 30); value = 1_200. };
    { period = Period.make (Date.make 2025 9 30) (Date.make 2025 12 31); value = 1_300. };
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
let to_seed h = S.Period.Seed { period = h.period; f = (fun () -> h.value) }

let growth_step rate period =
  S.Period.Step
    {
      period;
      queries = [ Self { period = Period.shift forecast_lookback period; reduce = S.Period.reduce_sum } ];
      f = (fun values -> match values with [ prev ] -> prev *. (1.0 +. rate) | _ -> 0.0);
    }

let units : [ `Units ] S.Period.t =
  let seeds = Seq.map to_seed (List.to_seq historical_units) in
  let last_period = (List.rev historical_units |> List.hd).period in
  let rec forecast n prev_period () =
    let p = Period.shift forecast_stride prev_period in
    let step = if n < 4 then growth_step 0.05 p else growth_step 0.02 p in
    Seq.Cons (step, forecast (n + 1) p)
  in
  S.Period.unfold ~label:"Units" ~deps:[] (Seq.append seeds (forecast 0 last_period))

let revenue : [ `USD ] S.Period.t =
  let seeds = Seq.map to_seed (List.to_seq historical_revenue) in
  let last_period = (List.rev historical_revenue |> List.hd).period in
  let rec forecast prev_period () =
    let p = Period.shift forecast_stride prev_period in
    let step =
      S.Period.Step
        {
          period = p;
          queries = [ Dep { index = 0; period = p; reduce = S.Period.reduce_sum } ];
          f = (fun values -> match values with [ u ] -> u *. 10.0 | _ -> 0.0);
        }
    in
    Seq.Cons (step, forecast p)
  in
  S.Period.unfold ~label:"Revenue"
    ~deps:[ Period_dep (lazy (S.Period.convert ~label:"Units to revenue" (fun _ value -> value) (lazy units))) ]
    (Seq.append seeds (forecast last_period))

let output_periods = Period.make_seq ~start_date:(Date.make 2024 12 31) ~offset:forecast_stride |> Seq.take 6

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
