open Orcaset

let d y m day = Date.make y m day
let p s e = Period.make s e
let sum_floats = List.fold_left (fun acc -> function Some v -> acc +. v | None -> acc) 0.0
let days n = Offset.make ~days:n ()
let months n = Offset.make ~months:n ()

(* Build a span series of [n] consecutive monthly cells starting at [start], with
   value [value_of i] for cell [i]. Useful for tests. *)
let monthly_series ~start ~n value_of =
  Series.Span_series.Unfold
    {
      id = Series.new_id ();
      init = 0;
      deps = (fun () -> Series.Deps.none);
      step =
        (fun () i ->
          if i >= n then None
          else
            let period = p (Date.shift (months i) start) (Date.shift (months (i + 1)) start) in
            let v = value_of i in
            Some (Series.step ~period ~split:Series.proportional_split (fun () -> v), i + 1));
    }

(* A single-step unfold with no deps. Emits one cell and terminates. *)
let test_unfold_no_deps_single_step () =
  let open Series in
  let series =
    Span_series.Unfold
      {
        id = new_id ();
        init = 0;
        deps = (fun () -> Deps.none);
        step =
          (fun () n ->
            if n >= 1 then None
            else
              let period = p (d 2026 1 1) (d 2026 2 1) in
              Some (step ~period ~split:proportional_split (fun () -> 42.0), n + 1));
      }
  in
  let cache = make_cache () in
  let total = query_span cache series ~period:(p (d 2026 1 1) (d 2026 2 1)) ~reduce:sum_floats in
  Alcotest.(check (float 1e-9)) "value" 42.0 total

(* Multi-step unfold with termination. *)
let test_unfold_multi_step () =
  let open Series in
  let start = d 2026 1 1 in
  let series =
    Span_series.Unfold
      {
        id = new_id ();
        init = 0;
        deps = (fun () -> Deps.none);
        step =
          (fun () n ->
            if n >= 3 then None
            else
              let period =
                p (Date.shift (days (30 * n)) start) (Date.shift (days (30 * (n + 1))) start)
              in
              Some (step ~period ~split:proportional_split (fun () -> Float.of_int n), n + 1));
      }
  in
  let cache = make_cache () in
  let total =
    query_span cache series ~period:(p start (Date.shift (days 90) start)) ~reduce:sum_floats
  in
  Alcotest.(check (float 1e-9)) "sum 0+1+2" 3.0 total

let test_derived_series_reuses_cached_dep_sequence () =
  let open Series in
  let start = d 2026 1 1 in
  let offset = months 1 in
  let step_count = ref 0 in
  let base =
    Span_series.Unfold
      {
        id = new_id ();
        init = 0;
        deps = (fun () -> Deps.none);
        step =
          (fun () n ->
            if n >= 4 then None
            else begin
              incr step_count;
              let period = p (Date.shift (months n) start) (Date.shift (months (n + 1)) start) in
              Some (step ~period ~split:proportional_split (fun () -> Float.of_int (n + 1)), n + 1)
            end);
      }
  in
  let scaled = Span_series.scale 2.0 base in
  let summed = Span_series.sum base scaled in
  let cache = make_cache () in
  let periods = Period.make_seq ~start ~offset |> Seq.take 4 |> List.of_seq in
  let values_for series =
    List.map (fun period -> query_span cache series ~period ~reduce:sum_floats) periods
  in
  ignore (values_for base);
  ignore (values_for scaled);
  ignore (values_for summed);
  Alcotest.(check int) "base unfold steps produced once" 4 !step_count

(* Unfold with a span dep: derive values by querying the dep. *)
let test_unfold_with_span_dep () =
  let open Series in
  let start = d 2026 1 1 in
  let base = monthly_series ~start ~n:3 (fun i -> 10.0 *. Float.of_int (i + 1)) in
  let doubled =
    Span_series.Unfold
      {
        id = new_id ();
        init = 0;
        deps = (fun () -> Deps.span_dep base);
        step =
          (fun read_base n ->
            if n >= 3 then None
            else
              let period = p (Date.shift (months n) start) (Date.shift (months (n + 1)) start) in
              let value () =
                let v = read_base ~period ~reduce:sum_floats in
                2.0 *. v
              in
              Some (step ~period ~split:proportional_split value, n + 1));
      }
  in
  let cache = make_cache () in
  let total = query_span cache doubled ~period:(p start (d 2026 4 1)) ~reduce:sum_floats in
  Alcotest.(check (float 1e-9)) "sum 20+40+60" 120.0 total

(* Dependency extraction: an Unfold's deps show up in [dependencies]. *)
let test_unfold_dependencies () =
  let open Series in
  let start = d 2026 1 1 in
  let base_a = monthly_series ~start ~n:1 (fun _ -> 1.0) in
  let base_b = monthly_series ~start ~n:1 (fun _ -> 2.0) in
  let u =
    Span_series.Unfold
      {
        id = new_id ();
        init = ();
        deps =
          (fun () ->
            let open Deps in
            let+ a = span_dep base_a and+ b = span_dep base_b in
            (~a, ~b));
        step = (fun (~a:_, ~b:_) () -> None);
      }
  in
  let deps = dependencies (Span_series u) in
  Alcotest.(check int) "two top-level deps" 2 (List.length deps);
  let all_span =
    List.for_all
      (fun d ->
        let (Series s) = d.series in
        match s with Span_series _ -> true | Point_series _ -> false)
      deps
  in
  Alcotest.(check bool) "both are span deps" true all_span

(* A recursive Unfold that reads its own prior-period value. Exercises the thunked-applicative
   route's ability to support [let rec] self-references. *)
let test_unfold_self_recursive () =
  let open Series in
  let start = d 2026 1 1 in
  let rec rev : Span_series.t =
    Span_series.Unfold
      {
        id = new_id ();
        init = 0;
        deps = (fun () -> Deps.span_dep rev);
        step =
          (fun prev i ->
            if i >= 4 then None
            else
              let period = p (Date.shift (months i) start) (Date.shift (months (i + 1)) start) in
              let value () =
                if i = 0 then 100.0
                else
                  prev
                    ~period:(p (Date.shift (months (i - 1)) start) (Date.shift (months i) start))
                    ~reduce:sum_floats
                  *. 1.10
              in
              Some (step ~period ~split:proportional_split value, i + 1));
      }
  in
  let cache = make_cache () in
  let total =
    query_span cache rev ~period:(p start (Date.shift (months 4) start)) ~reduce:sum_floats
  in
  (* 100 + 110 + 121 + 133.1 = 464.1 *)
  Alcotest.(check (float 1e-9)) "geometric 10% growth, 4 months" 464.1 total

let test_unfold_same_period_self_reference_converges () =
  let open Series in
  let start = d 2026 1 1 in
  let period = p start (Date.shift (months 1) start) in
  let rec s : Span_series.t =
    Span_series.Unfold
      {
        id = new_id ();
        init = 0;
        deps = (fun () -> Deps.span_dep s);
        step =
          (fun self i ->
            if i >= 1 then None
            else
              let value () = 10.0 +. (0.5 *. self ~period ~reduce:sum_floats) in
              Some (step ~period ~split:proportional_split value, i + 1));
      }
  in
  let cache = make_cache () in
  let value = query_span cache s ~period ~reduce:sum_floats in
  Alcotest.(check (float 1e-6)) "x = 10 + 0.5x" 20.0 value

let test_unfold_future_reference_converges () =
  let open Series in
  let start = d 2026 1 1 in
  let period0 = p start (Date.shift (months 1) start) in
  let period1 = p (Date.shift (months 1) start) (Date.shift (months 2) start) in
  let rec s : Span_series.t =
    Span_series.Unfold
      {
        id = new_id ();
        init = 0;
        deps = (fun () -> Deps.span_dep s);
        step =
          (fun self i ->
            if i >= 2 then None
            else
              let period = if i = 0 then period0 else period1 in
              let value () =
                if i = 0 then 10.0 +. (0.5 *. self ~period:period1 ~reduce:sum_floats)
                else 4.0 +. (0.25 *. self ~period:period0 ~reduce:sum_floats)
              in
              Some (step ~period ~split:proportional_split value, i + 1));
      }
  in
  let cache = make_cache () in
  let value0 = query_span cache s ~period:period0 ~reduce:sum_floats in
  let value1 = query_span cache s ~period:period1 ~reduce:sum_floats in
  Alcotest.(check (float 1e-6)) "future-dependent x0" (96.0 /. 7.0) value0;
  Alcotest.(check (float 1e-6)) "future-dependent x1" (52.0 /. 7.0) value1

(*  Each quarter reads [self] over a fixed [search_period], so the fixed-point for one cell depends
    on quarters that lie before, overlapping, and after that cell within the same unfold. *)
let test_unfold_past_current_future_self_reference () =
  let open Series in
  let initial_period = p (d 2024 12 31) (d 2025 3 31) in
  let search_period = p (d 2024 12 31) (d 2025 12 31) in
  let offset = Offset.make ~months:3 ~month_end:true () in
  let rec test_series =
    Span_series.Unfold
      {
        id = new_id ();
        deps = (fun () -> Deps.span_dep test_series);
        init = initial_period;
        step =
          (fun self period ->
            let qtr = ((Period.end_ period |> Date.month) / 3) + 1 in
            let qtr_ratio = Float.of_int qtr /. 10.0 in
            let annual_total = 100.0 +. (0.2 *. self ~period:search_period ~reduce:sum_floats) in
            let next_period = Period.next offset period in
            let value () = annual_total *. qtr_ratio in
            Some (step ~period ~split:proportional_split value, next_period));
      }
  in
  let cache = make_cache () in
  let quarters =
    Period.make_seq ~start:(Period.start initial_period) ~offset |> Seq.take 8 |> List.of_seq
  in
  let values =
    List.map (fun period -> query_span cache test_series ~period ~reduce:sum_floats) quarters
  in
  let expected = [ 20.; 30.; 40.; 50.; 20.; 30.; 40.; 50. ] in
  List.iteri
    (fun i (e, a) -> Alcotest.(check (float 1e-6)) (Printf.sprintf "quarter %d" i) e a)
    (List.combine expected values)

let test_unfold_non_convergence_fails () =
  let open Series in
  let start = d 2026 1 1 in
  let period = p start (Date.shift (months 1) start) in
  let rec s : Span_series.t =
    Span_series.Unfold
      {
        id = new_id ();
        init = 0;
        deps = (fun () -> Deps.span_dep s);
        step =
          (fun self i ->
            if i >= 1 then None
            else
              let value () = 1.0 +. self ~period ~reduce:sum_floats in
              Some (step ~period ~split:proportional_split value, i + 1));
      }
  in
  let cache = make_cache () in
  let failed =
    try
      ignore (query_span cache s ~period ~reduce:sum_floats);
      false
    with Resolution_failed _ -> true
  in
  Alcotest.(check bool) "non-convergence fails" true failed

let test_unfold_current_mutual_recursion_fails_deterministically () =
  let open Series in
  let start = d 2026 1 1 in
  let period = p start (Date.shift (months 1) start) in
  let rec revenue : Span_series.t =
    Span_series.Unfold
      {
        id = new_id ();
        init = 0;
        deps = (fun () -> Deps.none);
        step =
          (fun () i ->
            if i >= 1 then None
            else Some (step ~period ~split:proportional_split (fun () -> 100.0), i + 1));
      }
  and costs : Span_series.t =
    Span_series.Unfold
      {
        id = new_id ();
        init = 0;
        deps =
          (fun () ->
            let open Deps in
            let+ rev = span_dep revenue and+ cst = span_dep costs in
            (~rev, ~cst));
        step =
          (fun (~rev, ~cst) i ->
            if i >= 1 then None
            else
              let value () =
                cst ~period ~reduce:sum_floats +. (0.10 *. rev ~period ~reduce:sum_floats)
              in
              Some (step ~period ~split:proportional_split value, i + 1));
      }
  in
  let cache = make_cache () in
  let failed =
    try
      ignore (query_span cache costs ~period ~reduce:sum_floats);
      false
    with Resolution_failed _ -> true
  in
  Alcotest.(check bool) "same-period divergent mutual recursion fails" true failed

let test_point_same_date_self_reference_converges () =
  let open Series in
  let start = d 2026 1 1 in
  let period = p start (Date.shift (months 1) start) in
  let base = Point_series.Const { id = new_id (); period; value = (fun () -> 10.0) } in
  let rec self : Point_series.t =
    Point_series.Map2
      {
        id = new_id ();
        a = base;
        b = self;
        f =
          (fun base_value self_value ->
            Option.value ~default:0.0 base_value +. (0.5 *. Option.value ~default:0.0 self_value));
      }
  in
  let cache = make_cache () in
  let value = query_point cache self ~date:start ~default:0.0 in
  Alcotest.(check (float 1e-6)) "point x = 10 + 0.5x" 20.0 value

let tests =
  List.map
    (fun (name, f) -> Alcotest.test_case name `Quick f)
    [
      ("unfold no deps single step", test_unfold_no_deps_single_step);
      ("unfold multi step", test_unfold_multi_step);
      ("derived series reuses cached dep sequence", test_derived_series_reuses_cached_dep_sequence);
      ("unfold with span dep", test_unfold_with_span_dep);
      ("unfold dependencies extraction", test_unfold_dependencies);
      ("unfold self-recursive", test_unfold_self_recursive);
      ( "unfold same-period self-reference converges",
        test_unfold_same_period_self_reference_converges );
      ("unfold future reference converges", test_unfold_future_reference_converges);
      ( "unfold past/current/future self-reference (circ window)",
        test_unfold_past_current_future_self_reference );
      ("unfold non-convergence fails", test_unfold_non_convergence_fails);
      ( "unfold current mutual recursion fails deterministically",
        test_unfold_current_mutual_recursion_fails_deterministically );
      ("point same-date self-reference converges", test_point_same_date_self_reference_converges);
    ]
