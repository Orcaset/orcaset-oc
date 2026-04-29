open Orcaset

let d y m day = Date.make y m day
let p s e = Period.make s e
let sum_floats = Series.sum_float_opt ~fill:0.0
let days n = Offset.make ~days:n ()
let months n = Offset.make ~months:n ()

(* Build a span series of [n] consecutive monthly cells starting at [start], with
   value [value_of i] for cell [i]. Useful for tests. *)
let monthly_series ~start ~n value_of =
  Series.Spans.Unfold
    {
      id = Series.new_id ();
      label = None;
      init = 0;
      deps = (fun () -> Series.Deps.none);
      cells =
        (fun () i ->
          if i >= n then None
          else
            let period = p (Date.shift (months i) start) (Date.shift (months (i + 1)) start) in
            Some
              ( Series.cell ~period ~split:Series.proportional_split
                  (Series.Formula.pure (value_of i)),
                i + 1 ));
    }

let single_span_series_with_id ~id ~period ~split value =
  Series.Spans.Unfold
    {
      id;
      label = None;
      init = false;
      deps = (fun () -> Series.Deps.none);
      cells =
        (fun () emitted ->
          if emitted then None
          else Some (Series.cell ~period ~split (Series.Formula.pure value), true));
    }

let single_span_series ~period ~split value =
  single_span_series_with_id ~id:(Series.new_id ()) ~period ~split value

let single_point_series_with_id ~id ~period value =
  Series.Points.Const { id; label = None; period; value = (fun () -> value) }

let check_invalid_arg ~prefix f =
  try
    f ();
    Alcotest.fail "expected Invalid_argument"
  with
  | Invalid_argument message ->
      Alcotest.(check bool) "message prefix" true (String.starts_with ~prefix message)
  | exn -> Alcotest.failf "expected Invalid_argument, got %s" (Printexc.to_string exn)

let test_span_const () =
  let open Series in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let series = Spans.Const { id = new_id (); label = None; period; value = (fun () -> 42.0) } in
  let cache = make_cache () in
  let total = query_span cache series ~period ~reduce:sum_floats in
  Alcotest.(check (float 1e-9)) "const span value" 42.0 total

let test_span_map_applies_function () =
  let open Series in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let base = single_span_series ~period ~split:proportional_split 10.0 in
  let mapped =
    Spans.Map { id = new_id (); label = None; dep = base; f = (fun value -> value *. -0.9) }
  in
  let cache = make_cache () in
  let total = query_span cache mapped ~period ~reduce:sum_floats in
  Alcotest.(check (float 1e-9)) "mapped span value" (-9.0) total

let test_span_map2_applies_function () =
  let open Series in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let a = single_span_series ~period ~split:proportional_split 10.0 in
  let b = single_span_series ~period ~split:proportional_split 3.0 in
  let mapped =
    Spans.Map2
      {
        id = new_id ();
        label = None;
        a;
        b;
        f = (fun a b -> Option.value ~default:0.0 a -. Option.value ~default:0.0 b);
      }
  in
  let cache = make_cache () in
  let total = query_span cache mapped ~period ~reduce:sum_floats in
  Alcotest.(check (float 1e-9)) "map2 span value" 7.0 total

let test_span_convenience_constructors () =
  let open Series in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let a = single_span_series ~period ~split:proportional_split 10.0 in
  let b = single_span_series ~period ~split:proportional_split 2.0 in
  let cache = make_cache () in
  let check name expected series =
    Alcotest.(check (float 1e-9)) name expected (query_span cache series ~period ~reduce:sum_floats)
  in
  let total = Spans.sum ~label:"Total" a b in
  Alcotest.(check (option string)) "label" (Some "Total") (Spans.label total);
  check "neg" (-10.0) (Spans.neg a);
  check "scale" 30.0 (Spans.scale 3.0 a);
  check "sum" 12.0 total;
  check "sub" 8.0 (Spans.sub a b);
  check "mul" 20.0 (Spans.mul a b);
  check "div" 5.0 (Spans.div a b)

let test_span_convenience_fill () =
  let open Series in
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let feb = p (d 2026 2 1) (d 2026 3 1) in
  let query_period = p (d 2026 1 1) (d 2026 3 1) in
  let a = single_span_series ~period:jan ~split:proportional_split 10.0 in
  let b = single_span_series ~period:feb ~split:proportional_split 3.0 in
  let filled = Spans.sum ~fill:1.0 a b in
  let cache = make_cache () in
  let total = query_span cache filled ~period:query_period ~reduce:sum_floats in
  Alcotest.(check (float 1e-9)) "filled missing span sides" 15.0 total

let test_sum_float_opts () =
  let open Series in
  Alcotest.(check (float 1e-9))
    "fills none values" 12.0
    (sum_float_opt ~fill:2.0 [ Some 3.0; None; Some 5.0; None ])

let test_non_cyclic_map_preserves_gaps_and_evaluates_once () =
  let open Series in
  let calls = ref 0 in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let base =
    Spans.Const
      {
        id = new_id ();
        label = None;
        period;
        value =
          (fun () ->
            incr calls;
            10.0);
      }
  in
  let mapped =
    Spans.Map { id = new_id (); label = None; dep = base; f = (fun value -> value *. 2.0) }
  in
  let cache = make_cache () in
  let query_period = p (d 2025 12 1) (d 2026 3 1) in
  let values = query_span cache mapped ~period:query_period ~reduce:Fun.id in
  (match values with
  | [ None; Some value; None ] -> Alcotest.(check (float 1e-9)) "mapped value" 20.0 value
  | _ -> Alcotest.fail "expected leading and trailing gaps");
  Alcotest.(check int) "constant evaluated once" 1 !calls

let test_point_const_map_map2 () =
  let open Series in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let date = d 2026 1 15 in
  let a = Points.Const { id = new_id (); label = None; period; value = (fun () -> 10.0) } in
  let b = Points.Const { id = new_id (); label = None; period; value = (fun () -> 3.0) } in
  let mapped =
    Points.Map { id = new_id (); label = None; dep = a; f = (fun value -> value *. 2.0) }
  in
  let mapped2 =
    Points.Map2
      {
        id = new_id ();
        label = None;
        a;
        b;
        f = (fun a b -> Option.value ~default:0.0 a +. Option.value ~default:0.0 b);
      }
  in
  let cache = make_cache () in
  Alcotest.(check (float 1e-9)) "const point" 10.0 (query_point cache a ~date ~default:0.0);
  Alcotest.(check (float 1e-9)) "mapped point" 20.0 (query_point cache mapped ~date ~default:0.0);
  Alcotest.(check (float 1e-9)) "map2 point" 13.0 (query_point cache mapped2 ~date ~default:0.0)

let test_point_convenience_constructors () =
  let open Series in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let date = d 2026 1 15 in
  let a = Points.Const { id = new_id (); label = None; period; value = (fun () -> 10.0) } in
  let b = Points.Const { id = new_id (); label = None; period; value = (fun () -> 2.0) } in
  let cache = make_cache () in
  let check name expected series =
    Alcotest.(check (float 1e-9)) name expected (query_point cache series ~date ~default:0.0)
  in
  let total = Points.sum ~label:"Total" a b in
  Alcotest.(check (option string)) "label" (Some "Total") (Points.label total);
  check "neg" (-10.0) (Points.neg a);
  check "scale" 30.0 (Points.scale 3.0 a);
  check "sum" 12.0 total;
  check "sub" 8.0 (Points.sub a b);
  check "mul" 20.0 (Points.mul a b);
  check "div" 5.0 (Points.div a b)

let test_point_convenience_fill () =
  let open Series in
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let feb = p (d 2026 2 1) (d 2026 3 1) in
  let a = Points.Const { id = new_id (); label = None; period = jan; value = (fun () -> 10.0) } in
  let b = Points.Const { id = new_id (); label = None; period = feb; value = (fun () -> 3.0) } in
  let filled = Points.sum ~fill:1.0 a b in
  let cache = make_cache () in
  Alcotest.(check (float 1e-9))
    "filled missing point side" 11.0
    (query_point cache filled ~date:(d 2026 1 15) ~default:0.0)

let test_point_accum_uses_span_changes () =
  let open Series in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let changes = single_span_series ~period ~split:proportional_split 10.0 in
  let balance = Points.Accum { id = new_id (); label = None; init = 100.0; changes } in
  let cache = make_cache () in
  Alcotest.(check (float 1e-9))
    "initial balance" 100.0
    (query_point cache balance ~date:(Period.start period) ~default:0.0);
  Alcotest.(check (float 1e-9))
    "ending balance" 110.0
    (query_point cache balance ~date:(Period.end_ period) ~default:0.0)

let test_label_accessors () =
  let open Series in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let span =
    Spans.Const { id = new_id (); label = Some "Revenue"; period; value = (fun () -> 42.0) }
  in
  let point =
    Points.Const { id = new_id (); label = Some "Balance"; period; value = (fun () -> 10.0) }
  in
  Alcotest.(check (option string)) "span label" (Some "Revenue") (Spans.label span);
  Alcotest.(check (option string)) "point label" (Some "Balance") (Points.label point);
  Alcotest.(check (option string)) "wrapped span label" (Some "Revenue") (label (Span_series span));
  Alcotest.(check (option string))
    "wrapped point label" (Some "Balance") (label (Point_series point))

(* A single-step Unfold with no deps. Emits one cell and terminates. *)
let test_unfold_no_deps_single_step () =
  let open Series in
  let series =
    Spans.Unfold
      {
        id = new_id ();
        label = None;
        init = 0;
        deps = (fun () -> Deps.none);
        cells =
          (fun () n ->
            if n >= 1 then None
            else
              let period = p (d 2026 1 1) (d 2026 2 1) in
              Some (cell ~period ~split:proportional_split (Formula.pure 42.0), n + 1));
      }
  in
  let cache = make_cache () in
  let total = query_span cache series ~period:(p (d 2026 1 1) (d 2026 2 1)) ~reduce:sum_floats in
  Alcotest.(check (float 1e-9)) "value" 42.0 total

(* Multi-step Unfold with termination. *)
let test_unfold_multi_step () =
  let open Series in
  let start = d 2026 1 1 in
  let series =
    Spans.Unfold
      {
        id = new_id ();
        label = None;
        init = 0;
        deps = (fun () -> Deps.none);
        cells =
          (fun () n ->
            if n >= 3 then None
            else
              let period =
                p (Date.shift (days (30 * n)) start) (Date.shift (days (30 * (n + 1))) start)
              in
              Some (cell ~period ~split:proportional_split (Formula.pure (Float.of_int n)), n + 1));
      }
  in
  let cache = make_cache () in
  let total =
    query_span cache series ~period:(p start (Date.shift (days 90) start)) ~reduce:sum_floats
  in
  Alcotest.(check (float 1e-9)) "sum 0+1+2" 3.0 total

(* Unfold with a span dep: derive values by querying the dep. *)
let test_unfold_with_span_dep () =
  let open Series in
  let start = d 2026 1 1 in
  let base = monthly_series ~start ~n:3 (fun i -> 10.0 *. Float.of_int (i + 1)) in
  let doubled =
    Spans.Unfold
      {
        id = new_id ();
        label = None;
        init = 0;
        deps = (fun () -> Deps.span_dep base);
        cells =
          (fun read_base n ->
            if n >= 3 then None
            else
              let period = p (Date.shift (months n) start) (Date.shift (months (n + 1)) start) in
              let formula =
                let open Formula in
                let+ v = read_base ~period ~reduce:sum_floats in
                2.0 *. v
              in
              Some (cell ~period ~split:proportional_split formula, n + 1));
      }
  in
  let cache = make_cache () in
  let total = query_span cache doubled ~period:(p start (d 2026 4 1)) ~reduce:sum_floats in
  Alcotest.(check (float 1e-9)) "sum 20+40+60" 120.0 total

let test_repeated_clip_of_split_span () =
  let open Series in
  let full_period = p (d 2026 1 1) (d 2026 4 1) in
  let clipped_period = p (d 2026 2 1) (d 2026 3 1) in
  let proportional = single_span_series ~period:full_period ~split:proportional_split 90.0 in
  let const = single_span_series ~period:full_period ~split:const_split 5.0 in
  let cache = make_cache () in
  let proportional_total =
    query_span cache proportional ~period:clipped_period ~reduce:sum_floats
  in
  let const_total = query_span cache const ~period:clipped_period ~reduce:sum_floats in
  let expected_proportional =
    let clipped_days = Float.of_int (Date.diff (d 2026 3 1) (d 2026 2 1)) in
    let full_days = Float.of_int (Date.diff (d 2026 4 1) (d 2026 1 1)) in
    90.0 *. clipped_days /. full_days
  in
  Alcotest.(check (float 1e-9))
    "proportional repeated clip" expected_proportional proportional_total;
  Alcotest.(check (float 1e-9)) "const repeated clip" 5.0 const_total

let test_formula_tracks_cell_queries () =
  let open Series in
  let start = d 2026 1 1 in
  let period = p start (Date.shift (months 1) start) in
  let base = monthly_series ~start ~n:1 (fun _ -> 10.0) in
  let tracked_queries = ref [] in
  let tracked =
    Spans.Unfold
      {
        id = new_id ();
        label = None;
        init = ();
        deps = (fun () -> Deps.span_dep base);
        cells =
          (fun read_base () ->
            let formula = read_base ~period ~reduce:sum_floats in
            tracked_queries := Formula.queries formula;
            Some (cell ~period ~split:proportional_split formula, ()));
      }
  in
  let cache = make_cache () in
  let value = query_span cache tracked ~period ~reduce:sum_floats in
  Alcotest.(check (float 1e-9)) "tracked value" 10.0 value;
  Alcotest.(check int) "one cell-level query" 1 (List.length !tracked_queries);
  match !tracked_queries with
  | [ Formula.Span_query_item { period = tracked_period; _ } ] ->
      Alcotest.(check bool) "tracked period" true (Period.equal period tracked_period)
  | _ -> Alcotest.fail "expected one span query"

(* Dependency extraction: an Unfold's deps show up in [dependencies]. *)
let test_unfold_dependencies () =
  let open Series in
  let start = d 2026 1 1 in
  let base_a = monthly_series ~start ~n:1 (fun _ -> 1.0) in
  let base_b = monthly_series ~start ~n:1 (fun _ -> 2.0) in
  let u =
    Spans.Unfold
      {
        id = new_id ();
        label = None;
        init = ();
        deps =
          (fun () ->
            let open Deps in
            let+ a = span_dep base_a and+ b = span_dep base_b in
            (~a, ~b));
        cells = (fun (~a:_, ~b:_) () -> None);
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

let test_query_rejects_reused_span_id () =
  let open Series in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let id = new_id () in
  let first = single_span_series_with_id ~id ~period ~split:proportional_split 1.0 in
  let second = single_span_series_with_id ~id ~period ~split:proportional_split 2.0 in
  let cache = make_cache () in
  ignore (query_span cache first ~period ~reduce:sum_floats);
  check_invalid_arg ~prefix:"series id " (fun () ->
      ignore (query_span cache second ~period ~reduce:sum_floats))

let test_query_rejects_reused_span_point_id () =
  let open Series in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let id = new_id () in
  let span = single_span_series_with_id ~id ~period ~split:proportional_split 1.0 in
  let point = single_point_series_with_id ~id ~period 2.0 in
  let cache = make_cache () in
  ignore (query_span cache span ~period ~reduce:sum_floats);
  check_invalid_arg ~prefix:"series id " (fun () ->
      ignore (query_point cache point ~date:(Period.start period) ~default:0.0))

let test_dependencies_reject_reused_id () =
  let open Series in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let dep_id = new_id () in
  let first = single_span_series_with_id ~id:dep_id ~period ~split:proportional_split 1.0 in
  let second = single_span_series_with_id ~id:dep_id ~period ~split:proportional_split 2.0 in
  let parent =
    Spans.Unfold
      {
        id = new_id ();
        label = None;
        init = ();
        deps =
          (fun () ->
            let open Deps in
            let+ first = span_dep first and+ second = span_dep second in
            (~first, ~second));
        cells = (fun _ () -> None);
      }
  in
  check_invalid_arg ~prefix:"series id " (fun () -> ignore (dependencies (Span_series parent)))

(* An Unfold that reads its own prior-period value (recursive). *)
let test_unfold_self_recursive () =
  let open Series in
  let start = d 2026 1 1 in
  let rec rev : Spans.t =
    Spans.Unfold
      {
        id = new_id ();
        label = None;
        init = 0;
        deps = (fun () -> Deps.span_dep rev);
        cells =
          (fun prev i ->
            if i >= 4 then None
            else
              let period = p (Date.shift (months i) start) (Date.shift (months (i + 1)) start) in
              let formula =
                if i = 0 then Formula.pure 100.0
                else
                  let open Formula in
                  let+ previous =
                    prev
                      ~period:(p (Date.shift (months (i - 1)) start) (Date.shift (months i) start))
                      ~reduce:sum_floats
                  in
                  previous *. 1.10
              in
              Some (cell ~period ~split:proportional_split formula, i + 1));
      }
  in
  let cache = make_cache () in
  let total =
    query_span cache rev ~period:(p start (Date.shift (months 4) start)) ~reduce:sum_floats
  in
  (* 100 + 110 + 121 + 133.1 = 464.1 *)
  Alcotest.(check (float 1e-9)) "geometric 10% growth, 4 months" 464.1 total

let test_unfold_self_future_reference () =
  let open Series in
  let start = d 2026 1 1 in
  let rec rev : Spans.t =
    Spans.Unfold
      {
        id = new_id ();
        label = None;
        init = 0;
        deps = (fun () -> Deps.span_dep rev);
        cells =
          (fun read_self i ->
            if i >= 3 then None
            else
              let period = p (Date.shift (months i) start) (Date.shift (months (i + 1)) start) in
              let formula =
                if i = 2 then Formula.pure 100.0
                else
                  let open Formula in
                  let+ future =
                    read_self
                      ~period:
                        (p (Date.shift (months (i + 1)) start) (Date.shift (months (i + 2)) start))
                      ~reduce:sum_floats
                  in
                  future *. 0.5
              in
              Some (cell ~period ~split:proportional_split formula, i + 1));
      }
  in
  let cache = make_cache () in
  let first_month =
    query_span cache rev ~period:(p start (Date.shift (months 1) start)) ~reduce:sum_floats
  in
  Alcotest.(check (float 1e-9)) "future-derived first month" 25.0 first_month

let test_unfold_self_current_converges () =
  let open Series in
  let start = d 2026 1 1 in
  let rec rev : Spans.t =
    Spans.Unfold
      {
        id = new_id ();
        label = None;
        init = ();
        deps = (fun () -> Deps.span_dep rev);
        cells =
          (fun read_self () ->
            let period = p start (Date.shift (months 1) start) in
            let formula =
              let open Formula in
              let+ current = read_self ~period ~reduce:sum_floats in
              100.0 +. (0.5 *. current)
            in
            Some (cell ~period ~split:proportional_split formula, ()));
      }
  in
  let cache = make_cache () in
  let total =
    query_span cache rev ~period:(p start (Date.shift (months 1) start)) ~reduce:sum_floats
  in
  Alcotest.(check (float 1e-5)) "fixed point" 200.0 total

let test_unfold_self_current_diverges () =
  let open Series in
  let start = d 2026 1 1 in
  let rec rev : Spans.t =
    Spans.Unfold
      {
        id = new_id ();
        label = None;
        init = ();
        deps = (fun () -> Deps.span_dep rev);
        cells =
          (fun read_self () ->
            let period = p start (Date.shift (months 1) start) in
            let formula =
              let open Formula in
              let+ current = read_self ~period ~reduce:sum_floats in
              current +. 1.0
            in
            Some (cell ~period ~split:proportional_split formula, ()));
      }
  in
  let cache = make_cache () in
  try
    ignore (query_span cache rev ~period:(p start (Date.shift (months 1) start)) ~reduce:sum_floats);
    Alcotest.fail "expected non-convergence"
  with Evaluation_did_not_converge { iterations; tolerance; max_delta } ->
    Alcotest.(check int) "iteration cap" 1000 iterations;
    Alcotest.(check (float 0.0)) "tolerance" 1e-6 tolerance;
    Alcotest.(check bool) "max delta remains above tolerance" true (max_delta > tolerance)

let test_point_span_feedback_converges () =
  let open Series in
  let start = d 2026 1 1 in
  let end_ = d 2026 2 1 in
  let period = p start end_ in
  let rec interest : Spans.t =
    Spans.Unfold
      {
        id = new_id ();
        label = None;
        init = false;
        deps = (fun () -> Deps.point_dep balance);
        cells =
          (fun read_balance emitted ->
            if emitted then None
            else
              let formula =
                let open Formula in
                let+ ending_balance = read_balance ~date:end_ ~default:0.0 in
                ending_balance *. 0.10
              in
              Some (cell ~period ~split:proportional_split formula, true));
      }
  and balance : Points.t =
    Points.Accum { id = new_id (); label = None; init = 100.0; changes = interest }
  in
  let cache = make_cache () in
  let ending_balance = query_point cache balance ~date:end_ ~default:0.0 in
  Alcotest.(check (float 1e-5)) "feedback ending balance" (100.0 /. 0.9) ending_balance

let tests =
  List.map
    (fun (name, f) -> Alcotest.test_case name `Quick f)
    [
      ("span const", test_span_const);
      ("span map applies function", test_span_map_applies_function);
      ("span map2 applies function", test_span_map2_applies_function);
      ("span convenience constructors", test_span_convenience_constructors);
      ("span convenience fill", test_span_convenience_fill);
      ("sum_float_opts", test_sum_float_opts);
      ( "non-cyclic map preserves gaps and evaluates once",
        test_non_cyclic_map_preserves_gaps_and_evaluates_once );
      ("point const/map/map2", test_point_const_map_map2);
      ("point convenience constructors", test_point_convenience_constructors);
      ("point convenience fill", test_point_convenience_fill);
      ("point accum uses span changes", test_point_accum_uses_span_changes);
      ("label accessors", test_label_accessors);
      ("unfold no deps single step", test_unfold_no_deps_single_step);
      ("unfold multi step", test_unfold_multi_step);
      ("unfold with span dep", test_unfold_with_span_dep);
      ("repeated clip of split span", test_repeated_clip_of_split_span);
      ("formula tracks cell queries", test_formula_tracks_cell_queries);
      ("unfold dependencies extraction", test_unfold_dependencies);
      ("query rejects reused span id", test_query_rejects_reused_span_id);
      ("query rejects reused span/point id", test_query_rejects_reused_span_point_id);
      ("dependencies reject reused id", test_dependencies_reject_reused_id);
      ("unfold self-recursive", test_unfold_self_recursive);
      ("unfold self future reference", test_unfold_self_future_reference);
      ("unfold self current converges", test_unfold_self_current_converges);
      ("unfold self current diverges", test_unfold_self_current_diverges);
      ("point/span feedback converges", test_point_span_feedback_converges);
    ]
