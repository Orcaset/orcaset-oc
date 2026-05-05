open Orcaset

let d y m day = Date.make y m day
let p s e = Period.make s e
let agg = Series.Agg.sum
let days n = Offset.make ~days:n ()
let months n = Offset.make ~months:n ()
let option_float = Alcotest.option (Alcotest.float 1e-9)
let check_some_float name expected actual = Alcotest.check option_float name (Some expected) actual
let check_none_float name actual = Alcotest.check option_float name None actual

let span_values cache series ~period =
  Series.query_span_samples cache series ~period
  |> List.map (Option.map (fun (sample : Series.Agg.sample) -> sample.value))

(* Build a span series of [n] consecutive monthly cells starting at [start], with
   value [value_of i] for cell [i]. Useful for tests. *)
let monthly_series ~start ~n value_of =
  Series.Spans.unfold ~agg ~init:0
    ~deps:(fun () -> Series.Deps.none)
    ~cells:(fun () i ->
      if i >= n then None
      else
        let period = p (Date.shift (months i) start) (Date.shift (months (i + 1)) start) in
        Some
          ( Series.Spans.cell ~period ~split:Split.daily (Series.Formula.pure (Some (value_of i))),
            i + 1 ))
    ()

let single_span_series ~period ~split value = Series.Spans.of_list ~split ~agg [ (period, value) ]

let test_span_const () =
  let open Series in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let series = Spans.const ~split:Split.daily ~agg ~period 42.0 in
  let cache = make_cache () in
  let total = query_span cache series ~period in
  check_some_float "const span value" 42.0 total

let test_span_of_list () =
  let open Series in
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let feb = p (d 2026 2 1) (d 2026 3 1) in
  let series =
    Spans.of_list ~label:"Revenue" ~split:Split.daily ~agg [ (jan, 31.0); (feb, 28.0) ]
  in
  let cache = make_cache () in
  Alcotest.(check (option string)) "label" (Some "Revenue") (Spans.label series);
  check_some_float "jan" 31.0 (query_span cache series ~period:jan);
  check_some_float "feb" 28.0 (query_span cache series ~period:feb);
  check_some_float "partial proportional" 17.0
    (query_span cache series ~period:(p (d 2026 1 15) (d 2026 2 1)))

let test_span_custom_split () =
  let open Series in
  let full_period = p (d 2026 1 1) (d 2026 4 1) in
  let first_month = p (d 2026 1 1) (d 2026 2 1) in
  let remaining_months = p (d 2026 2 1) (d 2026 4 1) in
  let front_loaded_split : Split.t =
   fun ~period:_ ~date:_ ->
    (Split.part ~value:(fun value -> value *. 0.75), Split.part ~value:(fun value -> value *. 0.25))
  in
  let series = single_span_series ~period:full_period ~split:front_loaded_split 100.0 in
  let cache = make_cache () in
  check_some_float "custom split left side" 75.0 (query_span cache series ~period:first_month);
  check_some_float "custom split right side" 25.0 (query_span cache series ~period:remaining_months)

let test_span_map_applies_function () =
  let open Series in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let base = single_span_series ~period ~split:Split.daily 10.0 in
  let mapped = Spans.map (fun value -> value *. -0.9) base in
  let cache = make_cache () in
  let total = query_span cache mapped ~period in
  check_some_float "mapped span value" (-9.0) total

let test_span_map2_applies_function () =
  let open Series in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let a = single_span_series ~period ~split:Split.daily 10.0 in
  let b = single_span_series ~period ~split:Split.daily 3.0 in
  let mapped =
    Spans.map2 ~agg a b (fun a b ->
        Some (Option.value ~default:0.0 a -. Option.value ~default:0.0 b))
  in
  let cache = make_cache () in
  let total = query_span cache mapped ~period in
  check_some_float "map2 span value" 7.0 total

let test_span_map2_splits_parents_before_mapping () =
  let open Series in
  let full_period = p (d 2026 1 1) (d 2026 4 1) in
  let middle_month = p (d 2026 2 1) (d 2026 3 1) in
  let a = single_span_series ~period:full_period ~split:Split.const 10.0 in
  let b = single_span_series ~period:full_period ~split:Split.const 2.0 in
  let mapped =
    Spans.map2 ~agg a b (fun a b ->
        Some (Option.value ~default:0.0 a *. Option.value ~default:0.0 b))
  in
  let cache = make_cache () in
  let total = query_span cache mapped ~period:middle_month in
  check_some_float "map2 split parent values" 20.0 total

let test_span_convenience_constructors () =
  let open Series in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let a = single_span_series ~period ~split:Split.daily 10.0 in
  let b = single_span_series ~period ~split:Split.daily 2.0 in
  let cache = make_cache () in
  let check name expected series =
    check_some_float name expected (query_span cache series ~period)
  in
  let total = Spans.sum ~label:"Total" ~agg [ a; b ] in
  Alcotest.(check (option string)) "label" (Some "Total") (Spans.label total);
  let mapped = Spans.map ~label:"Doubled" (fun value -> value *. 2.0) a in
  Alcotest.(check (option string)) "map label" (Some "Doubled") (Spans.label mapped);
  check "map" 20.0 mapped;
  check "neg" (-10.0) (Spans.neg a);
  check "scale" 30.0 (Spans.scale 3.0 a);
  check "sum" 12.0 total;
  check "sub" 8.0 (Spans.sub ~agg a b);
  check "mul" 20.0 (Spans.mul ~agg [ a; b ]);
  check "div" 5.0 (Spans.div ~agg a b)

let test_span_convenience_identity () =
  let open Series in
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let feb = p (d 2026 2 1) (d 2026 3 1) in
  let query_period = p (d 2026 1 1) (d 2026 3 1) in
  let a = single_span_series ~period:jan ~split:Split.daily 10.0 in
  let b = single_span_series ~period:feb ~split:Split.daily 3.0 in
  let cache = make_cache () in
  let summed = Spans.sum ~agg [ a; b ] in
  let total = query_span cache summed ~period:query_period in
  check_some_float "sum uses 0 as additive identity for missing sides" 13.0 total;
  let multiplied = Spans.mul ~agg [ a; b ] in
  (* mul over disjoint periods is undefined because each aligned row has a missing side. *)
  let total = query_span cache multiplied ~period:query_period in
  check_none_float "mul requires every input" total

let test_span_mapn_fill () =
  let open Series in
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let feb = p (d 2026 2 1) (d 2026 3 1) in
  let query_period = p (d 2026 1 1) (d 2026 3 1) in
  let a = single_span_series ~period:jan ~split:Split.daily 10.0 in
  let b = single_span_series ~period:feb ~split:Split.daily 3.0 in
  let filled =
    Spans.mapn ~agg [ a; b ] (fun values ->
        Some
          (List.fold_left
             (fun acc -> function Some v -> acc +. v | None -> acc +. 1.0)
             0.0 values))
  in
  let cache = make_cache () in
  let total = query_span cache filled ~period:query_period in
  check_some_float "mapn fills missing entries before reducing" 15.0 total

let test_span_mapn_splits_parents_before_mapping () =
  let open Series in
  let full_period = p (d 2026 1 1) (d 2026 4 1) in
  let middle_month = p (d 2026 2 1) (d 2026 3 1) in
  let a = single_span_series ~period:full_period ~split:Split.const 10.0 in
  let b = single_span_series ~period:full_period ~split:Split.const 2.0 in
  let c = single_span_series ~period:full_period ~split:Split.const 3.0 in
  let mapped =
    Spans.mapn ~agg [ a; b; c ] (fun values ->
        Some
          (List.fold_left
             (fun acc -> function Some value -> acc *. value | None -> acc)
             1.0 values))
  in
  let cache = make_cache () in
  let total = query_span cache mapped ~period:middle_month in
  check_some_float "mapn split parent values" 60.0 total

let test_span_sum_n_way_mismatched_periods () =
  let open Series in
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let feb = p (d 2026 2 1) (d 2026 3 1) in
  let mar = p (d 2026 3 1) (d 2026 4 1) in
  let query_period = p (d 2026 1 1) (d 2026 4 1) in
  (* a: jan, feb. b: feb, mar. c: jan, mar. *)
  let a = Spans.of_list ~split:Split.daily ~agg [ (jan, 10.0); (feb, 20.0) ] in
  let b = Spans.of_list ~split:Split.daily ~agg [ (feb, 200.0); (mar, 300.0) ] in
  let c = Spans.of_list ~split:Split.daily ~agg [ (jan, 1.0); (mar, 2.0) ] in
  let total = Spans.sum ~agg [ a; b; c ] in
  let cache = make_cache () in
  let cells = span_values cache total ~period:query_period in
  match cells with
  | [ Some jan_v; Some feb_v; Some mar_v ] ->
      (* jan: 10 + 0 + 1 = 11. feb: 20 + 200 + 0 = 220. mar: 0 + 300 + 2 = 302. *)
      Alcotest.(check (float 1e-9)) "jan sum" 11.0 jan_v;
      Alcotest.(check (float 1e-9)) "feb sum" 220.0 feb_v;
      Alcotest.(check (float 1e-9)) "mar sum" 302.0 mar_v
  | _ -> Alcotest.fail "expected three aligned cells"

let test_span_mul_n_way_requires_all_inputs () =
  let open Series in
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let feb = p (d 2026 2 1) (d 2026 3 1) in
  let mar = p (d 2026 3 1) (d 2026 4 1) in
  let query_period = p (d 2026 1 1) (d 2026 4 1) in
  (* a: jan=2, feb=3. b: feb=4, mar=5. c: jan=10, mar=11. *)
  let a = Spans.of_list ~split:Split.daily ~agg [ (jan, 2.0); (feb, 3.0) ] in
  let b = Spans.of_list ~split:Split.daily ~agg [ (feb, 4.0); (mar, 5.0) ] in
  let c = Spans.of_list ~split:Split.daily ~agg [ (jan, 10.0); (mar, 11.0) ] in
  let total = Spans.mul ~agg [ a; b; c ] in
  let cache = make_cache () in
  let cells = span_values cache total ~period:query_period in
  match cells with
  | [ None; None; None ] -> ()
  | _ -> Alcotest.fail "expected undefined aligned cells"

let test_span_sum_empty_raises () =
  Alcotest.check_raises "empty sum raises" (Invalid_argument "Spans.sum: empty list") (fun () ->
      ignore (Series.Spans.sum ~agg []));
  Alcotest.check_raises "empty mul raises" (Invalid_argument "Spans.mul: empty list") (fun () ->
      ignore (Series.Spans.mul ~agg []))

let test_span_extend_contiguous () =
  let open Series in
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let feb = p (d 2026 2 1) (d 2026 3 1) in
  let a = single_span_series ~period:jan ~split:Split.daily 31.0 in
  let b = single_span_series ~period:feb ~split:Split.daily 28.0 in
  let extended = Spans.extend ~agg a b in
  let cache = make_cache () in
  match span_values cache extended ~period:(p (d 2026 1 1) (d 2026 3 1)) with
  | [ Some jan_value; Some feb_value ] ->
      Alcotest.(check (float 1e-9)) "jan" 31.0 jan_value;
      Alcotest.(check (float 1e-9)) "feb" 28.0 feb_value
  | _ -> Alcotest.fail "expected contiguous extended spans"

let test_span_extend_clips_overlapping_second_series () =
  let open Series in
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let overlap = p (d 2026 1 15) (d 2026 2 15) in
  let query_period = p (d 2026 2 1) (d 2026 2 15) in
  let a = single_span_series ~period:jan ~split:Split.daily 31.0 in
  let b = single_span_series ~period:overlap ~split:Split.daily 31.0 in
  let extended = Spans.extend ~agg a b in
  let cache = make_cache () in
  let total = query_span cache extended ~period:query_period in
  check_some_float "clipped second span" 14.0 total

let test_span_extend_skips_covered_second_spans () =
  let open Series in
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let feb = p (d 2026 2 1) (d 2026 3 1) in
  let mar = p (d 2026 3 1) (d 2026 4 1) in
  let jan_to_mar = p (d 2026 1 1) (d 2026 3 1) in
  let a = single_span_series ~period:jan_to_mar ~split:Split.const 60.0 in
  let b = Spans.of_list ~split:Split.const ~agg [ (jan, 10.0); (feb, 20.0); (mar, 30.0) ] in
  let extended = Spans.extend ~agg a b in
  let cache = make_cache () in
  let total = query_span cache extended ~period:(p (d 2026 1 1) (d 2026 4 1)) in
  check_some_float "covered second spans skipped" 90.0 total

let test_span_extend_preserves_gap_before_second_series () =
  let open Series in
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let mar = p (d 2026 3 1) (d 2026 4 1) in
  let a = single_span_series ~period:jan ~split:Split.daily 31.0 in
  let b = single_span_series ~period:mar ~split:Split.daily 31.0 in
  let extended = Spans.extend ~agg a b in
  let cache = make_cache () in
  match span_values cache extended ~period:(p (d 2026 2 1) (d 2026 4 1)) with
  | [ None; Some mar_value ] -> Alcotest.(check (float 1e-9)) "mar" 31.0 mar_value
  | _ -> Alcotest.fail "expected gap before second series"

let test_span_extend_empty_first_series () =
  let open Series in
  let feb = p (d 2026 2 1) (d 2026 3 1) in
  let empty = Spans.of_list ~split:Split.daily ~agg [] in
  let b = single_span_series ~period:feb ~split:Split.daily 28.0 in
  let extended = Spans.extend ~agg empty b in
  let cache = make_cache () in
  let total = query_span cache extended ~period:feb in
  check_some_float "empty first series" 28.0 total

let test_span_clipped_bounds () =
  let open Series in
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let feb = p (d 2026 2 1) (d 2026 3 1) in
  let mar = p (d 2026 3 1) (d 2026 4 1) in
  let base = Spans.of_list ~split:Split.daily ~agg [ (jan, 31.0); (feb, 28.0); (mar, 31.0) ] in
  let clipped = Spans.clipped ~after:(d 2026 1 15) ~until:(d 2026 3 15) base in
  let cache = make_cache () in
  match span_values cache clipped ~period:(p (d 2026 1 1) (d 2026 4 1)) with
  | [ None; Some jan_value; Some feb_value; Some mar_value; None ] ->
      Alcotest.(check (float 1e-9)) "leading partial" 17.0 jan_value;
      Alcotest.(check (float 1e-9)) "contained span" 28.0 feb_value;
      Alcotest.(check (float 1e-9)) "trailing partial" 14.0 mar_value
  | _ -> Alcotest.fail "expected clipped spans with boundary gaps"

let test_span_clipped_one_sided_constructors () =
  let open Series in
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let feb = p (d 2026 2 1) (d 2026 3 1) in
  let mar = p (d 2026 3 1) (d 2026 4 1) in
  let base = Spans.of_list ~split:Split.daily ~agg [ (jan, 31.0); (feb, 28.0); (mar, 31.0) ] in
  let cache = make_cache () in
  let from_mid_feb =
    span_values cache (Spans.after (d 2026 2 15) base) ~period:(p (d 2026 1 1) (d 2026 4 1))
  in
  let until_mid_feb =
    span_values cache (Spans.until (d 2026 2 15) base) ~period:(p (d 2026 1 1) (d 2026 4 1))
  in
  (match from_mid_feb with
  | [ None; Some feb_value; Some mar_value ] ->
      Alcotest.(check (float 1e-9)) "after partial" 14.0 feb_value;
      Alcotest.(check (float 1e-9)) "after following span" 31.0 mar_value
  | _ -> Alcotest.fail "expected one-sided after clip");
  match until_mid_feb with
  | [ Some jan_value; Some feb_value; None ] ->
      Alcotest.(check (float 1e-9)) "until preceding span" 31.0 jan_value;
      Alcotest.(check (float 1e-9)) "until partial" 14.0 feb_value
  | _ -> Alcotest.fail "expected one-sided until clip"

let test_span_clipped_zero_width_empty () =
  let open Series in
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let base = single_span_series ~period:jan ~split:Split.daily 31.0 in
  let clipped = Spans.clipped ~after:(d 2026 1 15) ~until:(d 2026 1 15) base in
  let cache = make_cache () in
  match span_values cache clipped ~period:jan with
  | [ None ] -> ()
  | _ -> Alcotest.fail "expected a gap for a zero-width clip over a non-empty query"

let test_span_clipped_reversed_bounds_raise () =
  let open Series in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let base = single_span_series ~period ~split:Split.daily 31.0 in
  Alcotest.check_raises "reversed bounds raise"
    (Invalid_argument "Spans.clipped: until before after") (fun () ->
      ignore (Spans.clipped ~after:(d 2026 2 1) ~until:(d 2026 1 1) base))

let test_span_clipped_outside_queries_do_not_force_base () =
  let open Series in
  let forced = ref false in
  let base =
    Spans.unfold ~agg ~init:()
      ~deps:(fun () -> Deps.none)
      ~cells:(fun () () ->
        forced := true;
        Alcotest.fail "base sequence should not be forced")
      ()
  in
  let clipped = Spans.clipped ~after:(d 2026 2 1) ~until:(d 2026 3 1) base in
  let cache = make_cache () in
  let before = span_values cache clipped ~period:(p (d 2026 1 1) (d 2026 2 1)) in
  let after = span_values cache clipped ~period:(p (d 2026 3 1) (d 2026 4 1)) in
  (match (before, after) with
  | [ None ], [ None ] -> ()
  | _ -> Alcotest.fail "expected outside queries to be gaps");
  Alcotest.(check bool) "base not forced" false !forced

let test_span_clipped_label_and_dependencies () =
  let open Series in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let base = Spans.const ~label:"Revenue" ~split:Split.daily ~agg ~period 31.0 in
  let clipped = Spans.clipped ~after:(d 2026 1 15) ~until:(d 2026 2 1) base in
  Alcotest.(check (option string)) "label" (Some "Revenue") (Spans.label clipped);
  match dependencies (Span_series clipped) with
  | [ { series = Series (Span_series dep); dependencies = []; is_back_edge = false } ] ->
      Alcotest.(check bool) "base dependency" true (dep == base)
  | _ -> Alcotest.fail "expected clipped series to depend on its base"

let test_unfold_from_replays_base_and_continues () =
  let open Series in
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let feb = p (d 2026 2 1) (d 2026 3 1) in
  let mar = p (d 2026 3 1) (d 2026 4 1) in
  let base = Spans.of_list ~split:Split.daily ~agg [ (jan, 31.0); (feb, 28.0) ] in
  let seen = ref [] in
  let series =
    Spans.unfold_from ~agg
      ~deps:(fun () -> Deps.none)
      ~cells:(fun () previous ->
        seen := previous :: !seen;
        let period = Period.next (months 1) previous in
        if Date.(Period.start period >= d 2026 5 1) then None
        else
          let value = Float.of_int (Date.month (Period.start period)) in
          Some (Spans.cell ~period ~split:Split.daily (Formula.pure (Some value)), period))
      base
  in
  let cache = make_cache () in
  (match span_values cache series ~period:(p (d 2026 1 1) (d 2026 5 1)) with
  | [ Some jan_value; Some feb_value; Some mar_value; Some apr_value ] ->
      Alcotest.(check (float 1e-9)) "jan from base" 31.0 jan_value;
      Alcotest.(check (float 1e-9)) "feb from base" 28.0 feb_value;
      Alcotest.(check (float 1e-9)) "mar from continuation" 3.0 mar_value;
      Alcotest.(check (float 1e-9)) "apr from continuation" 4.0 apr_value
  | _ -> Alcotest.fail "expected base spans followed by continuation spans");
  match List.rev !seen with
  | [ first; second ] ->
      Alcotest.(check bool)
        "first continuation receives last base period" true (Period.equal feb first);
      Alcotest.(check bool) "next period controls continuation state" true (Period.equal mar second)
  | _ -> Alcotest.fail "expected two continuation calls"

let test_unfold_from_empty_base_does_not_call_cells () =
  let open Series in
  let empty = Spans.of_list ~split:Split.daily ~agg [] in
  let called = ref false in
  let series =
    Spans.unfold_from ~agg
      ~deps:(fun () -> Deps.none)
      ~cells:(fun () _ ->
        called := true;
        Alcotest.fail "cells should not be called for an empty base")
      empty
  in
  let cache = make_cache () in
  let total = query_span cache series ~period:(p (d 2026 1 1) (d 2026 2 1)) in
  check_none_float "empty result" total;
  Alcotest.(check bool) "cells not called" false !called

let test_unfold_from_dependencies () =
  let open Series in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let base = single_span_series ~period ~split:Split.daily 1.0 in
  let points = Points.of_list [ (d 2026 2 1, 2.0) ] in
  let series =
    Spans.unfold_from ~agg ~deps:(fun () -> Deps.point_dep points) ~cells:(fun _ _ -> None) base
  in
  let deps = dependencies (Span_series series) in
  Alcotest.(check int) "base plus explicit dep" 2 (List.length deps);
  let span_deps, point_deps =
    List.fold_left
      (fun (span_count, point_count) dep ->
        let (Series s) = dep.series in
        match s with
        | Span_series _ -> (span_count + 1, point_count)
        | Point_series _ -> (span_count, point_count + 1))
      (0, 0) deps
  in
  Alcotest.(check int) "span deps" 1 span_deps;
  Alcotest.(check int) "point deps" 1 point_deps

let agg_sample period value = Some { Series.Agg.period; value }

let test_agg_helpers () =
  let open Series in
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let feb = p (d 2026 2 1) (d 2026 3 1) in
  let samples = [ agg_sample jan 10.0; None; agg_sample feb 30.0 ] in
  let act_360_average = Agg.time_weighted_average Yf.act_360 in
  let custom_average =
    Agg.time_weighted_average (fun start _ -> if Date.month start = 1 then 1.0 else 3.0)
  in
  check_some_float "sum ignores missing samples" 40.0 (Agg.reduce Agg.sum samples);
  check_some_float "min" 10.0 (Agg.reduce Agg.min samples);
  check_some_float "max" 30.0 (Agg.reduce Agg.max samples);
  check_some_float "average" 20.0 (Agg.reduce Agg.average samples);
  check_some_float "time-weighted average"
    (((10.0 *. 31.0) +. (30.0 *. 28.0)) /. 59.0)
    (Agg.reduce act_360_average samples);
  check_some_float "time-weighted average uses year fraction" 25.0
    (Agg.reduce custom_average samples);
  List.iter
    (fun builtin ->
      check_none_float "empty aggregation" (Agg.reduce builtin []);
      check_none_float "all-missing aggregation" (Agg.reduce builtin [ None; None ]))
    [ Agg.sum; Agg.min; Agg.max; Agg.average; act_360_average ]

let test_span_aggregation_defaults_and_samples () =
  let open Series in
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let feb = p (d 2026 2 1) (d 2026 3 1) in
  let mar = p (d 2026 3 1) (d 2026 4 1) in
  let time_weighted_average = Agg.time_weighted_average Yf.act_360 in
  let users =
    Spans.of_list ~split:Split.const ~agg:time_weighted_average [ (jan, 100.0); (feb, 200.0) ]
  in
  let gappy = Spans.of_list ~split:Split.daily ~agg [ (jan, 10.0); (mar, 30.0) ] in
  let cache = make_cache () in
  check_some_float "default time-weighted average"
    (((100.0 *. 31.0) +. (200.0 *. 28.0)) /. 59.0)
    (query_span cache users ~period:(p (d 2026 1 1) (d 2026 3 1)));
  (match span_values cache gappy ~period:(p (d 2026 1 1) (d 2026 4 1)) with
  | [ Some jan_value; None; Some mar_value ] ->
      Alcotest.(check (float 1e-9)) "jan sample" 10.0 jan_value;
      Alcotest.(check (float 1e-9)) "mar sample" 30.0 mar_value
  | _ -> Alcotest.fail "expected samples with an internal gap");
  let avg = Spans.with_agg ~agg:Agg.average gappy in
  check_some_float "with_agg replaces default aggregation" 20.0
    (query_span cache avg ~period:(p (d 2026 1 1) (d 2026 4 1)))

let test_span_query_missing_period_returns_none () =
  let open Series in
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let feb = p (d 2026 2 1) (d 2026 3 1) in
  let series = Spans.const ~split:Split.daily ~agg ~period:jan 10.0 in
  let cache = make_cache () in
  check_none_float "missing query aggregates to none" (query_span cache series ~period:feb);
  match span_values cache series ~period:feb with
  | [ None ] -> ()
  | _ -> Alcotest.fail "expected one uncovered gap"

let test_span_unary_and_clipped_inherit_agg () =
  let open Series in
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let feb = p (d 2026 2 1) (d 2026 3 1) in
  let base = Spans.of_list ~split:Split.const ~agg:Agg.average [ (jan, 10.0); (feb, 20.0) ] in
  let cache = make_cache () in
  check_some_float "scale inherits average aggregation" 30.0
    (query_span cache (Spans.scale 2.0 base) ~period:(p (d 2026 1 1) (d 2026 3 1)));
  check_some_float "clipped inherits average aggregation" 20.0
    (query_span cache
       (Spans.clipped ~after:(d 2026 2 1) ~until:(d 2026 3 1) base)
       ~period:(p (d 2026 1 1) (d 2026 3 1)))

let test_non_cyclic_map_preserves_gaps () =
  let open Series in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let base = Spans.const ~split:Split.daily ~agg ~period 10.0 in
  let mapped = Spans.map (fun value -> value *. 2.0) base in
  let cache = make_cache () in
  let query_period = p (d 2025 12 1) (d 2026 3 1) in
  let values = span_values cache mapped ~period:query_period in
  match values with
  | [ None; Some value; None ] -> Alcotest.(check (float 1e-9)) "mapped value" 20.0 value
  | _ -> Alcotest.fail "expected leading and trailing gaps"

let test_point_const_map_map2 () =
  let open Series in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let date = d 2026 1 15 in
  let a = Points.const ~period 10.0 in
  let b = Points.const ~period 3.0 in
  let mapped = Points.map (fun value -> value *. 2.0) a in
  let mapped2 =
    Points.map2 a b (fun a b -> Some (Option.value ~default:0.0 a +. Option.value ~default:0.0 b))
  in
  let cache = make_cache () in
  check_some_float "const point" 10.0 (query_point cache a ~date);
  check_some_float "mapped point" 20.0 (query_point cache mapped ~date);
  check_some_float "map2 point" 13.0 (query_point cache mapped2 ~date)

let test_point_of_list () =
  let open Series in
  let jan = d 2026 1 1 in
  let feb = d 2026 2 1 in
  let series = Points.of_list ~label:"Balance" [ (jan, 100.0); (feb, 125.0) ] in
  let cache = make_cache () in
  Alcotest.(check (option string)) "label" (Some "Balance") (Points.label series);
  check_some_float "jan" 100.0 (query_point cache series ~date:jan);
  check_some_float "feb" 125.0 (query_point cache series ~date:feb);
  check_none_float "missing point" (query_point cache series ~date:(d 2026 3 1))

let test_point_convenience_constructors () =
  let open Series in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let date = d 2026 1 15 in
  let a = Points.const ~period 10.0 in
  let b = Points.const ~period 2.0 in
  let cache = make_cache () in
  let check name expected series =
    check_some_float name expected (query_point cache series ~date)
  in
  let total = Points.sum ~label:"Total" [ a; b ] in
  Alcotest.(check (option string)) "label" (Some "Total") (Points.label total);
  let mapped = Points.map ~label:"Doubled" (fun value -> value *. 2.0) a in
  Alcotest.(check (option string)) "map label" (Some "Doubled") (Points.label mapped);
  check "map" 20.0 mapped;
  check "neg" (-10.0) (Points.neg a);
  check "scale" 30.0 (Points.scale 3.0 a);
  check "sum" 12.0 total;
  check "sub" 8.0 (Points.sub a b);
  check "mul" 20.0 (Points.mul [ a; b ]);
  check "div" 5.0 (Points.div a b)

let test_point_convenience_identity () =
  let open Series in
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let feb = p (d 2026 2 1) (d 2026 3 1) in
  let a = Points.const ~period:jan 10.0 in
  let b = Points.const ~period:feb 3.0 in
  let cache = make_cache () in
  let summed = Points.sum [ a; b ] in
  check_some_float "jan sum additive identity" 10.0 (query_point cache summed ~date:(d 2026 1 15));
  let multiplied = Points.mul [ a; b ] in
  check_none_float "feb mul requires every input" (query_point cache multiplied ~date:(d 2026 2 15))

let test_point_mapn_fill () =
  let open Series in
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let feb = p (d 2026 2 1) (d 2026 3 1) in
  let a = Points.const ~period:jan 10.0 in
  let b = Points.const ~period:feb 3.0 in
  let filled =
    Points.mapn [ a; b ] (fun values ->
        Some
          (List.fold_left
             (fun acc -> function Some v -> acc +. v | None -> acc +. 1.0)
             0.0 values))
  in
  let cache = make_cache () in
  check_some_float "mapn fills missing sides before reducing" 11.0
    (query_point cache filled ~date:(d 2026 1 15))

let test_point_sum_n_way () =
  let open Series in
  let period_all = Period.unbounded in
  let date = d 2026 1 15 in
  let x = Points.const ~period:period_all 2.0 in
  let y = Points.const ~period:period_all 3.0 in
  let z = Points.const ~period:period_all 4.0 in
  let cache = make_cache () in
  check_some_float "n-way sum" 9.0 (query_point cache (Points.sum [ x; y; z ]) ~date);
  check_some_float "n-way mul" 24.0 (query_point cache (Points.mul [ x; y; z ]) ~date)

let test_point_sum_empty_raises () =
  Alcotest.check_raises "empty point sum raises" (Invalid_argument "Points.sum: empty list")
    (fun () -> ignore (Series.Points.sum []));
  Alcotest.check_raises "empty point mul raises" (Invalid_argument "Points.mul: empty list")
    (fun () -> ignore (Series.Points.mul []))

let test_point_accum_uses_span_changes () =
  let open Series in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let changes = single_span_series ~period ~split:Split.daily 10.0 in
  let balance = Points.accum ~init:100.0 changes in
  let cache = make_cache () in
  check_some_float "initial balance" 100.0 (query_point cache balance ~date:(Period.start period));
  check_some_float "ending balance" 110.0 (query_point cache balance ~date:(Period.end_ period))

let test_label_accessors () =
  let open Series in
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let span = Spans.const ~label:"Revenue" ~split:Split.daily ~agg ~period 42.0 in
  let point = Points.const ~label:"Balance" ~period 10.0 in
  Alcotest.(check (option string)) "span label" (Some "Revenue") (Spans.label span);
  Alcotest.(check (option string)) "point label" (Some "Balance") (Points.label point);
  Alcotest.(check (option string)) "wrapped span label" (Some "Revenue") (label (Span_series span));
  Alcotest.(check (option string))
    "wrapped point label" (Some "Balance") (label (Point_series point))

(* A single-step Unfold with no deps. Emits one cell and terminates. *)
let test_unfold_no_deps_single_step () =
  let open Series in
  let series =
    Spans.unfold ~label:"Single step" ~agg ~init:0
      ~deps:(fun () -> Deps.none)
      ~cells:(fun () n ->
        if n >= 1 then None
        else
          let period = p (d 2026 1 1) (d 2026 2 1) in
          Some (Spans.cell ~period ~split:Split.daily (Formula.pure (Some 42.0)), n + 1))
      ()
  in
  let cache = make_cache () in
  Alcotest.(check (option string)) "label" (Some "Single step") (Spans.label series);
  let total = query_span cache series ~period:(p (d 2026 1 1) (d 2026 2 1)) in
  check_some_float "value" 42.0 total

(* Multi-step Unfold with termination. *)
let test_unfold_multi_step () =
  let open Series in
  let start = d 2026 1 1 in
  let series =
    Spans.unfold ~agg ~init:0
      ~deps:(fun () -> Deps.none)
      ~cells:(fun () n ->
        if n >= 3 then None
        else
          let period =
            p (Date.shift (days (30 * n)) start) (Date.shift (days (30 * (n + 1))) start)
          in
          Some (Spans.cell ~period ~split:Split.daily (Formula.pure (Some (Float.of_int n))), n + 1))
      ()
  in
  let cache = make_cache () in
  let total = query_span cache series ~period:(p start (Date.shift (days 90) start)) in
  check_some_float "sum 0+1+2" 3.0 total

(* Unfold with a span dep: derive values by querying the dep. *)
let test_unfold_with_span_dep () =
  let open Series in
  let start = d 2026 1 1 in
  let base = monthly_series ~start ~n:3 (fun i -> 10.0 *. Float.of_int (i + 1)) in
  let doubled =
    Spans.unfold ~agg ~init:0
      ~deps:(fun () -> Deps.span_dep base)
      ~cells:(fun read_base n ->
        if n >= 3 then None
        else
          let period = p (Date.shift (months n) start) (Date.shift (months (n + 1)) start) in
          let formula =
            let open Formula in
            let+ v = read_base ~period in
            Option.map (fun v -> 2.0 *. v) v
          in
          Some (Spans.cell ~period ~split:Split.daily formula, n + 1))
      ()
  in
  let cache = make_cache () in
  let total = query_span cache doubled ~period:(p start (d 2026 4 1)) in
  check_some_float "sum 20+40+60" 120.0 total

let test_unfold_missing_span_dep_propagates_none () =
  let open Series in
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let feb = p (d 2026 2 1) (d 2026 3 1) in
  let base = Spans.const ~split:Split.daily ~agg ~period:jan 10.0 in
  let derived =
    Spans.unfold ~agg ~init:false
      ~deps:(fun () -> Deps.span_dep base)
      ~cells:(fun read_base emitted ->
        if emitted then None
        else
          let formula = read_base ~period:feb in
          Some (Spans.cell ~period:feb ~split:Split.daily formula, true))
      ()
  in
  let cache = make_cache () in
  check_none_float "missing dependency propagates" (query_span cache derived ~period:feb)

let test_repeated_clip_of_split_span () =
  let open Series in
  let full_period = p (d 2026 1 1) (d 2026 4 1) in
  let clipped_period = p (d 2026 2 1) (d 2026 3 1) in
  let proportional = single_span_series ~period:full_period ~split:Split.daily 90.0 in
  let const = single_span_series ~period:full_period ~split:Split.const 5.0 in
  let cache = make_cache () in
  let proportional_total = query_span cache proportional ~period:clipped_period in
  let const_total = query_span cache const ~period:clipped_period in
  let expected_proportional =
    let clipped_days = Float.of_int (Date.diff (d 2026 3 1) (d 2026 2 1)) in
    let full_days = Float.of_int (Date.diff (d 2026 4 1) (d 2026 1 1)) in
    90.0 *. clipped_days /. full_days
  in
  check_some_float "proportional repeated clip" expected_proportional proportional_total;
  check_some_float "const repeated clip" 5.0 const_total

let test_formula_tracks_cell_queries () =
  let open Series in
  let start = d 2026 1 1 in
  let period = p start (Date.shift (months 1) start) in
  let base = monthly_series ~start ~n:1 (fun _ -> 10.0) in
  let tracked_queries = ref [] in
  let tracked =
    Spans.unfold ~agg ~init:()
      ~deps:(fun () -> Deps.span_dep base)
      ~cells:(fun read_base () ->
        let formula = read_base ~period in
        tracked_queries := Formula.queries formula;
        Some (Spans.cell ~period ~split:Split.daily formula, ()))
      ()
  in
  let cache = make_cache () in
  let value = query_span cache tracked ~period in
  check_some_float "tracked value" 10.0 value;
  Alcotest.(check int) "one cell-level query" 1 (List.length !tracked_queries);
  match !tracked_queries with
  | [ Formula.Span_query_item { period = tracked_period; _ } ] ->
      Alcotest.(check bool) "tracked period" true (Period.equal period tracked_period)
  | _ -> Alcotest.fail "expected one span query"

let test_formula_span_dep_uses_series_agg () =
  let open Series in
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let feb = p (d 2026 2 1) (d 2026 3 1) in
  let query_period = p (d 2026 1 1) (d 2026 3 1) in
  let time_weighted_average = Agg.time_weighted_average Yf.act_360 in
  let base =
    Spans.of_list ~split:Split.const ~agg:time_weighted_average [ (jan, 100.0); (feb, 200.0) ]
  in
  let derived =
    Spans.unfold ~agg:Agg.sum ~init:false
      ~deps:(fun () -> Deps.span_dep base)
      ~cells:(fun read_base emitted ->
        if emitted then None
        else
          let formula = read_base ~period:query_period in
          Some (Spans.cell ~period:query_period ~split:Split.daily formula, true))
      ()
  in
  let cache = make_cache () in
  check_some_float "formula uses dependency aggregation"
    (((100.0 *. 31.0) +. (200.0 *. 28.0)) /. 59.0)
    (query_span cache derived ~period:query_period)

(* Dependency extraction: an Unfold's deps show up in [dependencies]. *)
let test_unfold_dependencies () =
  let open Series in
  let start = d 2026 1 1 in
  let base_a = monthly_series ~start ~n:1 (fun _ -> 1.0) in
  let base_b = monthly_series ~start ~n:1 (fun _ -> 2.0) in
  let u =
    Spans.unfold ~agg ~init:()
      ~deps:(fun () ->
        let open Deps in
        let+ a = span_dep base_a and+ b = span_dep base_b in
        (~a, ~b))
      ~cells:(fun (~a:_, ~b:_) () -> None)
      ()
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

(* An Unfold that reads its own prior-period value (recursive). *)
let test_unfold_self_recursive () =
  let open Series in
  let start = d 2026 1 1 in
  let rev =
    Spans.unfold_rec ~agg ~init:0
      ~deps:(fun self -> Deps.span_dep self)
      ~cells:(fun prev i ->
        if i >= 4 then None
        else
          let period = p (Date.shift (months i) start) (Date.shift (months (i + 1)) start) in
          let formula =
            if i = 0 then Formula.pure (Some 100.0)
            else
              let open Formula in
              let+ previous =
                prev ~period:(p (Date.shift (months (i - 1)) start) (Date.shift (months i) start))
              in
              Option.map (fun previous -> previous *. 1.10) previous
          in
          Some (Spans.cell ~period ~split:Split.daily formula, i + 1))
      ()
  in
  let cache = make_cache () in
  let total = query_span cache rev ~period:(p start (Date.shift (months 4) start)) in
  (* 100 + 110 + 121 + 133.1 = 464.1 *)
  check_some_float "geometric 10% growth, 4 months" 464.1 total

let test_unfold_self_future_reference () =
  let open Series in
  let start = d 2026 1 1 in
  let rev =
    Spans.unfold_rec ~agg ~init:0
      ~deps:(fun self -> Deps.span_dep self)
      ~cells:(fun read_self i ->
        if i >= 3 then None
        else
          let period = p (Date.shift (months i) start) (Date.shift (months (i + 1)) start) in
          let formula =
            if i = 2 then Formula.pure (Some 100.0)
            else
              let open Formula in
              let+ future =
                read_self
                  ~period:
                    (p (Date.shift (months (i + 1)) start) (Date.shift (months (i + 2)) start))
              in
              Option.map (fun future -> future *. 0.5) future
          in
          Some (Spans.cell ~period ~split:Split.daily formula, i + 1))
      ()
  in
  let cache = make_cache () in
  let first_month = query_span cache rev ~period:(p start (Date.shift (months 1) start)) in
  check_some_float "future-derived first month" 25.0 first_month

let test_unfold_self_current_converges () =
  let open Series in
  let start = d 2026 1 1 in
  let rev =
    Spans.unfold_rec ~agg ~init:()
      ~deps:(fun self -> Deps.span_dep self)
      ~cells:(fun read_self () ->
        let period = p start (Date.shift (months 1) start) in
        let formula =
          let open Formula in
          let+ current = read_self ~period in
          Option.map (fun current -> 100.0 +. (0.5 *. current)) current
        in
        Some (Spans.cell ~period ~split:Split.daily formula, ()))
      ()
  in
  let cache = make_cache () in
  let total = query_span cache rev ~period:(p start (Date.shift (months 1) start)) in
  Alcotest.check (Alcotest.option (Alcotest.float 1e-5)) "fixed point" (Some 200.0) total

let test_unfold_self_current_diverges () =
  let open Series in
  let start = d 2026 1 1 in
  let rev =
    Spans.unfold_rec ~agg ~init:()
      ~deps:(fun self -> Deps.span_dep self)
      ~cells:(fun read_self () ->
        let period = p start (Date.shift (months 1) start) in
        let formula =
          let open Formula in
          let+ current = read_self ~period in
          Option.map (fun current -> current +. 1.0) current
        in
        Some (Spans.cell ~period ~split:Split.daily formula, ()))
      ()
  in
  let cache = make_cache () in
  try
    ignore (query_span cache rev ~period:(p start (Date.shift (months 1) start)));
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
  let rec interest_lazy =
    lazy
      (Spans.unfold ~agg ~init:false
         ~deps:(fun () -> Deps.point_dep (Lazy.force balance_lazy))
         ~cells:(fun read_balance emitted ->
           if emitted then None
           else
             let formula =
               let open Formula in
               let+ ending_balance = read_balance ~date:end_ in
               Option.map (fun ending_balance -> ending_balance *. 0.10) ending_balance
             in
             Some (Spans.cell ~period ~split:Split.daily formula, true))
         ())
  and balance_lazy = lazy (Points.accum ~init:100.0 (Lazy.force interest_lazy)) in
  let balance = Lazy.force balance_lazy in
  let cache = make_cache () in
  let ending_balance = query_point cache balance ~date:end_ in
  Alcotest.check
    (Alcotest.option (Alcotest.float 1e-5))
    "feedback ending balance"
    (Some (100.0 /. 0.9))
    ending_balance

let tests =
  List.map
    (fun (name, f) -> Alcotest.test_case name `Quick f)
    [
      ("span const", test_span_const);
      ("span of_list", test_span_of_list);
      ("span custom split", test_span_custom_split);
      ("span map applies function", test_span_map_applies_function);
      ("span map2 applies function", test_span_map2_applies_function);
      ("span map2 splits parents before mapping", test_span_map2_splits_parents_before_mapping);
      ("span convenience constructors", test_span_convenience_constructors);
      ("span sum identity and mul missingness", test_span_convenience_identity);
      ("span mapn fill", test_span_mapn_fill);
      ("span mapn splits parents before mapping", test_span_mapn_splits_parents_before_mapping);
      ("span sum n-way mismatched periods", test_span_sum_n_way_mismatched_periods);
      ("span mul n-way requires all inputs", test_span_mul_n_way_requires_all_inputs);
      ("span sum/mul empty list raises", test_span_sum_empty_raises);
      ("span extend contiguous", test_span_extend_contiguous);
      ( "span extend clips overlapping second series",
        test_span_extend_clips_overlapping_second_series );
      ("span extend skips covered second spans", test_span_extend_skips_covered_second_spans);
      ( "span extend preserves gap before second series",
        test_span_extend_preserves_gap_before_second_series );
      ("span extend empty first series", test_span_extend_empty_first_series);
      ("span clipped bounds", test_span_clipped_bounds);
      ("span clipped one-sided constructors", test_span_clipped_one_sided_constructors);
      ("span clipped zero-width empty", test_span_clipped_zero_width_empty);
      ("span clipped reversed bounds raise", test_span_clipped_reversed_bounds_raise);
      ( "span clipped outside queries do not force base",
        test_span_clipped_outside_queries_do_not_force_base );
      ("span clipped label and dependencies", test_span_clipped_label_and_dependencies);
      ("unfold_from replays base and continues", test_unfold_from_replays_base_and_continues);
      ("unfold_from empty base does not call cells", test_unfold_from_empty_base_does_not_call_cells);
      ("unfold_from dependencies", test_unfold_from_dependencies);
      ("aggregation helpers", test_agg_helpers);
      ("span aggregation defaults and samples", test_span_aggregation_defaults_and_samples);
      ("span query missing period returns none", test_span_query_missing_period_returns_none);
      ("span unary and clipped inherit aggregation", test_span_unary_and_clipped_inherit_agg);
      ("non-cyclic map preserves gaps", test_non_cyclic_map_preserves_gaps);
      ("point const/map/map2", test_point_const_map_map2);
      ("point of_list", test_point_of_list);
      ("point convenience constructors", test_point_convenience_constructors);
      ("point sum identity and mul missingness", test_point_convenience_identity);
      ("point mapn fill", test_point_mapn_fill);
      ("point sum/mul n-way", test_point_sum_n_way);
      ("point sum/mul empty list raises", test_point_sum_empty_raises);
      ("point accum uses span changes", test_point_accum_uses_span_changes);
      ("label accessors", test_label_accessors);
      ("unfold no deps single step", test_unfold_no_deps_single_step);
      ("unfold multi step", test_unfold_multi_step);
      ("unfold with span dep", test_unfold_with_span_dep);
      ("unfold missing span dep propagates none", test_unfold_missing_span_dep_propagates_none);
      ("repeated clip of split span", test_repeated_clip_of_split_span);
      ("formula tracks cell queries", test_formula_tracks_cell_queries);
      ("formula span dependency uses series aggregation", test_formula_span_dep_uses_series_agg);
      ("unfold dependencies extraction", test_unfold_dependencies);
      ("unfold self-recursive", test_unfold_self_recursive);
      ("unfold self future reference", test_unfold_self_future_reference);
      ("unfold self current converges", test_unfold_self_current_converges);
      ("unfold self current diverges", test_unfold_self_current_diverges);
      ("point/span feedback converges", test_point_span_feedback_converges);
    ]
