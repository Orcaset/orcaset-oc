open Orcaset

let d y m day = Date.make y m day
let p s e = Period.make s e
let span_series ~label ~period value = Series.Spans.const ?label ~period value

let point_series ~label ~period value =
  Series.Points.const ?label ~period value

let test_eval_periods_preserves_labels () =
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let revenue = span_series ~label:(Some "Revenue") ~period 42.0 in
  let balance = point_series ~label:(Some "Balance") ~period 10.0 in
  let stmt = Stmt.group [ Stmt.span_line revenue; Stmt.point_total balance [] ] in
  match Stmt.eval_periods [ period ] stmt with
  | Stmt.RGroup
      [
        Stmt.RLine { label = line_label; values = Some (Stmt.Span_values _) };
        Stmt.RTotal { label = total_label; values = Some (Stmt.Point_values _); children = [] };
      ] ->
      Alcotest.(check (option string)) "line label" (Some "Revenue") line_label;
      Alcotest.(check (option string)) "total label" (Some "Balance") total_label
  | _ -> Alcotest.fail "expected labeled resolved period statement"

let test_eval_dates_preserves_label_without_values () =
  let period = p (d 2026 1 1) (d 2026 2 1) in
  let revenue = span_series ~label:(Some "Revenue") ~period 42.0 in
  match Stmt.eval_dates [ d 2026 1 1 ] (Stmt.span_line revenue) with
  | Stmt.RLine { label; values = None } ->
      Alcotest.(check (option string)) "span label" (Some "Revenue") label
  | _ -> Alcotest.fail "expected labeled unresolved date statement"

let test_eval_periods_point_values_include_shared_boundaries () =
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let feb = p (d 2026 2 1) (d 2026 3 1) in
  let mar = p (d 2026 3 1) (d 2026 4 1) in
  let balance = point_series ~label:(Some "Balance") ~period:(p (d 2026 1 1) (d 2026 4 2)) 100.0 in
  match Stmt.eval_periods [ jan; feb; mar ] (Stmt.point_line balance) with
  | Stmt.RLine { values = Some (Stmt.Point_values values); _ } ->
      Alcotest.(check (list (pair Helpers.date (float 1e-9))))
        "point values"
        [
          (d 2026 1 1, 100.0);
          (d 2026 2 1, 100.0);
          (d 2026 3 1, 100.0);
          (d 2026 4 1, 100.0);
        ]
        values
  | _ -> Alcotest.fail "expected point values for period statement"

let test_fixed_width_renders_totals_and_indented_children () =
  let jan = p (d 2026 1 1) (d 2026 2 1) in
  let feb = p (d 2026 2 1) (d 2026 3 1) in
  let resolved =
    Stmt.RTotal
      {
        children =
          [
            Stmt.RLine
              {
                label = Some "Revenue";
                values = Some (Stmt.Span_values [ (jan, Some 100.0); (feb, Some 110.0) ]);
              };
            Stmt.RLine
              {
                label = Some "Costs";
                values = Some (Stmt.Span_values [ (jan, Some (-30.0)); (feb, Some (-30.0)) ]);
              };
          ];
        label = Some "Gross Profit";
        values = Some (Stmt.Span_values [ (jan, Some 70.0); (feb, Some 80.0) ]);
      }
  in
  let row = Printf.sprintf "%-*s  %10s  %10s" 12 in
  let expected =
    String.concat "\n"
      [
        row "" "2026-02-01" "2026-03-01";
        row "  Revenue" "100.00" "110.00";
        row "  Costs" "-30.00" "-30.00";
        row "" "----------" "----------";
        row "Gross Profit" "70.00" "80.00";
        "";
      ]
  in
  Alcotest.(check string) "fixed width total" expected (Stmt.fixed_width resolved)

let test_fixed_width_renders_point_stub_unknown_labels_and_group_spacing () =
  let jan = d 2026 1 1 in
  let feb = d 2026 2 1 in
  let period = p jan feb in
  let resolved =
    Stmt.RGroup
      [
        Stmt.RLine
          { label = None; values = Some (Stmt.Point_values [ (jan, 1000.0); (feb, 1100.0) ]) };
        Stmt.RLine { label = Some ""; values = Some (Stmt.Span_values [ (period, Some 50.0) ]) };
        Stmt.RLine { label = Some "No values"; values = None };
      ]
  in
  let row = Printf.sprintf "%-*s  %10s  %10s" 9 in
  let expected =
    String.concat "\n"
      [
        row "" "2026-01-01" "2026-02-01";
        "";
        row "Unknown" "1000.00" "1100.00";
        row "Unknown" "" "50.00";
        row "No values" "" "";
        "";
      ]
  in
  Alcotest.(check string) "fixed width group" expected (Stmt.fixed_width resolved)

let tests =
  List.map
    (fun (name, f) -> Alcotest.test_case name `Quick f)
    [
      ("eval_periods preserves labels", test_eval_periods_preserves_labels);
      ("eval_dates preserves label without values", test_eval_dates_preserves_label_without_values);
      ( "eval_periods point values include shared boundaries",
        test_eval_periods_point_values_include_shared_boundaries );
      ( "fixed_width renders totals and indented children",
        test_fixed_width_renders_totals_and_indented_children );
      ( "fixed_width renders point stub unknown labels and group spacing",
        test_fixed_width_renders_point_stub_unknown_labels_and_group_spacing );
    ]
