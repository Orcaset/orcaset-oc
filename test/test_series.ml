open Orcaset
open Series.Stmt

let check_int = Alcotest.(check int)
let check_string = Alcotest.(check string)
let check_bool = Alcotest.(check bool)
let check_float = Alcotest.(check (float 1e-9))

let contains_substring haystack needle =
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  let rec loop idx =
    if needle_len = 0 then true
    else if idx + needle_len > haystack_len then false
    else if String.sub haystack idx needle_len = needle then true
    else loop (idx + 1)
  in
  loop 0

let point_value = function
  | Series.Point { point = Some (_, Amount value); _ } -> value
  | Series.Point { point = None; _ } -> Alcotest.fail "expected point value"
  | Series.Period _ -> Alcotest.fail "expected point result"

let make_fixture () =
  let offset = Offset.make ~months:3 ~month_end:true () in
  let p1 = Period.make (Date.make 2025 12 31) (Date.make 2026 3 31) in
  let p2 = Period.next offset p1 in
  let revenue =
    Series.Period.unfold_seq_self ~label:"Revenue" ~cells:(fun () ->
      (List.to_seq
         [
           Series.Period.const ~period:p1 (fun () -> 100.0);
           Series.Period.const ~period:p2 (fun () -> 120.0);
         ]))
  in
  let expenses = Series.Period.map ~label:"Expenses" (fun revenue -> revenue *. -0.6) (lazy revenue) in
  let income = Series.Period.sum ~label:"Income" [ lazy revenue; lazy expenses ] in
  let stmt = period_total income [ period_line revenue; period_line expenses ] in
  (stmt, [ p1; p2 ])

let test_snapshot_structure () =
  let stmt, periods = make_fixture () in
  let snapshot : snapshot = Series.Stmt.snapshot stmt periods in
  check_int "snapshot version" 1 snapshot.version;
  check_int "column count" 3 (List.length snapshot.columns);
  check_int "row count" 3 (List.length snapshot.rows);
  check_bool "cell count is nonzero" true (snapshot.cells <> []);
  let first_column, second_column =
    match snapshot.columns with
    | first :: second :: _ -> (first, second)
    | _ -> Alcotest.fail "expected three columns"
  in
  check_string "first column id" "c0" first_column.id;
  check_bool "first column is start anchor" true
    (match first_column.role with Start_anchor -> true | Period_end -> false);
  check_bool "second column is period end" true
    (match second_column.role with Start_anchor -> false | Period_end -> true);
  match snapshot.rows with
  | revenue_row :: expenses_row :: income_row :: [] ->
      check_string "revenue row id" "r0" revenue_row.id;
      check_string "expenses row id" "r1" expenses_row.id;
      check_string "income row id" "r" income_row.id;
      check_bool "children point to total" true
        (revenue_row.parent_id = Some income_row.id && expenses_row.parent_id = Some income_row.id);
      check_bool "income row is total" true
        (match income_row.kind with Row_total -> true | Row_line -> false);
      check_int "row slot count matches columns" (List.length snapshot.columns)
        (List.length revenue_row.slots);
      (match revenue_row.slots with
      | first_slot :: second_slot :: _ ->
          check_bool "leading period slot is empty" true
            (match first_slot.kind with Slot_empty -> true | _ -> false);
          check_bool "leading period slot has no value" true (Option.is_none first_slot.value);
          check_bool "period slot has period metadata" true
            (match second_slot.kind with Slot_period _ -> true | _ -> false)
      | _ -> Alcotest.fail "expected row slots");
      check_bool "total row has drilldown cells" true
        (match List.nth income_row.slots 1 with
        | { cell_ids; _ } -> cell_ids <> []);
      check_bool "at least one cell has dependencies" true
        (List.exists (fun cell -> cell.dep_ids <> []) snapshot.cells);
      let referenced_cell_ids =
        snapshot.rows
        |> List.concat_map (fun row -> row.slots)
        |> List.concat_map (fun slot -> slot.cell_ids)
      in
      check_bool "all referenced cells exist" true
        (List.for_all
           (fun id -> List.exists (fun (cell : cell) -> String.equal cell.id id) snapshot.cells)
           referenced_cell_ids)
  | _ -> Alcotest.fail "expected three rows"

let test_snapshot_many_reuses_cells () =
  let stmt, periods = make_fixture () in
  let single : snapshot = Series.Stmt.snapshot stmt periods in
  let model : model_snapshot = Series.Stmt.snapshot_many [ ("income", stmt); ("duplicate", stmt) ] periods in
  check_int "statement count" 2 (List.length model.statements);
  check_int "shared cell registry size" (List.length single.cells) (List.length model.cells);
  match model.statements with
  | (first : statement_snapshot) :: (second : statement_snapshot) :: _ ->
      check_bool "first statement rows are namespaced" true
        (List.for_all
           (fun (row : row) -> String.starts_with ~prefix:"income:" row.id)
           first.rows);
      check_bool "second statement rows are namespaced" true
        (List.for_all
           (fun (row : row) -> String.starts_with ~prefix:"duplicate:" row.id)
           second.rows)
  | _ -> Alcotest.fail "expected two statements"

let test_snapshot_to_json_contains_core_fields () =
  let stmt, periods = make_fixture () in
  let snapshot = Series.Stmt.snapshot stmt periods in
  let json = Series.Stmt.snapshot_to_json snapshot in
  check_bool "has version field" true (String.starts_with ~prefix:"{\"version\":1" json);
  check_bool "has columns field" true (contains_substring json "\"columns\":[");
  check_bool "has rows field" true (contains_substring json "\"rows\":[");
  check_bool "has cells field" true (contains_substring json "\"cells\":[");
  check_bool "has row label" true (contains_substring json "\"label\":\"Revenue\"");
  check_bool "has cell id" true (contains_substring json "\"id\":\"period:")

let test_point_query_many_preserves_input_order () =
  let p1 = Period.make (Date.make 2025 1 1) (Date.make 2025 2 1) in
  let p2 = Period.make (Date.make 2025 2 1) (Date.make 2025 3 1) in
  let p3 = Period.make (Date.make 2025 3 1) (Date.make 2025 4 1) in
  let series =
    Series.Point.unfold_seq_self ~label:"Ordered"
      ~cells:(fun () ->
        List.to_seq
          [
            Series.Point.const_cell ~period:p1 (fun () -> 10.0);
            Series.Point.const_cell ~period:p2 (fun () -> 20.0);
            Series.Point.const_cell ~period:p3 (fun () -> 30.0);
          ])
  in
  let results =
    Series.Point.query_many
      [ Date.make 2025 4 1; Date.make 2025 2 1; Date.make 2025 3 1 ]
      series
    |> Series.eval_many
  in
  match results with
  | [ r1; r2; r3 ] ->
      check_float "first result keeps caller order" 60.0 (point_value r1);
      check_float "second result keeps caller order" 10.0 (point_value r2);
      check_float "third result keeps caller order" 30.0 (point_value r3)
  | _ -> Alcotest.fail "expected three point results"

let test_point_unfold_clipped_totals_and_gaps () =
  let p1 = Period.make (Date.make 2025 1 1) (Date.make 2025 1 11) in
  let p2 = Period.make (Date.make 2025 1 21) (Date.make 2025 1 31) in
  let series =
    Series.Point.unfold_seq_self ~label:"Clipped"
      ~cells:(fun () ->
        List.to_seq
          [
            Series.Point.const_cell ~period:p1 (fun () -> 10.0);
            Series.Point.const_cell ~period:p2 (fun () -> 20.0);
          ])
  in
  let results =
    Series.Point.query_many
      [
        Date.make 2025 2 5;
        Date.make 2025 1 5;
        Date.make 2025 1 26;
        Date.make 2025 1 15;
      ]
      series
    |> Series.eval_many
  in
  match results with
  | [ r1; r2; r3; r4 ] ->
      check_float "after final period carries total" 30.0 (point_value r1);
      check_float "partial first period is clipped" 4.0 (point_value r2);
      check_float "partial second period adds delta only" 20.0 (point_value r3);
      check_float "gap carries prior total" 10.0 (point_value r4)
  | _ -> Alcotest.fail "expected four point results"

let test_point_unfold_self_reference_still_accumulates () =
  let p1 = Period.make (Date.make 2025 1 1) (Date.make 2025 2 1) in
  let p2 = Period.make (Date.make 2025 2 1) (Date.make 2025 3 1) in
  let p3 = Period.make (Date.make 2025 3 1) (Date.make 2025 4 1) in
  let series =
    Series.Point.unfold_seq_self ~label:"Recursive"
      ~cells:(fun () ->
        List.to_seq
          [
            Series.Point.step ~period:p1
              (Series.Point.Query.self_or ~default:0.0 ~date:(Period.start_date p1))
              (fun prior -> prior +. 1.0);
            Series.Point.step ~period:p2
              (Series.Point.Query.self_or ~default:0.0 ~date:(Period.start_date p2))
              (fun prior -> prior +. 1.0);
            Series.Point.step ~period:p3
              (Series.Point.Query.self_or ~default:0.0 ~date:(Period.start_date p3))
              (fun prior -> prior +. 1.0);
          ])
  in
  let results =
    Series.Point.query_many
      [ Date.make 2025 4 1; Date.make 2025 2 1; Date.make 2025 3 1 ]
      series
    |> Series.eval_many
  in
  match results with
  | [ r1; r2; r3 ] ->
      check_float "third period sees accumulated history" 7.0 (point_value r1);
      check_float "first period starts from zero" 1.0 (point_value r2);
      check_float "second period sees prior total" 3.0 (point_value r3)
  | _ -> Alcotest.fail "expected three point results"

let tests =
  [
    Alcotest.test_case "snapshot structure" `Quick test_snapshot_structure;
    Alcotest.test_case "snapshot many reuses cells" `Quick test_snapshot_many_reuses_cells;
    Alcotest.test_case "snapshot to json contains core fields" `Quick
      test_snapshot_to_json_contains_core_fields;
    Alcotest.test_case "point query_many preserves caller order" `Quick
      test_point_query_many_preserves_input_order;
    Alcotest.test_case "point unfold clips and carries totals" `Quick
      test_point_unfold_clipped_totals_and_gaps;
    Alcotest.test_case "point unfold self-reference still accumulates" `Quick
      test_point_unfold_self_reference_still_accumulates;
  ]
