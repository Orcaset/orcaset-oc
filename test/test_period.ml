open Orcaset

let date = Helpers.date
let period = Helpers.period
let check_date name expected actual = Alcotest.(check date) name expected actual
let check_period name expected actual = Alcotest.(check period) name expected actual
let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)

let test_make_and_accessors () =
  let start_date = Date.make 2025 1 1 in
  let end_date = Date.make 2025 2 1 in
  let p = Period.make start_date end_date in
  check_date "start_date" start_date (Period.start_date p);
  check_date "end_date" end_date (Period.end_date p);
  check_int "days" 31 (Period.days p)

let test_inverted_period () =
  let start_date = Date.make 2025 2 1 in
  let end_date = Date.make 2025 1 1 in
  let p = Period.make start_date end_date in
  check_date "start_date" start_date (Period.start_date p);
  check_date "end_date" end_date (Period.end_date p);
  check_int "days" (-31) (Period.days p)

let test_to_tuple () =
  let start_date = Date.make 2025 3 1 in
  let end_date = Date.make 2025 4 1 in
  let actual_start, actual_end = Period.to_tuple (Period.make start_date end_date) in
  check_date "tuple start" start_date actual_start;
  check_date "tuple end" end_date actual_end

let test_contains_is_start_inclusive_end_exclusive () =
  let p = Period.make (Date.make 2025 1 1) (Date.make 2025 2 1) in
  check_bool "contains start" true (Period.contains (Date.make 2025 1 1) p);
  check_bool "contains interior" true (Period.contains (Date.make 2025 1 31) p);
  check_bool "excludes end" false (Period.contains (Date.make 2025 2 1) p);
  check_bool "excludes before" false (Period.contains (Date.make 2024 12 31) p);
  check_bool "excludes after" false (Period.contains (Date.make 2025 2 2) p)

let test_shift () =
  let offset = Offset.make ~months:1 ~days:2 () in
  let p = Period.make (Date.make 2025 1 31) (Date.make 2025 2 28) in
  let expected = Period.make (Date.make 2025 3 2) (Date.make 2025 3 30) in
  let neg_offset = Offset.make ~months:(-1) ~days:(-2) () in
  let expected_neg = Period.make (Date.make 2024 12 29) (Date.make 2025 1 26) in
  check_period "shifted period" expected (Period.shift offset p);
  check_period "negatively shifted period" expected_neg (Period.shift neg_offset p)

let test_make_seq_produces_contiguous_periods () =
  let offset = Offset.make ~months:1 ~month_end:true () in
  let periods =
    Period.make_seq ~start_date:(Date.make 2025 1 31) ~offset |> Seq.take 3 |> List.of_seq
  in
  match periods with
  | [ p1; p2; p3 ] ->
      check_period "p1" (Period.make (Date.make 2025 1 31) (Date.make 2025 2 28)) p1;
      check_period "p2" (Period.make (Date.make 2025 2 28) (Date.make 2025 3 31)) p2;
      check_period "p3" (Period.make (Date.make 2025 3 31) (Date.make 2025 4 30)) p3;
      check_date "contiguous 1->2" (Period.end_date p1) (Period.start_date p2);
      check_date "contiguous 2->3" (Period.end_date p2) (Period.start_date p3)
  | _ -> failwith "expected three periods"

let test_equal_and_hash () =
  let p1 = Period.make (Date.make 2025 1 1) (Date.make 2025 2 1) in
  let p1_same = Period.make (Date.make 2025 1 1) (Date.make 2025 2 1) in
  let p2 = Period.make (Date.make 2025 1 1) (Date.make 2025 3 1) in
  let p3 = Period.make (Date.make 2025 2 1) (Date.make 2025 3 1) in
  check_bool "equal" true (Period.equal p1 p1_same);
  check_bool "not equal different end" false (Period.equal p1 p2);
  check_bool "not equal different start" false (Period.equal p1 p3);
  check_bool "hash equal" true (Int.equal (Period.hash p1) (Period.hash p1_same));
  check_bool "hash not equal different end" false (Int.equal (Period.hash p1) (Period.hash p2));
  check_bool "hash not equal different start" false (Int.equal (Period.hash p1) (Period.hash p3))

let test_to_string () =
  Alcotest.(check string)
    "string" "2025-01-01..2025-02-01"
    (Period.to_string (Period.make (Date.make 2025 1 1) (Date.make 2025 2 1)))

let test_pp () =
  let p = Period.make (Date.make 2025 1 1) (Date.make 2025 2 1) in
  let actual = Format.asprintf "%a" Period.pp p in
  Alcotest.(check string) "pp" "2025-01-01..2025-02-01" actual

let test_negative_length_period_is_allowed () =
  let p = Period.make (Date.make 2025 2 1) (Date.make 2025 1 1) in
  check_int "negative days" (-31) (Period.days p);
  check_bool "contains nothing in reversed range" false (Period.contains (Date.make 2025 1 15) p)

let tests =
  [
    Alcotest.test_case "make and accessors" `Quick test_make_and_accessors;
    Alcotest.test_case "to_tuple" `Quick test_to_tuple;
    Alcotest.test_case "contains semantics" `Quick test_contains_is_start_inclusive_end_exclusive;
    Alcotest.test_case "shift" `Quick test_shift;
    Alcotest.test_case "make_seq contiguous" `Quick test_make_seq_produces_contiguous_periods;
    Alcotest.test_case "equal and hash" `Quick test_equal_and_hash;
    Alcotest.test_case "to_string" `Quick test_to_string;
    Alcotest.test_case "pp" `Quick test_pp;
    Alcotest.test_case "negative length allowed" `Quick test_negative_length_period_is_allowed;
  ]
