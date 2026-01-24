let date y m d = CalendarLib.Date.make y m d

let print_test_info name start_date end_date expected actual =
  Printf.printf "\n%s:\n" name;
  Printf.printf "  Start: %s\n" (CalendarLib.Printer.Date.to_string start_date);
  Printf.printf "  End:   %s\n" (CalendarLib.Printer.Date.to_string end_date);
  Printf.printf "  Expected: %.15f\n" expected;
  Printf.printf "  Actual:   %.15f\n" actual

let actual_360_simple () =
  let start_date = date 2025 1 1 in
  let end_date = date 2025 2 1 in
  let expected = 31.0 /. 360.0 in
  let actual = Orcaset.Yf.actual_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "actual/360" expected actual

let actual_360_reversed () =
  let start_date = date 2025 2 1 in
  let end_date = date 2025 1 1 in
  let expected = -31.0 /. 360.0 in
  let actual = Orcaset.Yf.actual_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "actual/360 reversed" expected actual

let actual_360_apr30_may31 () =
  let start_date = date 2025 4 30 in
  let end_date = date 2025 5 31 in
  let expected = 31.0 /. 360.0 in
  let actual = Orcaset.Yf.actual_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "actual/360 4/30 to 5/31" expected actual

let actual_360_may31_jun30 () =
  let start_date = date 2025 5 31 in
  let end_date = date 2025 6 30 in
  let expected = 30.0 /. 360.0 in
  let actual = Orcaset.Yf.actual_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "actual/360 5/31 to 6/30" expected actual

let actual_360_may30_jun30 () =
  let start_date = date 2025 5 30 in
  let end_date = date 2025 6 30 in
  let expected = 31.0 /. 360.0 in
  let actual = Orcaset.Yf.actual_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "actual/360 5/30 to 6/30" expected actual

let actual_360_apr30_may30 () =
  let start_date = date 2025 4 30 in
  let end_date = date 2025 5 30 in
  let expected = 30.0 /. 360.0 in
  let actual = Orcaset.Yf.actual_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "actual/360 4/30 to 5/30" expected actual

let actual_360_apr15_may15 () =
  let start_date = date 2025 4 15 in
  let end_date = date 2025 5 15 in
  let expected = 30.0 /. 360.0 in
  let actual = Orcaset.Yf.actual_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "actual/360 4/15 to 5/15" expected actual

let actual_360_feb28_mar31 () =
  let start_date = date 2025 2 28 in
  let end_date = date 2025 3 31 in
  let expected = 31.0 /. 360.0 in
  let actual = Orcaset.Yf.actual_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "actual/360 2/28 to 3/31" expected actual

let actual_360_jan31_feb28 () =
  let start_date = date 2025 1 31 in
  let end_date = date 2025 2 28 in
  let expected = 28.0 /. 360.0 in
  let actual = Orcaset.Yf.actual_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "actual/360 1/31 to 2/28" expected actual

let actual_360_feb29_mar31_leap () =
  let start_date = date 2024 2 29 in
  let end_date = date 2024 3 31 in
  let expected = 31.0 /. 360.0 in
  let actual = Orcaset.Yf.actual_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "actual/360 2/29 to 3/31 (leap)" expected actual

let actual_360_jan31_feb29_leap () =
  let start_date = date 2024 1 31 in
  let end_date = date 2024 2 29 in
  let expected = 29.0 /. 360.0 in
  let actual = Orcaset.Yf.actual_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "actual/360 1/31 to 2/29 (leap)" expected actual

let actual_360_same_date () =
  let start_date = date 2025 3 15 in
  let end_date = date 2025 3 15 in
  let expected = 0.0 in
  let actual = Orcaset.Yf.actual_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "actual/360 same date" expected actual

(* 30 / 360 Tests *)

let thirty_360_same_month () =
  let start_date = date 2025 1 10 in
  let end_date = date 2025 1 20 in
  let expected = 10.0 /. 360.0 in
  let actual = Orcaset.Yf.thirty_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "30/360 same month" expected actual

let thirty_360_month_end_adjustment () =
  let start_date = date 2025 1 31 in
  let end_date = date 2025 2 28 in
  let expected = 28.0 /. 360.0 in
  let actual = Orcaset.Yf.thirty_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "30/360 month end" expected actual

let thirty_360_reversed () =
  let start_date = date 2025 3 15 in
  let end_date = date 2025 1 15 in
  let expected = -60.0 /. 360.0 in
  let actual = Orcaset.Yf.thirty_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "30/360 reversed" expected actual

let thirty_360_apr30_may31 () =
  let start_date = date 2025 4 30 in
  let end_date = date 2025 5 31 in
  let expected = 30.0 /. 360.0 in
  let actual = Orcaset.Yf.thirty_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "30/360 4/30 to 5/31" expected actual

let thirty_360_may31_jun30 () =
  let start_date = date 2025 5 31 in
  let end_date = date 2025 6 30 in
  let expected = 30.0 /. 360.0 in
  let actual = Orcaset.Yf.thirty_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "30/360 5/31 to 6/30" expected actual

let thirty_360_may30_jun30 () =
  let start_date = date 2025 5 30 in
  let end_date = date 2025 6 30 in
  let expected = 30.0 /. 360.0 in
  let actual = Orcaset.Yf.thirty_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "30/360 5/30 to 6/30" expected actual

let thirty_360_apr30_may30 () =
  let start_date = date 2025 4 30 in
  let end_date = date 2025 5 30 in
  let expected = 30.0 /. 360.0 in
  let actual = Orcaset.Yf.thirty_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "30/360 4/30 to 5/30" expected actual

let thirty_360_apr15_may15 () =
  let start_date = date 2025 4 15 in
  let end_date = date 2025 5 15 in
  let expected = 30.0 /. 360.0 in
  let actual = Orcaset.Yf.thirty_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "30/360 4/15 to 5/15" expected actual

let thirty_360_feb28_mar31 () =
  let start_date = date 2025 2 28 in
  let end_date = date 2025 3 31 in
  let expected = 31.0 /. 360.0 in
  let actual = Orcaset.Yf.thirty_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "30/360 2/28 to 3/31" expected actual

let thirty_360_jan31_feb28 () =
  let start_date = date 2025 1 31 in
  let end_date = date 2025 2 28 in
  let expected = 28.0 /. 360.0 in
  let actual = Orcaset.Yf.thirty_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "30/360 1/31 to 2/28" expected actual

let thirty_360_feb29_mar31_leap () =
  let start_date = date 2024 2 29 in
  let end_date = date 2024 3 31 in
  let expected = 31.0 /. 360.0 in
  let actual = Orcaset.Yf.thirty_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "30/360 2/29 to 3/31 (leap)" expected actual

let thirty_360_jan31_feb29_leap () =
  let start_date = date 2024 1 31 in
  let end_date = date 2024 2 29 in
  let expected = 29.0 /. 360.0 in
  let actual = Orcaset.Yf.thirty_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "30/360 1/31 to 2/29 (leap)" expected actual

let thirty_360_same_date () =
  let start_date = date 2025 3 15 in
  let end_date = date 2025 3 15 in
  let expected = 0.0 in
  let actual = Orcaset.Yf.thirty_360 start_date end_date in
  Alcotest.(check (float 1e-9)) "30/360 same date" expected actual

(* CMonthly Tests *)
let cmonthly_full_month () =
  let start_date = date 2025 1 1 in
  let end_date = date 2025 2 1 in
  let expected = (1.0 /. 12.0) +. (((30.0 /. 31.0) -. (27.0 /. 28.0)) /. 12.0) in
  let actual = Orcaset.Yf.cmonthly start_date end_date in
  Alcotest.(check (float 1e-9)) "cmonthly full month" expected actual

let cmonthly_partial_month () =
  let start_date = date 2025 1 10 in
  let end_date = date 2025 1 20 in
  let expected = 10.0 /. 31.0 /. 12.0 in
  let actual = Orcaset.Yf.cmonthly start_date end_date in
  Alcotest.(check (float 1e-9)) "cmonthly partial" expected actual

let cmonthly_reversed () =
  let start_date = date 2025 2 1 in
  let end_date = date 2025 1 1 in
  let expected = -.((1.0 /. 12.0) +. (((30.0 /. 31.0) -. (27.0 /. 28.0)) /. 12.0)) in
  let actual = Orcaset.Yf.cmonthly start_date end_date in
  Alcotest.(check (float 1e-9)) "cmonthly reversed" expected actual

let cmonthly_apr30_may31 () =
  let start_date = date 2025 4 30 in
  let end_date = date 2025 5 31 in
  let expected = 1.0 /. 12.0 in
  let actual = Orcaset.Yf.cmonthly start_date end_date in
  Alcotest.(check (float 1e-9)) "cmonthly 4/30 to 5/31" expected actual

let cmonthly_may31_jun30 () =
  let start_date = date 2025 5 31 in
  let end_date = date 2025 6 30 in
  let expected = 1.0 /. 12.0 in
  let actual = Orcaset.Yf.cmonthly start_date end_date in
  Alcotest.(check (float 1e-9)) "cmonthly 5/31 to 6/30" expected actual

let cmonthly_may30_jun30 () =
  let start_date = date 2025 5 30 in
  let end_date = date 2025 6 30 in
  let expected = (1.0 /. 12.0) +. (((1.0 /. 31.0) -. (0.0 /. 30.0)) /. 12.0) in
  let actual = Orcaset.Yf.cmonthly start_date end_date in
  Alcotest.(check (float 1e-9)) "cmonthly 5/30 to 6/30" expected actual

let cmonthly_apr30_may30 () =
  let start_date = date 2025 4 30 in
  let end_date = date 2025 5 30 in
  let expected = (1.0 /. 12.0) +. (((0.0 /. 30.0) -. (1.0 /. 31.0)) /. 12.0) in
  let actual = Orcaset.Yf.cmonthly start_date end_date in
  Alcotest.(check (float 1e-9)) "cmonthly 4/30 to 5/30" expected actual

let cmonthly_apr15_may15 () =
  let start_date = date 2025 4 15 in
  let end_date = date 2025 5 15 in
  let expected = (1.0 /. 12.0) +. (((15.0 /. 30.0) -. (16.0 /. 31.0)) /. 12.0) in
  let actual = Orcaset.Yf.cmonthly start_date end_date in
  Alcotest.(check (float 1e-9)) "cmonthly 4/15 to 5/15" expected actual

let cmonthly_feb28_mar31 () =
  let start_date = date 2025 2 28 in
  let end_date = date 2025 3 31 in
  let expected = (1.0 /. 12.0) +. (((0.0 /. 28.0) -. (0.0 /. 31.0)) /. 12.0) in
  let actual = Orcaset.Yf.cmonthly start_date end_date in
  Alcotest.(check (float 1e-9)) "cmonthly 2/28 to 3/31" expected actual

let cmonthly_jan31_feb28 () =
  let start_date = date 2025 1 31 in
  let end_date = date 2025 2 28 in
  let expected = (1.0 /. 12.0) +. (((0.0 /. 31.0) -. (0.0 /. 28.0)) /. 12.0) in
  let actual = Orcaset.Yf.cmonthly start_date end_date in
  Alcotest.(check (float 1e-9)) "cmonthly 1/31 to 2/28" expected actual

let cmonthly_feb29_mar31_leap () =
  let start_date = date 2024 2 29 in
  let end_date = date 2024 3 31 in
  let expected = (1.0 /. 12.0) +. (((0.0 /. 29.0) -. (0.0 /. 31.0)) /. 12.0) in
  let actual = Orcaset.Yf.cmonthly start_date end_date in
  Alcotest.(check (float 1e-9)) "cmonthly 2/29 to 3/31 (leap)" expected actual

let cmonthly_jan31_feb29_leap () =
  let start_date = date 2024 1 31 in
  let end_date = date 2024 2 29 in
  let expected = (1.0 /. 12.0) +. (((0.0 /. 31.0) -. (0.0 /. 29.0)) /. 12.0) in
  let actual = Orcaset.Yf.cmonthly start_date end_date in
  Alcotest.(check (float 1e-9)) "cmonthly 1/31 to 2/29 (leap)" expected actual

let cmonthly_same_date () =
  let start_date = date 2025 3 15 in
  let end_date = date 2025 3 15 in
  let expected = 0.0 in
  let actual = Orcaset.Yf.cmonthly start_date end_date in
  Alcotest.(check (float 1e-9)) "cmonthly same date" expected actual

let suite =
  [
    ( "actual_360",
      [
        Alcotest.test_case "simple" `Quick actual_360_simple;
        Alcotest.test_case "reversed" `Quick actual_360_reversed;
        Alcotest.test_case "4/30 to 5/31" `Quick actual_360_apr30_may31;
        Alcotest.test_case "5/31 to 6/30" `Quick actual_360_may31_jun30;
        Alcotest.test_case "5/30 to 6/30" `Quick actual_360_may30_jun30;
        Alcotest.test_case "4/30 to 5/30" `Quick actual_360_apr30_may30;
        Alcotest.test_case "4/15 to 5/15" `Quick actual_360_apr15_may15;
        Alcotest.test_case "2/28 to 3/31" `Quick actual_360_feb28_mar31;
        Alcotest.test_case "1/31 to 2/28" `Quick actual_360_jan31_feb28;
        Alcotest.test_case "2/29 to 3/31 (leap)" `Quick actual_360_feb29_mar31_leap;
        Alcotest.test_case "1/31 to 2/29 (leap)" `Quick actual_360_jan31_feb29_leap;
        Alcotest.test_case "same date" `Quick actual_360_same_date;
      ] );
    ( "thirty_360",
      [
        Alcotest.test_case "same_month" `Quick thirty_360_same_month;
        Alcotest.test_case "month_end" `Quick thirty_360_month_end_adjustment;
        Alcotest.test_case "reversed" `Quick thirty_360_reversed;
        Alcotest.test_case "4/30 to 5/31" `Quick thirty_360_apr30_may31;
        Alcotest.test_case "5/31 to 6/30" `Quick thirty_360_may31_jun30;
        Alcotest.test_case "5/30 to 6/30" `Quick thirty_360_may30_jun30;
        Alcotest.test_case "4/30 to 5/30" `Quick thirty_360_apr30_may30;
        Alcotest.test_case "4/15 to 5/15" `Quick thirty_360_apr15_may15;
        Alcotest.test_case "2/28 to 3/31" `Quick thirty_360_feb28_mar31;
        Alcotest.test_case "1/31 to 2/28" `Quick thirty_360_jan31_feb28;
        Alcotest.test_case "2/29 to 3/31 (leap)" `Quick thirty_360_feb29_mar31_leap;
        Alcotest.test_case "1/31 to 2/29 (leap)" `Quick thirty_360_jan31_feb29_leap;
        Alcotest.test_case "same date" `Quick thirty_360_same_date;
      ] );
    ( "cmonthly",
      [
        Alcotest.test_case "full_month" `Quick cmonthly_full_month;
        Alcotest.test_case "partial" `Quick cmonthly_partial_month;
        Alcotest.test_case "reversed" `Quick cmonthly_reversed;
        Alcotest.test_case "4/30 to 5/31" `Quick cmonthly_apr30_may31;
        Alcotest.test_case "5/31 to 6/30" `Quick cmonthly_may31_jun30;
        Alcotest.test_case "5/30 to 6/30" `Quick cmonthly_may30_jun30;
        Alcotest.test_case "4/30 to 5/30" `Quick cmonthly_apr30_may30;
        Alcotest.test_case "4/15 to 5/15" `Quick cmonthly_apr15_may15;
        Alcotest.test_case "2/28 to 3/31" `Quick cmonthly_feb28_mar31;
        Alcotest.test_case "1/31 to 2/28" `Quick cmonthly_jan31_feb28;
        Alcotest.test_case "2/29 to 3/31 (leap)" `Quick cmonthly_feb29_mar31_leap;
        Alcotest.test_case "1/31 to 2/29 (leap)" `Quick cmonthly_jan31_feb29_leap;
        Alcotest.test_case "same date" `Quick cmonthly_same_date;
      ] );
  ]
