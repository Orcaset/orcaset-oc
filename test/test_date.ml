open Orcaset

let date = Helpers.date
let check_date name expected actual = Alcotest.(check date) name expected actual
let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)

let test_make_and_accessors () =
  let d = Date.make 2026 3 30 in
  check_int "year" 2026 (Date.year d);
  check_int "month" 3 (Date.month d);
  check_int "day" 30 (Date.day d)

let test_make_rejects_invalid_month () =
  Alcotest.check_raises "invalid month" (Invalid_argument "Date.make: invalid month 0") (fun () ->
      ignore (Date.make 2026 0 1));
  Alcotest.check_raises "invalid month" (Invalid_argument "Date.make: invalid month 13") (fun () ->
      ignore (Date.make 2026 13 1))

let test_make_rejects_invalid_day () =
  Alcotest.check_raises "invalid day" (Invalid_argument "Date.make: invalid day 0 for 2025-02")
    (fun () -> ignore (Date.make 2025 2 0));
  Alcotest.check_raises "invalid day" (Invalid_argument "Date.make: invalid day 29 for 2025-02")
    (fun () -> ignore (Date.make 2025 2 29))

let test_is_leap_year () =
  check_bool "2025 not leap" false (Date.is_leap_year 2025);
  check_bool "2024 leap" true (Date.is_leap_year 2024);
  (* Divisible by 100 but not 400 *)
  check_bool "1900 not leap" false (Date.is_leap_year 1900);
  check_bool "2100 not leap" false (Date.is_leap_year 2100);
  (* Divisible by 400 *)
  check_bool "2000 leap" true (Date.is_leap_year 2000);
  check_bool "1600 leap" true (Date.is_leap_year 1600);
  (* Ordinary leap years *)
  check_bool "2028 leap" true (Date.is_leap_year 2028);
  check_bool "2023 not leap" false (Date.is_leap_year 2023)

let test_days_in_month () =
  check_int "jan" 31 (Date.days_in_month (Date.make 2025 1 1));
  check_int "feb leap" 29 (Date.days_in_month (Date.make 2024 2 1));
  check_int "feb non-leap" 28 (Date.days_in_month (Date.make 2025 2 1));
  check_int "mar" 31 (Date.days_in_month (Date.make 2025 3 1));
  check_int "apr" 30 (Date.days_in_month (Date.make 2025 4 1));
  check_int "may" 31 (Date.days_in_month (Date.make 2025 5 1));
  check_int "jun" 30 (Date.days_in_month (Date.make 2025 6 1));
  check_int "jul" 31 (Date.days_in_month (Date.make 2025 7 1));
  check_int "aug" 31 (Date.days_in_month (Date.make 2025 8 1));
  check_int "sep" 30 (Date.days_in_month (Date.make 2025 9 1));
  check_int "oct" 31 (Date.days_in_month (Date.make 2025 10 1));
  check_int "nov" 30 (Date.days_in_month (Date.make 2025 11 1));
  check_int "dec" 31 (Date.days_in_month (Date.make 2025 12 1))

let test_diff () =
  let start = Date.make 2025 12 31 in
  let after = Date.make 2026 1 10 in
  check_int "forward diff" 10 (Date.diff after start);
  check_int "backward diff" (-10) (Date.diff start after);
  check_int "same day" 0 (Date.diff start start)

let test_add_days () =
  let start = Date.make 2025 12 31 in
  check_date "add 10 days" (Date.make 2026 1 10) (Date.add_days 10 start);
  check_date "subtract 10 days" (Date.make 2025 12 21) (Date.add_days (-10) start);
  check_date "add 0 days" start (Date.add_days 0 start)

let test_shift_applies_months_then_days () =
  let offset = Offset.make ~months:1 ~days:1 () in
  let start = Date.make 2025 1 31 in
  check_date "shift" (Date.make 2025 3 1) (Date.shift offset start)

let test_shift_month_end () =
  let offset = Offset.make ~months:1 ~month_end:true () in
  let neg_offset = Offset.make ~months:(-1) ~month_end:true () in
  let start = Date.make 2025 1 15 in
  check_date "month end" (Date.make 2025 2 28) (Date.shift offset start);
  check_date "month end negative" (Date.make 2025 1 31)
    (Date.shift neg_offset (Date.make 2025 2 15))

let test_weekday () =
  check_int "monday" 1 (Date.weekday (Date.make 2025 12 29));
  check_int "tuesday" 2 (Date.weekday (Date.make 2025 12 30));
  check_int "wednesday" 3 (Date.weekday (Date.make 2025 12 31));
  check_int "thursday" 4 (Date.weekday (Date.make 2026 1 1));
  check_int "friday" 5 (Date.weekday (Date.make 2026 1 2));
  check_int "saturday" 6 (Date.weekday (Date.make 2026 1 3));
  check_int "sunday" 7 (Date.weekday (Date.make 2025 12 28))

let test_min_bound_max_compare_equal () =
  let d1 = Date.make 2025 6 1 in
  let d2 = Date.make 2025 6 2 in
  check_date "lower bound" (Date.make 1 1 1) Date.lower_bound;
  check_bool "equal" true (Date.equal d1 (Date.make 2025 6 1));
  check_bool "not equal" false (Date.equal d1 d2);
  check_int "compare" (-1) (Date.compare d1 d2);
  check_int "compare equal" 0 (Date.compare d1 (Date.make 2025 6 1));
  check_int "compare reverse" 1 (Date.compare d2 d1);
  check_date "min" d1 (Date.min d1 d2);
  check_date "max" d2 (Date.max d1 d2);
  check_bool "less than" true (d1 < d2);
  check_bool "not less than" false (d2 < d1);
  check_bool "less than or equal" true (d1 <= d2);
  check_bool "not less than or equal" false (d2 <= d1);
  check_bool "greater than" true (d2 > d1);
  check_bool "not greater than" false (d1 > d2);
  check_bool "greater than or equal" true (d2 >= d1);
  check_bool "not greater than or equal" false (d1 >= d2);
  check_bool "less than equal" true (d1 <= d1);
  check_bool "not less than equal" false (d1 < d1);
  check_bool "greater than equal" true (d1 >= d1);
  check_bool "not greater than equal" false (d1 > d1)

let test_hash () =
  let d1 = Date.make 2025 6 1 in
  let d2 = Date.make 2025 6 1 in
  let d3 = Date.make 2025 6 2 in
  check_int "deterministic" (Date.hash d1) (Date.hash d1);
  check_int "equal dates equal hashes" (Date.hash d1) (Date.hash d2);
  check_bool "different dates different hashes" true (Date.hash d1 <> Date.hash d3)

let test_pp () =
  let buf = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buf in
  Date.pp fmt (Date.make 2025 3 9);
  Format.pp_print_flush fmt ();
  Alcotest.(check string) "pp" "2025-03-09" (Buffer.contents buf)

let test_to_string () =
  Alcotest.(check string) "string" "2025-03-09" (Date.to_string (Date.make 2025 3 9))

let tests =
  [
    Alcotest.test_case "make and accessors" `Quick test_make_and_accessors;
    Alcotest.test_case "reject invalid month" `Quick test_make_rejects_invalid_month;
    Alcotest.test_case "reject invalid day" `Quick test_make_rejects_invalid_day;
    Alcotest.test_case "leap year rules" `Quick test_is_leap_year;
    Alcotest.test_case "days in month" `Quick test_days_in_month;
    Alcotest.test_case "diff" `Quick test_diff;
    Alcotest.test_case "add_days" `Quick test_add_days;
    Alcotest.test_case "shift months then days" `Quick test_shift_applies_months_then_days;
    Alcotest.test_case "shift month_end" `Quick test_shift_month_end;
    Alcotest.test_case "weekday" `Quick test_weekday;
    Alcotest.test_case "compare lower bound min max" `Quick test_min_bound_max_compare_equal;
    Alcotest.test_case "hash" `Quick test_hash;
    Alcotest.test_case "pp" `Quick test_pp;
    Alcotest.test_case "to_string" `Quick test_to_string;
  ]
