let date y m d = CalendarLib.Date.make y m d

let period_testable =
  Alcotest.testable
    (fun fmt p ->
      Fmt.pf fmt "Period { start_date = %s; end_date = %s }"
        (CalendarLib.Printer.Date.sprint "%Y-%m-%d" p.Orcaset.Period.start_date)
        (CalendarLib.Printer.Date.sprint "%Y-%m-%d" p.Orcaset.Period.end_date))
    Orcaset.Period.equal

let date_testable =
  Alcotest.testable
    (fun fmt d -> Fmt.pf fmt "%s" (CalendarLib.Printer.Date.sprint "%Y-%m-%d" d))
    CalendarLib.Date.equal

(* 1. make *)

let make_correct_dates () =
  let p = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 2 1) in
  Alcotest.(check date_testable) "start_date" (date 2025 1 1) p.start_date;
  Alcotest.(check date_testable) "end_date" (date 2025 2 1) p.end_date

let make_zero_length () =
  let p = Orcaset.Period.make ~start_date:(date 2025 3 15) ~end_date:(date 2025 3 15) in
  Alcotest.(check date_testable) "start_date" (date 2025 3 15) p.start_date;
  Alcotest.(check date_testable) "end_date" (date 2025 3 15) p.end_date

(* TODO: Not sure this should be allowed since I think some functions might rely on start_date <= end_date *)
let make_inverted () =
  let p = Orcaset.Period.make ~start_date:(date 2025 6 1) ~end_date:(date 2025 1 1) in
  Alcotest.(check date_testable) "start_date" (date 2025 6 1) p.start_date;
  Alcotest.(check date_testable) "end_date" (date 2025 1 1) p.end_date

(* 2. make_offset *)

let make_offset_defaults () =
  let o = Orcaset.Period.make_offset () in
  Alcotest.(check int) "days" 0 o.days;
  Alcotest.(check int) "weeks" 0 o.weeks;
  Alcotest.(check int) "months" 0 o.months;
  Alcotest.(check int) "quarters" 0 o.quarters;
  Alcotest.(check int) "years" 0 o.years;
  Alcotest.(check bool) "month_end" false o.month_end

let make_offset_custom () =
  let o =
    Orcaset.Period.make_offset ~days:5 ~weeks:2 ~months:3 ~quarters:1 ~years:2 ~month_end:true ()
  in
  Alcotest.(check int) "days" 5 o.days;
  Alcotest.(check int) "weeks" 2 o.weeks;
  Alcotest.(check int) "months" 3 o.months;
  Alcotest.(check int) "quarters" 1 o.quarters;
  Alcotest.(check int) "years" 2 o.years;
  Alcotest.(check bool) "month_end" true o.month_end

let make_offset_single_days () =
  let o = Orcaset.Period.make_offset ~days:10 () in
  Alcotest.(check int) "days" 10 o.days;
  Alcotest.(check int) "weeks" 0 o.weeks;
  Alcotest.(check int) "months" 0 o.months;
  Alcotest.(check int) "quarters" 0 o.quarters;
  Alcotest.(check int) "years" 0 o.years;
  Alcotest.(check bool) "month_end" false o.month_end

let make_offset_single_weeks () =
  let o = Orcaset.Period.make_offset ~weeks:3 () in
  Alcotest.(check int) "days" 0 o.days;
  Alcotest.(check int) "weeks" 3 o.weeks;
  Alcotest.(check int) "months" 0 o.months;
  Alcotest.(check int) "quarters" 0 o.quarters;
  Alcotest.(check int) "years" 0 o.years;
  Alcotest.(check bool) "month_end" false o.month_end

let make_offset_single_months () =
  let o = Orcaset.Period.make_offset ~months:6 () in
  Alcotest.(check int) "days" 0 o.days;
  Alcotest.(check int) "weeks" 0 o.weeks;
  Alcotest.(check int) "months" 6 o.months;
  Alcotest.(check int) "quarters" 0 o.quarters;
  Alcotest.(check int) "years" 0 o.years;
  Alcotest.(check bool) "month_end" false o.month_end

let make_offset_single_quarters () =
  let o = Orcaset.Period.make_offset ~quarters:2 () in
  Alcotest.(check int) "days" 0 o.days;
  Alcotest.(check int) "weeks" 0 o.weeks;
  Alcotest.(check int) "months" 0 o.months;
  Alcotest.(check int) "quarters" 2 o.quarters;
  Alcotest.(check int) "years" 0 o.years;
  Alcotest.(check bool) "month_end" false o.month_end

let make_offset_single_years () =
  let o = Orcaset.Period.make_offset ~years:5 () in
  Alcotest.(check int) "days" 0 o.days;
  Alcotest.(check int) "weeks" 0 o.weeks;
  Alcotest.(check int) "months" 0 o.months;
  Alcotest.(check int) "quarters" 0 o.quarters;
  Alcotest.(check int) "years" 5 o.years;
  Alcotest.(check bool) "month_end" false o.month_end

let make_offset_negative_days () =
  let o = Orcaset.Period.make_offset ~days:(-5) () in
  Alcotest.(check int) "days" (-5) o.days;
  Alcotest.(check int) "weeks" 0 o.weeks;
  Alcotest.(check int) "months" 0 o.months;
  Alcotest.(check int) "quarters" 0 o.quarters;
  Alcotest.(check int) "years" 0 o.years

let make_offset_negative_months () =
  let o = Orcaset.Period.make_offset ~months:(-1) () in
  Alcotest.(check int) "days" 0 o.days;
  Alcotest.(check int) "months" (-1) o.months;
  Alcotest.(check int) "quarters" 0 o.quarters;
  Alcotest.(check int) "years" 0 o.years

let make_offset_negative_years () =
  let o = Orcaset.Period.make_offset ~years:(-2) () in
  Alcotest.(check int) "days" 0 o.days;
  Alcotest.(check int) "months" 0 o.months;
  Alcotest.(check int) "years" (-2) o.years

let make_offset_mixed_negative () =
  let o = Orcaset.Period.make_offset ~days:(-3) ~months:(-2) ~years:(-1) () in
  Alcotest.(check int) "days" (-3) o.days;
  Alcotest.(check int) "months" (-2) o.months;
  Alcotest.(check int) "years" (-1) o.years

(* 3. days *)

let days_31_day_month () =
  let p = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 2 1) in
  Alcotest.(check int) "Jan 1 to Feb 1 = 31 days" 31 (Orcaset.Period.days p)

let days_same_date () =
  let p = Orcaset.Period.make ~start_date:(date 2025 3 15) ~end_date:(date 2025 3 15) in
  Alcotest.(check int) "same date = 0 days" 0 (Orcaset.Period.days p)

let days_leap_year_feb () =
  let p = Orcaset.Period.make ~start_date:(date 2024 1 1) ~end_date:(date 2024 3 1) in
  Alcotest.(check int) "Jan 1 to Mar 1 2024 (leap) = 60 days" 60 (Orcaset.Period.days p)

let days_non_leap_year_feb () =
  let p = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 3 1) in
  Alcotest.(check int) "Jan 1 to Mar 1 2025 (non-leap) = 59 days" 59 (Orcaset.Period.days p)

let days_multi_year_span () =
  let p = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2027 1 1) in
  Alcotest.(check int) "Jan 1 2025 to Jan 1 2027 = 730 days" 730 (Orcaset.Period.days p)

let days_single_day () =
  let p = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 1 2) in
  Alcotest.(check int) "Jan 1 to Jan 2 = 1 day" 1 (Orcaset.Period.days p)

let days_inverted_negative () =
  let p = Orcaset.Period.make ~start_date:(date 2025 2 1) ~end_date:(date 2025 1 1) in
  Alcotest.(check int) "Feb 1 to Jan 1 = -31 days" (-31) (Orcaset.Period.days p)

let days_year_boundary () =
  let p = Orcaset.Period.make ~start_date:(date 2025 12 15) ~end_date:(date 2026 1 15) in
  Alcotest.(check int) "Dec 15 to Jan 15 = 31 days" 31 (Orcaset.Period.days p)

(* 4. contains *)

let contains_start_inclusive () =
  let p = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 1 31) in
  Alcotest.(check bool) "start date is inclusive" true (Orcaset.Period.contains p (date 2025 1 1))

let contains_end_inclusive () =
  let p = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 1 31) in
  Alcotest.(check bool) "end date is inclusive" true (Orcaset.Period.contains p (date 2025 1 31))

let contains_middle () =
  let p = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 1 31) in
  Alcotest.(check bool) "middle date" true (Orcaset.Period.contains p (date 2025 1 15))

let contains_before_exclusive () =
  let p = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 1 31) in
  Alcotest.(check bool) "before start" false (Orcaset.Period.contains p (date 2024 12 31))

let contains_after_exclusive () =
  let p = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 1 31) in
  Alcotest.(check bool) "after end" false (Orcaset.Period.contains p (date 2025 2 1))

let contains_zero_length () =
  let p = Orcaset.Period.make ~start_date:(date 2025 3 15) ~end_date:(date 2025 3 15) in
  Alcotest.(check bool) "exact date" true (Orcaset.Period.contains p (date 2025 3 15));
  Alcotest.(check bool) "day before" false (Orcaset.Period.contains p (date 2025 3 14));
  Alcotest.(check bool) "day after" false (Orcaset.Period.contains p (date 2025 3 16))

let contains_single_day () =
  let p = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 1 2) in
  Alcotest.(check bool) "start date" true (Orcaset.Period.contains p (date 2025 1 1));
  Alcotest.(check bool) "end date" true (Orcaset.Period.contains p (date 2025 1 2));
  Alcotest.(check bool) "day after" false (Orcaset.Period.contains p (date 2025 1 3))

let contains_year_boundary () =
  let p = Orcaset.Period.make ~start_date:(date 2025 12 15) ~end_date:(date 2026 1 15) in
  Alcotest.(check bool) "Dec 31" true (Orcaset.Period.contains p (date 2025 12 31));
  Alcotest.(check bool) "Jan 1" true (Orcaset.Period.contains p (date 2026 1 1));
  Alcotest.(check bool) "Jan 10" true (Orcaset.Period.contains p (date 2026 1 10))

(* 5. add_offset_to_date *)

let add_offset_to_date_days_only () =
  let offset = Orcaset.Period.make_offset ~days:10 () in
  let result = Orcaset.Period.add_offset_to_date offset (date 2025 1 1) in
  Alcotest.(check date_testable) "+10 days" (date 2025 1 11) result

let add_offset_to_date_weeks_only () =
  let offset = Orcaset.Period.make_offset ~weeks:2 () in
  let result = Orcaset.Period.add_offset_to_date offset (date 2025 1 1) in
  Alcotest.(check date_testable) "+2 weeks" (date 2025 1 15) result

let add_offset_to_date_months_only () =
  let offset = Orcaset.Period.make_offset ~months:3 () in
  let result = Orcaset.Period.add_offset_to_date offset (date 2025 1 1) in
  Alcotest.(check date_testable) "+3 months" (date 2025 4 1) result

let add_offset_to_date_quarters_only () =
  let offset = Orcaset.Period.make_offset ~quarters:1 () in
  let result = Orcaset.Period.add_offset_to_date offset (date 2025 1 1) in
  Alcotest.(check date_testable) "+1 quarter" (date 2025 4 1) result

let add_offset_to_date_years_only () =
  let offset = Orcaset.Period.make_offset ~years:1 () in
  let result = Orcaset.Period.add_offset_to_date offset (date 2025 1 1) in
  Alcotest.(check date_testable) "+1 year" (date 2026 1 1) result

let add_offset_to_date_combined () =
  let offset = Orcaset.Period.make_offset ~days:5 ~months:2 ~years:1 () in
  let result = Orcaset.Period.add_offset_to_date offset (date 2025 1 1) in
  Alcotest.(check date_testable) "+1y +2m +5d" (date 2026 3 6) result

let add_offset_to_date_month_end () =
  let offset = Orcaset.Period.make_offset ~month_end:true () in
  let result = Orcaset.Period.add_offset_to_date offset (date 2025 2 15) in
  Alcotest.(check date_testable) "Feb 15 -> Feb 28" (date 2025 2 28) result

let add_offset_to_date_month_end_with_months () =
  let offset = Orcaset.Period.make_offset ~months:1 ~month_end:true () in
  let result = Orcaset.Period.add_offset_to_date offset (date 2025 1 15) in
  Alcotest.(check date_testable) "Jan 15 +1m month_end -> Feb 28" (date 2025 2 28) result

let add_offset_to_date_day_clamping_non_leap () =
  let offset = Orcaset.Period.make_offset ~months:1 () in
  let result = Orcaset.Period.add_offset_to_date offset (date 2025 1 31) in
  Alcotest.(check date_testable) "Jan 31 +1m -> Feb 28 (non-leap)" (date 2025 2 28) result

let add_offset_to_date_day_clamping_leap () =
  let offset = Orcaset.Period.make_offset ~months:1 () in
  let result = Orcaset.Period.add_offset_to_date offset (date 2024 1 31) in
  Alcotest.(check date_testable) "Jan 31 +1m -> Feb 29 (leap)" (date 2024 2 29) result

let add_offset_to_date_month_end_already_at_end () =
  let offset = Orcaset.Period.make_offset ~month_end:true () in
  let result = Orcaset.Period.add_offset_to_date offset (date 2025 1 31) in
  Alcotest.(check date_testable) "Jan 31 month_end -> Jan 31" (date 2025 1 31) result

let add_offset_to_date_month_end_leap_year () =
  let offset = Orcaset.Period.make_offset ~month_end:true () in
  let result = Orcaset.Period.add_offset_to_date offset (date 2024 2 15) in
  Alcotest.(check date_testable) "Feb 15 2024 month_end -> Feb 29" (date 2024 2 29) result

let add_offset_to_date_negative_months () =
  let offset = Orcaset.Period.make_offset ~months:(-1) () in
  let result = Orcaset.Period.add_offset_to_date offset (date 2025 3 1) in
  Alcotest.(check date_testable) "Mar 1 -1m -> Feb 1" (date 2025 2 1) result

let add_offset_to_date_zero_offset () =
  let offset = Orcaset.Period.make_offset () in
  let result = Orcaset.Period.add_offset_to_date offset (date 2025 6 15) in
  Alcotest.(check date_testable) "zero offset -> same date" (date 2025 6 15) result

let add_offset_to_date_days_and_month_end () =
  let offset = Orcaset.Period.make_offset ~days:20 ~month_end:true () in
  let result = Orcaset.Period.add_offset_to_date offset (date 2025 2 15) in
  Alcotest.(check date_testable) "Feb 15 +20d month_end -> Mar 31" (date 2025 3 31) result

let add_offset_to_date_quarter_boundary () =
  let offset = Orcaset.Period.make_offset ~quarters:2 () in
  let result = Orcaset.Period.add_offset_to_date offset (date 2025 1 1) in
  Alcotest.(check date_testable) "Jan 1 +2q -> Jul 1" (date 2025 7 1) result

let add_offset_to_date_weeks_and_days () =
  let offset = Orcaset.Period.make_offset ~weeks:1 ~days:3 () in
  let result = Orcaset.Period.add_offset_to_date offset (date 2025 1 1) in
  Alcotest.(check date_testable) "Jan 1 +1w +3d -> Jan 11" (date 2025 1 11) result

(* 6. add_offset *)

let add_offset_both_dates_shift () =
  let p = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 2 1) in
  let offset = Orcaset.Period.make_offset ~months:1 () in
  let result = Orcaset.Period.add_offset offset p in
  let expected = Orcaset.Period.make ~start_date:(date 2025 2 1) ~end_date:(date 2025 3 1) in
  Alcotest.(check period_testable) "both dates shifted" expected result

let add_offset_month_end () =
  let p = Orcaset.Period.make ~start_date:(date 2025 1 15) ~end_date:(date 2025 2 15) in
  let offset = Orcaset.Period.make_offset ~month_end:true () in
  let result = Orcaset.Period.add_offset offset p in
  let expected = Orcaset.Period.make ~start_date:(date 2025 1 31) ~end_date:(date 2025 2 28) in
  Alcotest.(check period_testable) "both endpoints snap to month-end" expected result

let add_offset_day_clamping () =
  let p = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 1 31) in
  let offset = Orcaset.Period.make_offset ~months:1 () in
  let result = Orcaset.Period.add_offset offset p in
  let expected = Orcaset.Period.make ~start_date:(date 2025 2 1) ~end_date:(date 2025 2 28) in
  Alcotest.(check period_testable) "end_date clamps to Feb 28" expected result

let add_offset_zero () =
  let p = Orcaset.Period.make ~start_date:(date 2025 3 10) ~end_date:(date 2025 4 10) in
  let offset = Orcaset.Period.make_offset () in
  let result = Orcaset.Period.add_offset offset p in
  Alcotest.(check period_testable) "zero offset returns identical period" p result

let add_offset_multi_field () =
  let p = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 2 1) in
  let offset = Orcaset.Period.make_offset ~months:2 ~days:5 () in
  let result = Orcaset.Period.add_offset offset p in
  let expected = Orcaset.Period.make ~start_date:(date 2025 3 6) ~end_date:(date 2025 4 6) in
  Alcotest.(check period_testable) "months + days applied to both dates" expected result

let add_offset_year_crossing () =
  let p = Orcaset.Period.make ~start_date:(date 2025 11 1) ~end_date:(date 2025 12 1) in
  let offset = Orcaset.Period.make_offset ~months:3 () in
  let result = Orcaset.Period.add_offset offset p in
  let expected = Orcaset.Period.make ~start_date:(date 2026 2 1) ~end_date:(date 2026 3 1) in
  Alcotest.(check period_testable) "Nov-Dec +3m crosses into next year" expected result

(* 7. equal *)

let equal_identical () =
  let p1 = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 2 1) in
  let p2 = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 2 1) in
  Alcotest.(check bool) "identical periods" true (Orcaset.Period.equal p1 p2)

let equal_different_start () =
  let p1 = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 2 1) in
  let p2 = Orcaset.Period.make ~start_date:(date 2025 1 2) ~end_date:(date 2025 2 1) in
  Alcotest.(check bool) "different start" false (Orcaset.Period.equal p1 p2)

let equal_different_end () =
  let p1 = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 2 1) in
  let p2 = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 2 2) in
  Alcotest.(check bool) "different end" false (Orcaset.Period.equal p1 p2)

let equal_both_dates_different () =
  let p1 = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 2 1) in
  let p2 = Orcaset.Period.make ~start_date:(date 2025 1 2) ~end_date:(date 2025 2 2) in
  Alcotest.(check bool) "both dates different" false (Orcaset.Period.equal p1 p2)

let equal_symmetry () =
  let p1 = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 2 1) in
  let p2 = Orcaset.Period.make ~start_date:(date 2025 1 2) ~end_date:(date 2025 2 2) in
  Alcotest.(check bool)
    "equal p1 p2 = equal p2 p1" (Orcaset.Period.equal p1 p2) (Orcaset.Period.equal p2 p1)

let equal_reflexivity () =
  let p = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 2 1) in
  Alcotest.(check bool) "equal p p" true (Orcaset.Period.equal p p)

(* 8. make_seq *)

let make_seq_first_period_starts_at_start_date () =
  let seq =
    Orcaset.Period.make_seq ~start_date:(date 2025 1 1)
      ~offset:(Orcaset.Period.make_offset ~months:1 ())
  in
  let first = Seq.uncons seq |> Option.get |> fst in
  Alcotest.(check date_testable) "first period start" (date 2025 1 1) first.start_date

let make_seq_monthly_3_contiguous () =
  let seq =
    Orcaset.Period.make_seq ~start_date:(date 2025 1 1)
      ~offset:(Orcaset.Period.make_offset ~months:1 ())
  in
  let p1, rest = Seq.uncons seq |> Option.get in
  let p2, rest = Seq.uncons rest |> Option.get in
  let p3, _ = Seq.uncons rest |> Option.get in
  Alcotest.(check date_testable) "p1 end = p2 start" p1.end_date p2.start_date;
  Alcotest.(check date_testable) "p2 end = p3 start" p2.end_date p3.start_date

let make_seq_quarterly () =
  let seq =
    Orcaset.Period.make_seq ~start_date:(date 2025 1 1)
      ~offset:(Orcaset.Period.make_offset ~quarters:1 ())
  in
  let p1, rest = Seq.uncons seq |> Option.get in
  let p2, _ = Seq.uncons rest |> Option.get in
  let expected_p1 = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 4 1) in
  let expected_p2 = Orcaset.Period.make ~start_date:(date 2025 4 1) ~end_date:(date 2025 7 1) in
  Alcotest.(check period_testable) "Q1" expected_p1 p1;
  Alcotest.(check period_testable) "Q2" expected_p2 p2

let make_seq_month_end_offset () =
  let seq =
    Orcaset.Period.make_seq ~start_date:(date 2025 1 31)
      ~offset:(Orcaset.Period.make_offset ~months:1 ~month_end:true ())
  in
  let p1, rest = Seq.uncons seq |> Option.get in
  let p2, rest = Seq.uncons rest |> Option.get in
  let p3, rest = Seq.uncons rest |> Option.get in
  let p4, _ = Seq.uncons rest |> Option.get in
  (* All boundaries should snap to month-ends *)
  let expected_p1 = Orcaset.Period.make ~start_date:(date 2025 1 31) ~end_date:(date 2025 2 28) in
  let expected_p2 = Orcaset.Period.make ~start_date:(date 2025 2 28) ~end_date:(date 2025 3 31) in
  let expected_p3 = Orcaset.Period.make ~start_date:(date 2025 3 31) ~end_date:(date 2025 4 30) in
  let expected_p4 = Orcaset.Period.make ~start_date:(date 2025 4 30) ~end_date:(date 2025 5 31) in
  Alcotest.(check period_testable) "Jan 31 -> Feb 28" expected_p1 p1;
  Alcotest.(check period_testable) "Feb 28 -> Mar 31" expected_p2 p2;
  Alcotest.(check period_testable) "Mar 31 -> Apr 30" expected_p3 p3;
  Alcotest.(check period_testable) "Apr 30 -> May 31" expected_p4 p4;
  (* Period lengths vary because months have different day counts *)
  Alcotest.(check int) "p1 = 28 days" 28 (Orcaset.Period.days p1);
  Alcotest.(check int) "p2 = 31 days" 31 (Orcaset.Period.days p2);
  Alcotest.(check int) "p3 = 30 days" 30 (Orcaset.Period.days p3);
  Alcotest.(check int) "p4 = 31 days" 31 (Orcaset.Period.days p4)

let make_seq_quarterly_contiguous_dates () =
  let seq =
    Orcaset.Period.make_seq ~start_date:(date 2025 4 1)
      ~offset:(Orcaset.Period.make_offset ~quarters:1 ())
  in
  let p1, rest = Seq.uncons seq |> Option.get in
  let p2, rest = Seq.uncons rest |> Option.get in
  let p3, _ = Seq.uncons rest |> Option.get in
  let expected_q2 = Orcaset.Period.make ~start_date:(date 2025 4 1) ~end_date:(date 2025 7 1) in
  let expected_q3 = Orcaset.Period.make ~start_date:(date 2025 7 1) ~end_date:(date 2025 10 1) in
  let expected_q4 = Orcaset.Period.make ~start_date:(date 2025 10 1) ~end_date:(date 2026 1 1) in
  Alcotest.(check period_testable) "Q2 dates" expected_q2 p1;
  Alcotest.(check period_testable) "Q3 dates" expected_q3 p2;
  Alcotest.(check period_testable) "Q4 dates" expected_q4 p3

(* 9. print *)

let print_formats_correctly () =
  let p = Orcaset.Period.make ~start_date:(date 2025 1 1) ~end_date:(date 2025 2 1) in
  let buf = Buffer.create 64 in
  let old_stdout = Unix.dup Unix.stdout in
  let rd, wr = Unix.pipe () in
  Unix.dup2 wr Unix.stdout;
  Unix.close wr;
  Orcaset.Period.print p;
  Printf.printf "%!";
  Unix.dup2 old_stdout Unix.stdout;
  Unix.close old_stdout;
  let ic = Unix.in_channel_of_descr rd in
  (try
     while true do
       Buffer.add_char buf (input_char ic)
     done
   with End_of_file -> ());
  close_in ic;
  let output = Buffer.contents buf in
  Alcotest.(check string)
    "print output" "Period { start_date = 2025-01-01; end_date = 2025-02-01 }\n" output

(* Suite *)

let suite =
  [
    ( "make",
      [
        Alcotest.test_case "correct dates" `Quick make_correct_dates;
        Alcotest.test_case "zero-length period" `Quick make_zero_length;
        Alcotest.test_case "inverted period" `Quick make_inverted;
      ] );
    ( "make_offset",
      [
        Alcotest.test_case "defaults" `Quick make_offset_defaults;
        Alcotest.test_case "custom values" `Quick make_offset_custom;
        Alcotest.test_case "single days" `Quick make_offset_single_days;
        Alcotest.test_case "single weeks" `Quick make_offset_single_weeks;
        Alcotest.test_case "single months" `Quick make_offset_single_months;
        Alcotest.test_case "single quarters" `Quick make_offset_single_quarters;
        Alcotest.test_case "single years" `Quick make_offset_single_years;
        Alcotest.test_case "negative days" `Quick make_offset_negative_days;
        Alcotest.test_case "negative months" `Quick make_offset_negative_months;
        Alcotest.test_case "negative years" `Quick make_offset_negative_years;
        Alcotest.test_case "mixed negative" `Quick make_offset_mixed_negative;
      ] );
    ( "days",
      [
        Alcotest.test_case "31-day month" `Quick days_31_day_month;
        Alcotest.test_case "same date" `Quick days_same_date;
        Alcotest.test_case "leap year Feb" `Quick days_leap_year_feb;
        Alcotest.test_case "non-leap year Feb" `Quick days_non_leap_year_feb;
        Alcotest.test_case "multi-year span" `Quick days_multi_year_span;
        Alcotest.test_case "single day" `Quick days_single_day;
        Alcotest.test_case "inverted negative" `Quick days_inverted_negative;
        Alcotest.test_case "year boundary" `Quick days_year_boundary;
      ] );
    ( "contains",
      [
        Alcotest.test_case "start inclusive" `Quick contains_start_inclusive;
        Alcotest.test_case "end inclusive" `Quick contains_end_inclusive;
        Alcotest.test_case "middle" `Quick contains_middle;
        Alcotest.test_case "before exclusive" `Quick contains_before_exclusive;
        Alcotest.test_case "after exclusive" `Quick contains_after_exclusive;
        Alcotest.test_case "zero-length period" `Quick contains_zero_length;
        Alcotest.test_case "single-day period" `Quick contains_single_day;
        Alcotest.test_case "year boundary" `Quick contains_year_boundary;
      ] );
    ( "add_offset_to_date",
      [
        Alcotest.test_case "days only" `Quick add_offset_to_date_days_only;
        Alcotest.test_case "weeks only" `Quick add_offset_to_date_weeks_only;
        Alcotest.test_case "months only" `Quick add_offset_to_date_months_only;
        Alcotest.test_case "quarters only" `Quick add_offset_to_date_quarters_only;
        Alcotest.test_case "years only" `Quick add_offset_to_date_years_only;
        Alcotest.test_case "combined" `Quick add_offset_to_date_combined;
        Alcotest.test_case "month_end" `Quick add_offset_to_date_month_end;
        Alcotest.test_case "month_end with months" `Quick add_offset_to_date_month_end_with_months;
        Alcotest.test_case "day clamping non-leap" `Quick add_offset_to_date_day_clamping_non_leap;
        Alcotest.test_case "day clamping leap" `Quick add_offset_to_date_day_clamping_leap;
        Alcotest.test_case "month_end already at end" `Quick
          add_offset_to_date_month_end_already_at_end;
        Alcotest.test_case "month_end leap year" `Quick add_offset_to_date_month_end_leap_year;
        Alcotest.test_case "negative months" `Quick add_offset_to_date_negative_months;
        Alcotest.test_case "zero offset" `Quick add_offset_to_date_zero_offset;
        Alcotest.test_case "days + month_end" `Quick add_offset_to_date_days_and_month_end;
        Alcotest.test_case "quarter boundary" `Quick add_offset_to_date_quarter_boundary;
        Alcotest.test_case "weeks + days combined" `Quick add_offset_to_date_weeks_and_days;
      ] );
    ( "add_offset",
      [
        Alcotest.test_case "both dates shift" `Quick add_offset_both_dates_shift;
        Alcotest.test_case "month_end snaps both endpoints" `Quick add_offset_month_end;
        Alcotest.test_case "day-of-month clamping" `Quick add_offset_day_clamping;
        Alcotest.test_case "zero offset" `Quick add_offset_zero;
        Alcotest.test_case "multi-field offset" `Quick add_offset_multi_field;
        Alcotest.test_case "year-crossing offset" `Quick add_offset_year_crossing;
      ] );
    ( "equal",
      [
        Alcotest.test_case "identical" `Quick equal_identical;
        Alcotest.test_case "different start" `Quick equal_different_start;
        Alcotest.test_case "different end" `Quick equal_different_end;
        Alcotest.test_case "both dates different" `Quick equal_both_dates_different;
        Alcotest.test_case "symmetry" `Quick equal_symmetry;
        Alcotest.test_case "reflexivity" `Quick equal_reflexivity;
      ] );
    ( "make_seq",
      [
        Alcotest.test_case "first period start" `Quick make_seq_first_period_starts_at_start_date;
        Alcotest.test_case "monthly contiguous" `Quick make_seq_monthly_3_contiguous;
        Alcotest.test_case "quarterly" `Quick make_seq_quarterly;
        Alcotest.test_case "month-end offset" `Quick make_seq_month_end_offset;
        Alcotest.test_case "quarterly contiguous dates" `Quick make_seq_quarterly_contiguous_dates;
      ] );
    ("print", [ Alcotest.test_case "formats correctly" `Quick print_formats_correctly ]);
  ]
