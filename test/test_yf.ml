open Orcaset

let d = Date.make
let check_float = Alcotest.(check (float 1e-12))

let test_act_360 () =
  check_float "actual days over 360" (30.0 /. 360.0) (Yf.act_360 (d 2026 1 1) (d 2026 1 31))

let test_act_act_same_year () =
  check_float "same leap year" (182.0 /. 366.0) (Yf.act_act (d 2024 1 1) (d 2024 7 1))

let test_act_act_cross_year () =
  check_float "cross year"
    ((1.0 /. 365.0) +. (1.0 /. 366.0))
    (Yf.act_act (d 2023 12 31) (d 2024 1 2))

let test_act_act_reversed_dates () =
  check_float "reversed dates"
    (-.((1.0 /. 365.0) +. (1.0 /. 366.0)))
    (Yf.act_act (d 2024 1 2) (d 2023 12 31))

let tests =
  List.map
    (fun (name, f) -> Alcotest.test_case name `Quick f)
    [
      ("act_360", test_act_360);
      ("act_act same year", test_act_act_same_year);
      ("act_act cross year", test_act_act_cross_year);
      ("act_act reversed dates", test_act_act_reversed_dates);
    ]
