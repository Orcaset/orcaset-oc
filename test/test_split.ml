open Orcaset

let d = Date.make
let p = Period.make
let check_float = Alcotest.(check (float 1e-12))

let split_values split ~period ~date value =
  let left, right = split ~period ~date in
  (Split.value left value, Split.value right value)

let test_daily () =
  let period = p (d 2026 1 1) (d 2026 4 1) in
  let left, right = split_values Split.daily ~period ~date:(d 2026 2 1) 90.0 in
  check_float "left" 31.0 left;
  check_float "right" 59.0 right

let test_const () =
  let period = p (d 2026 1 1) (d 2026 4 1) in
  let left, right = split_values Split.const ~period ~date:(d 2026 2 1) 10.0 in
  check_float "left" 10.0 left;
  check_float "right" 10.0 right

let check_year_frac_split name split year_frac =
  let start = d 2024 1 15 in
  let split_date = d 2024 3 1 in
  let end_ = d 2024 4 10 in
  let period = p start end_ in
  let value = 1200.0 in
  let denominator = year_frac start end_ in
  let expected_left = value *. year_frac start split_date /. denominator in
  let expected_right = value *. year_frac split_date end_ /. denominator in
  let left, right = split_values split ~period ~date:split_date value in
  check_float (name ^ " left") expected_left left;
  check_float (name ^ " right") expected_right right

let test_cmonthly () = check_year_frac_split "cmonthly" Split.cmonthly Yf.cmonthly
let test_act_360 () = check_year_frac_split "act_360" Split.act_360 Yf.act_360

let tests =
  List.map
    (fun (name, f) -> Alcotest.test_case name `Quick f)
    [
      ("daily", test_daily);
      ("const", test_const);
      ("cmonthly", test_cmonthly);
      ("act_360", test_act_360);
    ]
