let date y m d = CalendarLib.Date.make y m d

let make_basic () =
  let d = date 2025 1 15 in
  let b = Orcaset.Balance.make ~date:d ~amount:(lazy 1000.0) in
  Alcotest.(check (float 1e-9)) "amount" 1000.0 (Lazy.force b.amount);
  Alcotest.(check bool) "date" true (CalendarLib.Date.equal d b.date)

let on_empty_sequence () =
  let query_date = date 2025 1 1 in
  let seq = Seq.empty in
  let result = Orcaset.Balance.on seq query_date in
  Alcotest.(check (float 1e-9)) "empty sequence returns 0" 0.0 (Lazy.force result.amount);
  Alcotest.(check bool) "date matches query" true (CalendarLib.Date.equal query_date result.date)

let on_exact_date_match () =
  let d1 = date 2025 1 1 in
  let d2 = date 2025 2 1 in
  let b1 = Orcaset.Balance.make ~date:d1 ~amount:(lazy 100.0) in
  let b2 = Orcaset.Balance.make ~date:d2 ~amount:(lazy 200.0) in
  let seq = List.to_seq [ b1; b2 ] in
  let result = Orcaset.Balance.on seq d2 in
  Alcotest.(check (float 1e-9)) "exact match amount" 200.0 (Lazy.force result.amount)

let on_date_before_first () =
  let d1 = date 2025 2 1 in
  let b1 = Orcaset.Balance.make ~date:d1 ~amount:(lazy 100.0) in
  let seq = Seq.return b1 in
  let query_date = date 2025 1 1 in
  let result = Orcaset.Balance.on seq query_date in
  Alcotest.(check (float 1e-9)) "before first returns 0" 0.0 (Lazy.force result.amount)

let on_date_between_balances () =
  let d1 = date 2025 1 1 in
  let d2 = date 2025 3 1 in
  let b1 = Orcaset.Balance.make ~date:d1 ~amount:(lazy 100.0) in
  let b2 = Orcaset.Balance.make ~date:d2 ~amount:(lazy 200.0) in
  let seq = List.to_seq [ b1; b2 ] in
  let query_date = date 2025 2 1 in
  let result = Orcaset.Balance.on seq query_date in
  Alcotest.(check (float 1e-9))
    "between balances returns first amount" 100.0 (Lazy.force result.amount)

let on_date_after_last () =
  let d1 = date 2025 1 1 in
  let d2 = date 2025 2 1 in
  let b1 = Orcaset.Balance.make ~date:d1 ~amount:(lazy 100.0) in
  let b2 = Orcaset.Balance.make ~date:d2 ~amount:(lazy 200.0) in
  let seq = List.to_seq [ b1; b2 ] in
  let query_date = date 2025 3 1 in
  let result = Orcaset.Balance.on seq query_date in
  Alcotest.(check (float 1e-9)) "after last returns last amount" 200.0 (Lazy.force result.amount)

let to_string_format () =
  let d = date 2025 1 15 in
  let b = Orcaset.Balance.make ~date:d ~amount:(lazy 1234.56) in
  let result = Orcaset.Balance.to_string b in
  Alcotest.(check string) "format" "Date: 2025-01-15, Amount: 1234.56" result

let to_string_negative () =
  let d = date 2025 1 15 in
  let b = Orcaset.Balance.make ~date:d ~amount:(lazy (-500.25)) in
  let result = Orcaset.Balance.to_string b in
  Alcotest.(check string) "negative format" "Date: 2025-01-15, Amount: -500.25" result

let suite =
  [
    ("make", [ Alcotest.test_case "basic construction" `Quick make_basic ]);
    ( "on",
      [
        Alcotest.test_case "empty sequence" `Quick on_empty_sequence;
        Alcotest.test_case "exact date match" `Quick on_exact_date_match;
        Alcotest.test_case "date before first" `Quick on_date_before_first;
        Alcotest.test_case "date between balances" `Quick on_date_between_balances;
        Alcotest.test_case "date after last" `Quick on_date_after_last;
      ] );
    ( "to_string",
      [
        Alcotest.test_case "format check" `Quick to_string_format;
        Alcotest.test_case "negative amount" `Quick to_string_negative;
      ] );
  ]
