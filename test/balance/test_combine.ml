let date y m d = CalendarLib.Date.make y m d

let combine_same_date () =
  let d = date 2025 1 1 in
  let b1 = Orcaset.Balance.make ~date:d ~amount:(lazy 100.0) in
  let b2 = Orcaset.Balance.make ~date:d ~amount:(lazy 200.0) in
  let result = Orcaset.Balance.combine b1 b2 ( +. ) |> List.of_seq in
  Alcotest.(check int) "single result" 1 (List.length result);
  let first = List.hd result in
  Alcotest.(check (float 1e-9)) "combined amount" 300.0 (Lazy.force first.amount)

let combine_b1_before_b2 () =
  let d1 = date 2025 1 1 in
  let d2 = date 2025 2 1 in
  let b1 = Orcaset.Balance.make ~date:d1 ~amount:(lazy 100.0) in
  let b2 = Orcaset.Balance.make ~date:d2 ~amount:(lazy 200.0) in
  let result = Orcaset.Balance.combine b1 b2 ( +. ) |> List.of_seq in
  Alcotest.(check int) "two results" 2 (List.length result);
  let first = List.nth result 0 in
  let second = List.nth result 1 in
  Alcotest.(check (float 1e-9)) "first amount (b1 + 0)" 100.0 (Lazy.force first.amount);
  Alcotest.(check (float 1e-9)) "second amount (b1 + b2)" 300.0 (Lazy.force second.amount)

let combine_b2_before_b1 () =
  let d1 = date 2025 2 1 in
  let d2 = date 2025 1 1 in
  let b1 = Orcaset.Balance.make ~date:d1 ~amount:(lazy 100.0) in
  let b2 = Orcaset.Balance.make ~date:d2 ~amount:(lazy 200.0) in
  let result = Orcaset.Balance.combine b1 b2 ( +. ) |> List.of_seq in
  Alcotest.(check int) "two results" 2 (List.length result);
  let first = List.nth result 0 in
  let second = List.nth result 1 in
  Alcotest.(check (float 1e-9)) "first amount (0 + b2)" 200.0 (Lazy.force first.amount);
  Alcotest.(check (float 1e-9)) "second amount (b1 + b2)" 300.0 (Lazy.force second.amount)

let combine_custom_combiner () =
  let d = date 2025 1 1 in
  let b1 = Orcaset.Balance.make ~date:d ~amount:(lazy 500.0) in
  let b2 = Orcaset.Balance.make ~date:d ~amount:(lazy 200.0) in
  let result = Orcaset.Balance.combine b1 b2 ( -. ) |> List.of_seq in
  let first = List.hd result in
  Alcotest.(check (float 1e-9)) "subtraction" 300.0 (Lazy.force first.amount)

let combine_seq_both_empty () =
  let seq1 = Seq.empty in
  let seq2 = Seq.empty in
  let result = Orcaset.Balance.combine_seq ( +. ) seq1 seq2 |> List.of_seq in
  Alcotest.(check int) "empty result" 0 (List.length result)

let combine_seq_first_empty () =
  let seq1 = Seq.empty in
  let b1 = Orcaset.Balance.make ~date:(date 2025 1 1) ~amount:(lazy 100.0) in
  let seq2 = Seq.return b1 in
  let result = Orcaset.Balance.combine_seq ( +. ) seq1 seq2 |> List.of_seq in
  Alcotest.(check int) "one result" 1 (List.length result);
  let first = List.hd result in
  Alcotest.(check (float 1e-9)) "carries second seq amount" 100.0 (Lazy.force first.amount)

let combine_seq_second_empty () =
  let b1 = Orcaset.Balance.make ~date:(date 2025 1 1) ~amount:(lazy 100.0) in
  let seq1 = Seq.return b1 in
  let seq2 = Seq.empty in
  let result = Orcaset.Balance.combine_seq ( +. ) seq1 seq2 |> List.of_seq in
  Alcotest.(check int) "one result" 1 (List.length result);
  let first = List.hd result in
  Alcotest.(check (float 1e-9)) "carries first seq amount" 100.0 (Lazy.force first.amount)

let combine_seq_aligned_dates () =
  let d1 = date 2025 1 1 in
  let d2 = date 2025 2 1 in
  let b1 = Orcaset.Balance.make ~date:d1 ~amount:(lazy 100.0) in
  let b2 = Orcaset.Balance.make ~date:d2 ~amount:(lazy 200.0) in
  let seq1 = List.to_seq [ b1; b2 ] in
  let b3 = Orcaset.Balance.make ~date:d1 ~amount:(lazy 50.0) in
  let b4 = Orcaset.Balance.make ~date:d2 ~amount:(lazy 75.0) in
  let seq2 = List.to_seq [ b3; b4 ] in
  let result = Orcaset.Balance.combine_seq ( +. ) seq1 seq2 |> List.of_seq in
  Alcotest.(check int) "two results" 2 (List.length result);
  let first = List.nth result 0 in
  let second = List.nth result 1 in
  Alcotest.(check (float 1e-9)) "first combined" 150.0 (Lazy.force first.amount);
  Alcotest.(check (float 1e-9)) "second combined" 275.0 (Lazy.force second.amount)

let combine_seq_interleaved_dates () =
  let d1 = date 2025 1 1 in
  let d2 = date 2025 2 1 in
  let d3 = date 2025 3 1 in
  let d4 = date 2025 4 1 in
  let b1 = Orcaset.Balance.make ~date:d1 ~amount:(lazy 100.0) in
  let b2 = Orcaset.Balance.make ~date:d3 ~amount:(lazy 200.0) in
  let seq1 = List.to_seq [ b1; b2 ] in
  let b3 = Orcaset.Balance.make ~date:d2 ~amount:(lazy 50.0) in
  let b4 = Orcaset.Balance.make ~date:d4 ~amount:(lazy 75.0) in
  let seq2 = List.to_seq [ b3; b4 ] in
  let result = Orcaset.Balance.combine_seq ( +. ) seq1 seq2 |> List.of_seq in
  Alcotest.(check int) "four results" 4 (List.length result);
  let amounts = List.map (fun (b : Orcaset.Balance.t) -> Lazy.force b.amount) result in
  Alcotest.(check (list (float 1e-9))) "interleaved amounts" [ 100.0; 150.0; 250.0; 275.0 ] amounts

let combine_seq_overlapping_dates () =
  let d1 = date 2025 1 1 in
  let d2 = date 2025 2 1 in
  let d3 = date 2025 3 1 in
  let b1 = Orcaset.Balance.make ~date:d1 ~amount:(lazy 100.0) in
  let b2 = Orcaset.Balance.make ~date:d2 ~amount:(lazy 200.0) in
  let seq1 = List.to_seq [ b1; b2 ] in
  let b3 = Orcaset.Balance.make ~date:d1 ~amount:(lazy 50.0) in
  let b4 = Orcaset.Balance.make ~date:d3 ~amount:(lazy 75.0) in
  let seq2 = List.to_seq [ b3; b4 ] in
  let result = Orcaset.Balance.combine_seq ( +. ) seq1 seq2 |> List.of_seq in
  Alcotest.(check int) "three results" 3 (List.length result);
  let amounts = List.map (fun (b : Orcaset.Balance.t) -> Lazy.force b.amount) result in
  Alcotest.(check (list (float 1e-9))) "overlapping amounts" [ 150.0; 250.0; 275.0 ] amounts

let combine_seq_different_lengths () =
  let d1 = date 2025 1 1 in
  let d2 = date 2025 2 1 in
  let d3 = date 2025 3 1 in
  let b1 = Orcaset.Balance.make ~date:d1 ~amount:(lazy 100.0) in
  let seq1 = Seq.return b1 in
  let b2 = Orcaset.Balance.make ~date:d1 ~amount:(lazy 50.0) in
  let b3 = Orcaset.Balance.make ~date:d2 ~amount:(lazy 75.0) in
  let b4 = Orcaset.Balance.make ~date:d3 ~amount:(lazy 25.0) in
  let seq2 = List.to_seq [ b2; b3; b4 ] in
  let result = Orcaset.Balance.combine_seq ( +. ) seq1 seq2 |> List.of_seq in
  Alcotest.(check int) "three results" 3 (List.length result);
  let amounts = List.map (fun (b : Orcaset.Balance.t) -> Lazy.force b.amount) result in
  Alcotest.(check (list (float 1e-9))) "different length amounts" [ 150.0; 175.0; 125.0 ] amounts

let suite =
  [
    ( "combine",
      [
        Alcotest.test_case "same date" `Quick combine_same_date;
        Alcotest.test_case "b1 before b2" `Quick combine_b1_before_b2;
        Alcotest.test_case "b2 before b1" `Quick combine_b2_before_b1;
        Alcotest.test_case "custom combiner" `Quick combine_custom_combiner;
      ] );
    ( "combine_seq",
      [
        Alcotest.test_case "both empty" `Quick combine_seq_both_empty;
        Alcotest.test_case "first empty" `Quick combine_seq_first_empty;
        Alcotest.test_case "second empty" `Quick combine_seq_second_empty;
        Alcotest.test_case "aligned dates" `Quick combine_seq_aligned_dates;
        Alcotest.test_case "interleaved dates" `Quick combine_seq_interleaved_dates;
        Alcotest.test_case "overlapping dates" `Quick combine_seq_overlapping_dates;
        Alcotest.test_case "different lengths" `Quick combine_seq_different_lengths;
      ] );
  ]
