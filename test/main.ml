let () =
  Alcotest.run "orcaset"
    [
      ("Date", Test_date.tests);
      ("Period", Test_period.tests);
      ("Series", Test_series.tests);
      ("Stmt", Test_stmt.tests);
    ]
