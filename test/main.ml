let () =
  Alcotest.run "orcaset"
    [
      ("Date", Test_date.tests);
      ("Yf", Test_yf.tests);
      ("Split", Test_split.tests);
      ("Period", Test_period.tests);
      ("Series", Test_series.tests);
      ("Stmt", Test_stmt.tests);
    ]
