let () =
  Alcotest.run "orcaset"
    (Yf_tests.suite @ Balance_tests.Test_basic.suite @ Balance_tests.Test_combine.suite)
