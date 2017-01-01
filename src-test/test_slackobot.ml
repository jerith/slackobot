(*
* Collector for all slackobot tests.
*)

open OUnit2

let suite =
  "test_slackobot" >::: [
    Test_oyjson.suite;
  ]

let () = run_test_tt_main suite
