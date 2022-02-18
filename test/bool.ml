open OUnit2
open Common

let tests = [
  "simple compare">::(
    assert_eval_bool
    true
    "(< 1 2)"
  );
  "equality">::(
    assert_eval_bool
    false
    "(== 2 3)"
  );
  "bool and">::(
    assert_eval_bool
    true
    "(&& true true)"
  );
  "bool or">::(
    assert_eval_bool
    true
    "(|| true false)"
  );
  "bool not">::(
    assert_eval_bool
    false
    "(not true)"
  );
]