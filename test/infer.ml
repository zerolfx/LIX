open OUnit2
open Common

let tests = [
  "basic">::(
    assert_eval_int
    3
    "((fn (x y) (+ x y)) 1 2)"
  );
  "basic 2">::(
    assert_eval_int
    3
    "((fn (f x) (f x)) (+ 1) 2)"
  );
  "apply">::(
    assert_eval_int
    3
    "(def apply (fn (f x) (f x)))
     (apply (+ 1) 2)"
  );
  "id">::(
    assert_eval_all
    [Lix.Type.Int 3; Lix.Type.Bool false]
    "(def id (fn (x) x))
     (id 3)
     (id false)"
  );
  "compose">::(
    assert_eval_int
    5
    "(def compose (fn (f g) (fn (x) (f (g x)))))
     (compose (+ 2) (* 3) 1)"
  );
  "infinite type">::(fun _ -> 
    assert_raises Lix.Tast.InfiniteType 
    (fun _ -> eval "(def f (fn x (x x)))"));
]