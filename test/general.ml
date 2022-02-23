open OUnit2
open Common

let tests = [
  "recursion">::(
    assert_eval_int
    10
    "
(def id (: (-> Int Int) (fn (x)
  (if (== x 0)
      0
      (+ 1 (id (- x 1)))))))
      
(id 10)
    "
  );
  "gcd">::(
    assert_eval_all
    [Lix.Type.Int 24; Lix.Type.Int 12]
    "
(def gcd (: (-> Int Int Int) (fn (x y)
  (if (== y 0) 
      x
      (gcd y (% x y))))))
(gcd 72 192)
(gcd 24 36)
    "
  )
  

]