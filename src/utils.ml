(* src/utils.ml *)

(** Utility functions for permutations *)

(** Swap elements at indices i and j in an array *)
let swap arr i j =
  let temp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- temp
;;

(** Factorial function *)
let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
