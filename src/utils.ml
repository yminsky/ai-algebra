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

(** Reverse a subarray from index [start] to [stop] inclusive *)
let reverse_subarray arr start stop =
  let rec helper i j =
    if i < j
    then (
      swap arr i j;
      helper (i + 1) (j - 1))
  in
  helper start stop
;;
