(* src/utils.ml *)
open! Base

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

(** Merge two sorted lists and count the number of inversions between them *)
let merge_and_count_inversions left right =
  let rec helper left right acc inversions =
    match left, right with
    | [], r -> List.rev_append acc r, inversions
    | l, [] -> List.rev_append acc l, inversions
    | l :: ls, r :: rs ->
      if l <= r
      then helper ls right (l :: acc) inversions
      else (
        let new_inversions = inversions + List.length left in
        helper left rs (r :: acc) new_inversions)
  in
  helper left right [] 0
;;

(** Sort a list and count inversions *)
let rec sort_and_count_inversions list =
  match list with
  | [] | [ _ ] -> list, 0
  | _ ->
    let mid = List.length list / 2 in
    let left, right = List.split_n list mid in
    let left_sorted, left_inv = sort_and_count_inversions left in
    let right_sorted, right_inv = sort_and_count_inversions right in
    let merged, split_inv =
      merge_and_count_inversions left_sorted right_sorted
    in
    merged, left_inv + right_inv + split_inv
;;

(** Calculate the parity of a permutation using inversion count

    An inversion in a permutation is a pair of elements (i, j) where i < j,
    but i appears after j in the permutation. For example, in the permutation
    [3; 1; 4; 2], the inversions are (3,1), (3,2), and (4,2).

    This implementation uses a divide-and-conquer approach similar to merge sort:
    1. Divide the array into two halves.
    2. Recursively count inversions in both halves.
    3. Count inversions that span both halves (split inversions).
    4. Sum up all inversions.

    This approach achieves O(n log n) time complexity. *)
let count_inversions arr =
  arr |> Array.to_list |> sort_and_count_inversions |> snd
;;
