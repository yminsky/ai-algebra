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

(** Calculate the parity of a permutation using inversion count

    An inversion in a permutation is a pair of elements (i, j) where i < j,
    but i appears after j in the permutation. For example, in the permutation
    [3; 1; 4; 2], the inversions are (3,1), (3,2), and (4,2).

    The algorithm used here is a modification of merge sort. It counts
    inversions while sorting the array:
    1. Divide the array into two halves.
    2. Recursively count inversions in both halves.
    3. Merge the two sorted halves, counting additional inversions.
    4. The total inversion count is the sum of inversions from both halves
    plus the inversions counted during merging.

    This approach has a time complexity of O(n log n), where n is the length
    of the permutation. *)
let count_inversions arr =
  let n = Array.length arr in
  let temp = Array.copy arr in
  let rec merge_sort l r =
    if l >= r
    then 0
    else (
      let m = (l + r) / 2 in
      let inv_left = merge_sort l m in
      let inv_right = merge_sort (m + 1) r in
      let inv_merge = merge l m r in
      inv_left + inv_right + inv_merge)
  and merge l m r =
    let inv_count = ref 0 in
    let left = Array.sub temp ~pos:l ~len:(m - l + 1) in
    let right = Array.sub temp ~pos:(m + 1) ~len:(r - m) in
    let i = ref 0 in
    let j = ref 0 in
    let k = ref l in
    while !i < Array.length left && !j < Array.length right do
      if left.(!i) <= right.(!j)
      then (
        temp.(!k) <- left.(!i);
        i := !i + 1)
      else (
        temp.(!k) <- right.(!j);
        j := !j + 1;
        inv_count := !inv_count + (Array.length left - !i));
      k := !k + 1
    done;
    while !i < Array.length left do
      temp.(!k) <- left.(!i);
      i := !i + 1;
      k := !k + 1
    done;
    while !j < Array.length right do
      temp.(!k) <- right.(!j);
      j := !j + 1;
      k := !k + 1
    done;
    !inv_count
  in
  merge_sort 0 (n - 1)
;;
