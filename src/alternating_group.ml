(* src/alternating_group.ml *)
open! Base
open Group

(** Implementation of alternating groups *)
type t = int array

module M (N : sig
    val n : int
  end) : GROUP with type t = t = struct
  type nonrec t = t

  module Sym = Symmetric_group.M (N)

  let order = Sym.order / 2
  let identity = Sym.identity
  let multiply = Sym.multiply
  let inverse = Sym.inverse
  let equal = Sym.equal
  let structure = Group_structure.Alternating N.n

  (** Calculate the parity of a permutation using inversion count *)
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

  let is_even p = count_inversions p % 2 = 0
  let elements = Sequence.filter Sym.elements ~f:is_even
end
