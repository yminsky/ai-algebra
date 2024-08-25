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

  (** Calculate the parity of a permutation *)
  let is_even p =
    let rec count_inversions arr =
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
        let i = ref l
        and j = ref (m + 1)
        and k = ref l
        and inv_count = ref 0 in
        while !i <= m && !j <= r do
          if temp.(!i) <= temp.(!j)
          then (
            arr.(!k) <- temp.(!i);
            incr i)
          else (
            arr.(!k) <- temp.(!j);
            incr j;
            inv_count := !inv_count + (m - !i + 1));
          incr k
        done;
        while !i <= m do
          arr.(!k) <- temp.(!i);
          incr i;
          incr k
        done;
        while !j <= r do
          arr.(!k) <- temp.(!j);
          incr j;
          incr k
        done;
        !inv_count
      in
      merge_sort 0 (n - 1)
    in
    count_inversions (Array.copy p) % 2 = 0
  ;;

  let elements = Sequence.filter Sym.elements ~f:is_even
end
