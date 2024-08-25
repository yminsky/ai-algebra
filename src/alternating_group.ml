(* src/alternating_group.ml *)
open! Base
open Group

(** Implementation of alternating groups *)
type t = int array

module Make (N : sig
    val n : int
  end) : GROUP with type t = t = struct
  type nonrec t = t

  module Sym = Symmetric_group.Make (N)

  let order = Sym.order / 2
  let identity = Sym.identity
  let multiply = Sym.multiply
  let inverse = Sym.inverse
  let equal = Sym.equal
  let structure = Group_structure.Alternating N.n

  let is_even p =
    let inv_count = ref 0 in
    for i = 0 to N.n - 1 do
      for j = i + 1 to N.n - 1 do
        if p.(i) > p.(j) then Int.incr inv_count
      done
    done;
    !inv_count % 2 = 0
  ;;

  let elements = List.filter ~f:is_even Sym.elements
end
