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
  let is_even p = Utils.count_inversions p % 2 = 0
  let elements = Sequence.filter Sym.elements ~f:is_even
end
