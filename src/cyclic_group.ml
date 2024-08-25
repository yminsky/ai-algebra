(* src/cyclic_group.ml *)

open! Base
open Group

(** Implementation of cyclic groups *)
module Make (N : sig
    val n : int
  end) : GROUP with type t = int = struct
  (** Elements are integers from 0 to n - 1 *)
  type t = int

  let order = N.n
  let identity = 0
  let multiply a b = (a + b) % N.n
  let inverse a = (N.n - a) % N.n
  let equal = Int.( = )
  let elements = List.init N.n ~f:Fn.id
  let structure = Group_structure.Cyclic N.n
end
