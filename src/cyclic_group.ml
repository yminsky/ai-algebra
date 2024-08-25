(* src/cyclic_group.ml *)

open Group

(** Implementation of cyclic groups *)
module Make (N : sig
    val n : int
  end) : GROUP with type t = int = struct
  (** Elements are integers from 0 to n - 1 *)
  type t = int

  let order = N.n
  let identity = 0
  let multiply a b = (a + b) mod N.n
  let inverse a = (N.n - a) mod N.n
  let equal = Int.equal
  let elements = List.init N.n (fun i -> i)
  let structure = Group_structure.Cyclic N.n
end
