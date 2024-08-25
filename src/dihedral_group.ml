(* src/dihedral_group.ml *)

open! Base
open Group

type t =
  { rotation : int
  ; reflection : bool
  }

(** Implementation of dihedral groups *)
module Make (N : sig
    val n : int
  end) : GROUP with type t = t = struct
  (** Elements are pairs (a, b) where a is an integer from 0 to n-1 and b is a boolean *)
  type nonrec t = t

  let identity = { rotation = 0; reflection = false }

  let multiply a b =
    { rotation = (a.rotation + b.rotation) % N.n
    ; reflection = Bool.( <> ) a.reflection b.reflection
    }
  ;;

  let inverse a =
    { rotation = (N.n - a.rotation) % N.n; reflection = a.reflection }
  ;;

  let equal a b =
    Int.( = ) a.rotation b.rotation && Bool.( = ) a.reflection b.reflection
  ;;

  let elements =
    let rec loop i =
      if i = N.n
      then []
      else
        { rotation = i; reflection = false }
        :: { rotation = i; reflection = true }
        :: loop (i + 1)
    in
    loop 0
  ;;

  let structure = Group_structure.Dihedral N.n
  let order = 2 * N.n
end
