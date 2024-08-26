(* src/dihedral_group.ml *)

open! Base
open Group

module Element = struct
  type t =
    { rotation : int
    ; reflection : bool
    }
end

type element = Element.t =
  { rotation : int
  ; reflection : bool
  }

(** Implementation of dihedral groups *)
module M (N : sig
    val n : int
  end) : GROUP with type t = Element.t = struct
  (** Elements are pairs (a, b) where a is an integer from 0 to n-1 and b is a boolean *)
  type t = Element.t

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

  let elements : Element.t Sequence.t =
    Sequence.unfold ~init:0 ~f:(fun i ->
      if i = N.n
      then None
      else
        Some
          ( Sequence.of_list
              [ { rotation = i; reflection = false }
              ; { rotation = i; reflection = true }
              ]
          , i + 1 ))
    |> Sequence.concat
  ;;

  let structure = Group_structure.Dihedral N.n
  let order = 2 * N.n
end
