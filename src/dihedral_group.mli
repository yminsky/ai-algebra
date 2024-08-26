(** Dihedral group implementation *)

open! Base
open Group

(** Elements of the dihedral group *)
module Element : sig
  type t = private
    { rotation : int (** Rotation component (0 to n-1) *)
    ; reflection : bool (** Reflection component *)
    }
end

(** Functor to create a dihedral group of order 2n *)
module M (_ : sig
    val n : int
  end) : GROUP with type t = Element.t
