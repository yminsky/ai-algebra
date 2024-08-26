(* src/cyclic_group.mli *)

open! Base
open Group

(** Implementation of cyclic groups *)
module M (_ : sig
    val n : int
  end) : GROUP with type t = int
