(* src/group.mli *)

(** Abstract representation of a group *)
module type GROUP = sig
  (** Type representing group elements *)
  type t

  (** The identity element of the group *)
  val identity : t

  (** Group operation (usually denoted as multiplication) *)
  val multiply : t -> t -> t

  (** Inverse of a group element *)
  val inverse : t -> t

  (** Equality check between group elements *)
  val equal : t -> t -> bool

  (** List of all elements in the group *)
  val elements : t list

  (** Order (size) of the group *)
  val order : int

  (** Structure of the group *)
  val structure : Group_structure.t
end
