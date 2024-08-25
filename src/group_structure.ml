(* src/group_structure.ml *)
open! Base
(** Defines various group structures *)
type t =
  | Cyclic of int
  | Dihedral of int
  | Symmetric of int
  | Alternating of int
  | Unknown
[@@deriving equal, sexp]