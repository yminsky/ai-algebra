(* src/group_structure.ml *)
open! Base
(** Defines various group structures *)
type t =
  | Cyclic of int
  | Dihedral of int
  | Symmetric of int
  | Alternating of int
  | Direct_product of t * t
  | Unknown
[@@deriving equal, sexp]

(** Create a direct product of two group structures *)
let cross_product g1 g2 =
  Direct_product (g1, g2)
