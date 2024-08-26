(** Direct product of groups *)

open! Base
open Group

(** Functor to create a direct product of two groups.

    The direct product of two groups G and H is a new group G × H where:
    - Elements are ordered pairs (g, h) with g in G and h in H
    - The group operation is defined component-wise: (g1, h1) * (g2, h2) = (g1 * g2, h1 * h2)
    - The identity element is (e_G, e_H) where e_G and e_H are identities of G and H respectively
    - The inverse of (g, h) is (g^-1, h^-1)

    The order of G × H is the product of the orders of G and H. *)
module M (G1 : GROUP) (G2 : GROUP) : GROUP with type t = G1.t * G2.t
