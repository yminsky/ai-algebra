open! Base
open Group

module M (G1 : GROUP) (G2 : GROUP) : GROUP with type t = G1.t * G2.t = struct
  type t = G1.t * G2.t

  let identity = G1.identity, G2.identity
  let equal (a1, a2) (b1, b2) = G1.equal a1 b1 && G2.equal a2 b2
  let multiply (a1, a2) (b1, b2) = G1.multiply a1 b1, G2.multiply a2 b2
  let inverse (a1, a2) = G1.inverse a1, G2.inverse a2
  let structure = Group_structure.cross_product G1.structure G2.structure
  let elements = Sequence.cartesian_product G1.elements G2.elements
  let order = G1.order * G2.order
end
