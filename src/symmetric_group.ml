(* src/symmetric_group.ml *)
open! Base

(** Implementation of symmetric groups *)
type t = int array
(** Permutations represented as arrays *)

module Make (N : sig
    val n : int
  end) : Group.GROUP with type t = t = struct
  type nonrec t = t

  let order = Utils.factorial N.n
  let identity = Array.init N.n ~f:Fn.id
  let multiply a b = Array.init N.n ~f:(fun i -> a.(b.(i)))

  let inverse p =
    let inv = Array.init N.n ~f:Fn.id in
    Array.iteri ~f:(fun i pi -> inv.(pi) <- i) p;
    inv
  ;;

  let structure = Group_structure.Symmetric N.n
  let equal a b = Array.equal Int.( = ) a b

  let elements =
    let rec heap_permute k arr acc =
      if k = 1
      then Array.copy arr :: acc
      else (
        let acc = heap_permute (k - 1) arr acc in
        List.fold (List.init k ~f:Fn.id) ~init:acc ~f:(fun acc i ->
          Array.swap arr (if k % 2 = 0 then i else 0) (k - 1);
          let new_acc = heap_permute (k - 1) arr acc in
          Array.swap arr (if k % 2 = 0 then i else 0) (k - 1);
          new_acc))
    in
    heap_permute N.n (Array.init N.n ~f:Fn.id) []
  ;;
end
