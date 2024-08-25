(* src/symmetric_group.ml *)

open Utils

(** Implementation of symmetric groups *)
type t = int array
(** Permutations represented as arrays *)

module Make (N : sig
    val n : int
  end) : Group.GROUP with type t = t = struct
  type nonrec t = t

  let order = factorial N.n
  let identity = Array.init N.n (fun i -> i)
  let multiply a b = Array.init N.n (fun i -> a.(b.(i)))

  let inverse p =
    let inv = Array.make N.n 0 in
    Array.iteri (fun i pi -> inv.(pi) <- i) p;
    inv
  ;;

  let structure = Group_structure.Symmetric N.n
  let equal a b = Array.for_all2 ( = ) a b

  let elements =
    let rec heap_permute k arr acc =
      if k = 1
      then Array.copy arr :: acc
      else (
        let acc = heap_permute (k - 1) arr acc in
        let acc =
          List.fold_left
            (fun acc i ->
              swap arr (if k mod 2 = 0 then i else 0) (k - 1);
              heap_permute (k - 1) arr acc)
            acc
            (List.init (k - 1) Fun.id)
        in
        acc)
    in
    heap_permute N.n (Array.init N.n (fun i -> i)) []
  ;;
end
