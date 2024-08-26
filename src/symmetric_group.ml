(* src/symmetric_group.ml *)
open! Base

(** Implementation of symmetric groups *)
type t = int array
(** Permutations represented as arrays *)

module M (N : sig
    val n : int
  end) : Group.GROUP with type t = t = struct
  type nonrec t = t

  let order = Utils.factorial N.n
  let identity = Array.init N.n ~f:Fn.id

  (** Generates the next permutation in lexicographic order *)
  let next_permutation arr =
    let n = Array.length arr in
    let k = ref (n - 2) in
    while !k >= 0 && arr.(!k) >= arr.(!k + 1) do
      k := !k - 1
    done;
    if !k < 0
    then None
    else (
      let l = ref (n - 1) in
      while arr.(!k) >= arr.(!l) do
        l := !l - 1
      done;
      Array.swap arr !k !l;
      Utils.reverse_subarray arr (!k + 1) (n - 1);
      Some arr)
  ;;

  (** Generates all permutations using Lexicographic Order Algorithm *)
  let elements =
    let initial = Array.init N.n ~f:Fn.id in
    Sequence.unfold ~init:(Some initial) ~f:(function
      | None -> None
      | Some perm ->
        let next = next_permutation (Array.copy perm) in
        Some (perm, next))
  ;;

  let multiply a b = Array.init N.n ~f:(fun i -> a.(b.(i)))

  let inverse p =
    let inv = Array.create ~len:N.n 0 in
    Array.iteri p ~f:(fun i v -> inv.(v) <- i);
    inv
  ;;

  let equal a b = Array.equal Int.equal a b
  let structure = Group_structure.Symmetric N.n
end
