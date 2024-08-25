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

  (** Reverse a subarray from index [start] to [stop] inclusive *)
  let reverse_subarray arr start stop =
    let rec helper i j =
      if i < j
      then (
        Array.swap arr i j;
        helper (i + 1) (j - 1))
    in
    helper start stop
  ;;

  (** Generates all permutations using Lexicographic Order Algorithm *)
  let elements =
    let initial = Array.init N.n ~f:Fn.id in
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
        reverse_subarray arr (!k + 1) (n - 1);
        Some (Array.copy arr))
    in
    Sequence.unfold_step ~init:(Some identity) ~f:(function
      | None -> Sequence.Step.Done
      | Some perm ->
        Sequence.Step.Yield (perm, next_permutation (Array.copy perm)))
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
