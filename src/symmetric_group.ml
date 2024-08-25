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
  let multiply a b = Array.init N.n ~f:(fun i -> a.(b.(i)))

  let inverse p =
    let inv = Array.init N.n ~f:Fn.id in
    Array.iteri ~f:(fun i pi -> inv.(pi) <- i) p;
    inv
  ;;

  let structure = Group_structure.Symmetric N.n
  let equal a b = Array.equal Int.( = ) a b

  let next_permutation perm =
    let n = Array.length perm in
    let rec find_k i =
      if i < 0
      then None
      else if perm.(i) < perm.(i + 1)
      then Some i
      else find_k (i - 1)
    in
    match find_k (n - 2) with
    | None ->
      Array.init n ~f:Fn.id (* Return identity if it's the last permutation *)
    | Some k ->
      let l =
        Array.findi_exn ~f:(fun i x -> i > k && x > perm.(k)) perm |> fst
      in
      Array.swap perm k l;
      (* Reverse the subarray from k+1 to the end *)
      for i = 0 to (n - k - 2) / 2 do
        Array.swap perm (k + 1 + i) (n - 1 - i)
      done;
      perm
  ;;

  let elements =
    Sequence.unfold_step ~init:identity ~f:(fun perm ->
      let next = next_permutation perm in
      if equal next identity
      then Sequence.Step.Done
      else Sequence.Step.Yield { value = perm; state = next })
  ;;
end
