open! Base

(** Calculate the factorial of a non-negative integer *)
val factorial : int -> int

(** Reverse a subarray from index [start] to [stop] inclusive
    @param arr The input array
    @param start The starting index of the subarray (inclusive)
    @param stop The ending index of the subarray (inclusive) *)
val reverse_subarray : 'a array -> int -> int -> unit
