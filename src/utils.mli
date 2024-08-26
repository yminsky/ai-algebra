open! Base

(** Calculate the factorial of a non-negative integer *)
val factorial : int -> int

(** Reverse a subarray from index [start] to [stop] inclusive
    @param arr The input array
    @param start The starting index of the subarray (inclusive)
    @param stop The ending index of the subarray (inclusive) *)
val reverse_subarray : 'a array -> int -> int -> unit

(** Calculate the parity of a permutation using inversion count

    This function counts the number of inversions in a given permutation.
    An inversion is a pair of elements (i, j) where i < j, but i appears
    after j in the permutation.

    The algorithm uses a modified merge sort approach with O(n log n) time complexity.

    @param arr The input array representing the permutation
    @return The total number of inversions in the permutation *)
val count_inversions : int array -> int
