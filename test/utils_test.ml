open! Base
open! Stdio
open! Group_theory

let%expect_test "factorial" =
  let fact n = printf "%d\n" (Utils.factorial n) in
  fact 0;
  [%expect {| 1 |}];
  fact 1;
  [%expect {| 1 |}];
  fact 5;
  [%expect {| 120 |}];
  fact 10;
  [%expect {| 3628800 |}];
  fact 15;
  [%expect {| 1307674368000 |}];
  fact 8;
  [%expect {| 40320 |}];
  (* int overflow!*)
  fact 21;
  [%expect {| -4249290049419214848 |}]
;;

let%expect_test "reverse_subarray" =
  let test_reverse_subarray length start finish =
    let arr = Array.init length ~f:Int.succ in
    let original = Array.copy arr in
    let range_marker =
      Array.init length ~f:(fun i ->
        if i >= start && i <= finish then '#' else '-')
    in
    Utils.reverse_subarray arr start finish;
    print_s [%sexp (original : int array)];
    print_s [%sexp (range_marker : char array)];
    print_s [%sexp (arr : int array)]
  in
  test_reverse_subarray 5 1 3;
  [%expect {|
    (1 2 3 4 5)
    (- # # # -)
    (1 4 3 2 5)
    |}];
  test_reverse_subarray 5 0 4;
  [%expect {|
    (1 2 3 4 5)
    (# # # # #)
    (5 4 3 2 1)
    |}];
  test_reverse_subarray 5 2 2;
  [%expect {|
    (1 2 3 4 5)
    (- - # - -)
    (1 2 3 4 5)
    |}];
  test_reverse_subarray 5 0 3;
  [%expect {|
    (1 2 3 4 5)
    (# # # # -)
    (4 3 2 1 5)
    |}];
  (* an example where the subarray is empty *)
  test_reverse_subarray 5 2 1;
  [%expect {|
    (1 2 3 4 5)
    (- - - - -)
    (1 2 3 4 5)
    |}]
;;

let%expect_test "count_inversions" =
  let test arr =
    let inversions = Utils.count_inversions arr in
    print_s [%sexp (inversions : int)]
  in
  test [||];
  [%expect {| 0 |}];
  test [| 1 |];
  [%expect {| 0 |}];
  test [| 1; 2; 3; 4; 5 |];
  [%expect {| 0 |}];
  test [| 5; 4; 3; 2; 1 |];
  [%expect {| 10 |}];
  test [| 2; 4; 1; 3; 5 |];
  [%expect {| 3 |}];
  test [| 1; 1; 1; 1 |];
  [%expect {| 0 |}];
  test (Array.init 10 ~f:(fun i -> 10 - i));
  [%expect {| 45 |}]
;;
