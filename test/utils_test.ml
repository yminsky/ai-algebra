open! Base
open! Stdio
open! Group_theory.Utils

let%test_unit "factorial" =
  [%test_result: int] (factorial 0) ~expect:1;
  [%test_result: int] (factorial 1) ~expect:1;
  [%test_result: int] (factorial 5) ~expect:120;
  [%test_result: int] (factorial 10) ~expect:3628800
;;

let%test_unit "reverse_subarray" =
  let test_reverse_subarray input start finish expected =
    let arr = Array.copy input in
    reverse_subarray arr start finish;
    [%test_result: int array] arr ~expect:expected
  in
  test_reverse_subarray [| 1; 2; 3; 4; 5 |] 1 3 [| 1; 4; 3; 2; 5 |];
  test_reverse_subarray [| 1; 2; 3; 4; 5 |] 0 4 [| 5; 4; 3; 2; 1 |];
  test_reverse_subarray [| 1; 2; 3; 4; 5 |] 2 2 [| 1; 2; 3; 4; 5 |]
;;

let%expect_test "factorial examples" =
  printf "factorial 0 = %d\n" (factorial 0);
  printf "factorial 1 = %d\n" (factorial 1);
  printf "factorial 5 = %d\n" (factorial 5);
  [%expect
    {|
    factorial 0 = 1
    factorial 1 = 1
    factorial 5 = 120
  |}]
;;

let%expect_test "reverse_subarray examples" =
  let print_array arr =
    Array.iter arr ~f:(fun x -> printf "%d " x);
    printf "\n"
  in
  let arr = [| 1; 2; 3; 4; 5 |] in
  printf "Original array: ";
  print_array arr;
  reverse_subarray arr 1 3;
  printf "After reverse_subarray 1 3: ";
  print_array arr;
  reverse_subarray arr 0 4;
  printf "After reverse_subarray 0 4: ";
  print_array arr;
  [%expect
    {|
    Original array: 1 2 3 4 5 
    After reverse_subarray 1 3: 1 4 3 2 5 
    After reverse_subarray 0 4: 5 2 3 4 1 
  |}]
;;
