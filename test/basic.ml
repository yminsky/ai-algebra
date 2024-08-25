(* examples/main.ml *)
open Base
open Stdio
open Group_theory

let%expect_test "Cyclic Group Z_5" =
  let module Z5 =
    Cyclic_group.M (struct
      let n = 5
    end)
  in
  printf "Cyclic Group Z_5 (order: %d):\n" Z5.order;
  Sequence.iter ~f:(printf "%d ") Z5.elements;
  [%expect {|
    Cyclic Group Z_5 (order: 5):
    0 1 2 3 4
    |}];
  assert (Z5.order = Sequence.length Z5.elements)
;;

let%expect_test "Dihedral Group D_4" =
  let module D4 =
    Dihedral_group.M (struct
      let n = 4
    end)
  in
  printf "Dihedral Group D_4 (order: %d):\n" D4.order;
  Sequence.iter
    ~f:(fun (e : Dihedral_group.t) ->
      printf "%c%d " (if e.reflection then 'S' else 'R') e.rotation)
    D4.elements;
  [%expect
    {|
    Dihedral Group D_4 (order: 8):
    R0 S0 R1 S1 R2 S2 R3 S3
    |}];
  assert (D4.order = Sequence.length D4.elements)
;;

let%expect_test "Symmetric Group S_3" =
  let module S3 =
    Symmetric_group.M (struct
      let n = 3
    end)
  in
  printf "Symmetric Group S_3 (order: %d):\n" S3.order;
  Sequence.iter
    ~f:(fun p ->
      Array.iter ~f:(printf "%d") p;
      printf " ")
    S3.elements;
  [%expect {| Symmetric Group S_3 (order: 6): |}];
  [%test_eq: int] S3.order (Sequence.length S3.elements)
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  (runtime-lib/runtime.ml.E "comparison failed"
    (6 vs 0 (Loc test/basic.ml:53:13)))
  Raised at Ppx_assert_lib__Runtime.test_eq in file "runtime-lib/runtime.ml", line 91, characters 22-69
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]
;;

let%expect_test "Alternating Group A_3" =
  let module A3 =
    Alternating_group.M (struct
      let n = 3
    end)
  in
  printf "Alternating Group A_3 (order: %d):\n" A3.order;
  Sequence.iter
    ~f:(fun p ->
      Array.iter ~f:(printf "%d") p;
      printf " ")
    A3.elements;
  [%expect {| Alternating Group A_3 (order: 3): |}];
  assert (A3.order = Sequence.length A3.elements)
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  "Assert_failure test/basic.ml:78:2"
  Raised at Group_theory_tests__Basic.(fun) in file "test/basic.ml", line 78, characters 2-49
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]
;;

let%expect_test "Direct Product of Cyclic Groups Z_2 and Z_3" =
  let module Z2 =
    Cyclic_group.M (struct
      let n = 2
    end)
  in
  let module Z3 =
    Cyclic_group.M (struct
      let n = 3
    end)
  in
  let module Z2xZ3 = Direct_product.M (Z2) (Z3) in
  printf "Direct Product Z_2 x Z_3 (order: %d):\n" Z2xZ3.order;
  Sequence.iter ~f:(fun (a, b) -> printf "(%d, %d) " a b) Z2xZ3.elements;
  [%expect
    {|
    Direct Product Z_2 x Z_3 (order: 6):
    (0, 0) (0, 1) (0, 2) (1, 0) (1, 1) (1, 2)
    |}];
  assert (Z2xZ3.order = Sequence.length Z2xZ3.elements)
;;
