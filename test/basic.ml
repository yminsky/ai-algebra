(* test/basic.ml *)
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
  Sequence.iter Z5.elements ~f:(printf "%d ");
  printf "\n";
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
  Sequence.iter D4.elements ~f:(fun (e : Dihedral_group.Element.t) ->
    printf "%c%d " (if e.reflection then 'S' else 'R') e.rotation);
  printf "\n";
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
  Sequence.iter S3.elements ~f:(fun p ->
    Array.iter p ~f:(printf "%d");
    printf " ");
  printf "\n";
  [%expect
    {|
    Symmetric Group S_3 (order: 6):
    012 021 102 120 201 210
  |}];
  assert (S3.order = Sequence.length S3.elements)
;;

let%expect_test "Alternating Group A_3" =
  let module A3 =
    Alternating_group.M (struct
      let n = 3
    end)
  in
  printf "Alternating Group A_3 (order: %d):\n" A3.order;
  Sequence.iter A3.elements ~f:(fun p ->
    Array.iter p ~f:(printf "%d");
    printf " ");
  printf "\n";
  [%expect {|
    Alternating Group A_3 (order: 3):
    012 120 201
    |}];
  assert (A3.order = Sequence.length A3.elements)
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
  Sequence.iter Z2xZ3.elements ~f:(fun (a, b) -> printf "(%d, %d) " a b);
  printf "\n";
  [%expect
    {|
    Direct Product Z_2 x Z_3 (order: 6):
    (0, 0) (0, 1) (0, 2) (1, 0) (1, 1) (1, 2)
  |}];
  assert (Z2xZ3.order = Sequence.length Z2xZ3.elements)
;;
