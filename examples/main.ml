(* examples/main.ml *)

open Group_theory

let%expect_test "Cyclic Group Z_5" =
  let module Z5 =
    Cyclic_group.Make (struct
      let n = 5
    end)
  in
  Printf.printf "Cyclic Group Z_5 (order: %d):\n" Z5.order;
  List.iter (Printf.printf "%d ") Z5.elements;
  [%expect {|
    Cyclic Group Z_5 (order: 5):
    0 1 2 3 4 |}]
;;

let%expect_test "Dihedral Group D_4" =
  let module D4 =
    Dihedral_group.Make (struct
      let n = 4
    end)
  in
  Printf.printf "Dihedral Group D_4 (order: %d):\n" D4.order;
  List.iter
    (fun (e : Dihedral_group.t) ->
      Printf.printf "%c%d " (if e.reflection then 'S' else 'R') e.rotation)
    D4.elements;
  [%expect {|
    Dihedral Group D_4 (order: 8):
    R0 S0 R1 S1 R2 S2 R3 S3
    |}]
;;

let%expect_test "Symmetric Group S_3" =
  let module S3 =
    Symmetric_group.Make (struct
      let n = 3
    end)
  in
  Printf.printf "Symmetric Group S_3 (order: %d):\n" S3.order;
  List.iter
    (fun p ->
      Array.iter (Printf.printf "%d") p;
      Printf.printf " ")
    S3.elements;
  [%expect
    {|
    Symmetric Group S_3 (order: 6):
    210 120 210 210 120 210 210 120 210 012 102 012
    |}]
;;

let%expect_test "Alternating Group A_3" =
  let module A3 =
    Alternating_group.Make (struct
      let n = 3
    end)
  in
  Printf.printf "Alternating Group A_3 (order: %d):\n" A3.order;
  List.iter
    (fun p ->
      Array.iter (Printf.printf "%d") p;
      Printf.printf " ")
    A3.elements;
  [%expect {|
    Alternating Group A_3 (order: 3):
    120 120 120 012 012
    |}]
;;
