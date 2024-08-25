(* examples/main.ml *)

open Printf
open Group_theory

let () =
  (* Cyclic Group Z_5 *)
  let module Z5 =
    Cyclic_group.Make (struct
      let n = 5
    end)
  in
  printf "Cyclic Group Z_5 (order: %d):\n" Z5.order;
  List.iter (fun e -> printf "%d " e) Z5.elements;
  printf "\n\n";
  (* Dihedral Group D_4 *)
  let module D4 =
    Dihedral_group.Make (struct
      let n = 4
    end)
  in
  printf "Dihedral Group D_4 (order: %d):\n" D4.order;
  List.iter
    (fun (e : Dihedral_group.t) ->
      printf "%c%d " (if e.reflection then 'S' else 'R') e.rotation)
    D4.elements;
  printf "\n\n";
  (* Symmetric Group S_3 *)
  let module S3 =
    Symmetric_group.Make (struct
      let n = 3
    end)
  in
  printf "Symmetric Group S_3 (order: %d):\n" S3.order;
  List.iter
    (fun p ->
      Array.iter (printf "%d") p;
      printf " ")
    S3.elements;
  printf "\n\n";
  (* Alternating Group A_3 *)
  let module A3 =
    Alternating_group.Make (struct
      let n = 3
    end)
  in
  printf "Alternating Group A_3 (order: %d):\n" A3.order;
  List.iter
    (fun p ->
      Array.iter (printf "%d") p;
      printf " ")
    A3.elements;
  printf "\n"
;;
