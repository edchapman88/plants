open Plants.Diffuse
open Plants.Mesh

let arrange_float_grid () = Grid.of_arrays [| [| 0.0; 1.0 |]; [| 2.0; 3.0 |] |]

let print_mesh m =
  let str_el = Printf.sprintf "%.3f" in
  print_endline (string_of_mesh ~str_el m)

let%expect_test "diffuse" =
  let m = of_grid (arrange_float_grid ()) in
  let rec loop n mesh =
    if n < 0 then ()
    else (
      print_mesh mesh;
      loop (n - 1) (diffuse ~d:0.1 mesh))
  in
  loop 10 m
