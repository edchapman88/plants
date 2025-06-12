open Plants.Diffuse
open Plants.Mesh

let arrange_node_grid () =
  Grid.mapi
    (fun _ q -> { quant = q; d = 0.1 })
    (Grid.of_arrays [| [| 0.0; 1.0 |]; [| 2.0; 3.0 |] |])

let print_mesh m =
  let str_el node = Printf.sprintf "%.3f" node.quant in
  print_endline (string_of_mesh ~str_el m)

let%expect_test "diffuse" =
  let m = of_grid (arrange_node_grid ()) in
  let rec loop n mesh =
    if n < 0 then ()
    else (
      print_mesh mesh;
      loop (n - 1) (diffuse mesh))
  in
  loop 10 m;
  [%expect {|
    0.000 1.000
    2.000 3.000

    0.600 1.200
    1.800 2.400

    0.960 1.320
    1.680 2.040

    1.176 1.392
    1.608 1.824

    1.306 1.435
    1.565 1.694

    1.383 1.461
    1.539 1.617

    1.430 1.477
    1.523 1.570

    1.458 1.486
    1.514 1.542

    1.475 1.492
    1.508 1.525

    1.485 1.495
    1.505 1.515

    1.491 1.497
    1.503 1.509
    |}]
