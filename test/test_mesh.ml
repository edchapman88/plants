open Plants.Mesh

let arrange_grid () =
  let nodes = Grid.of_arrays [| [| 0; 1 |]; [| 2; 3 |] |] in
  let edge_fn = ( + ) in
  of_nodes nodes edge_fn

let%expect_test "create mesh" =
  let m = arrange_grid () in
  Printf.printf "top-left = %d\n" (at m (0, 1));
  Printf.printf "top-right = %d\n" (at m (1, 1));
  Printf.printf "bottom-left = %d\n" (at m (0, 0));
  Printf.printf "bottom-right = %d\n" (at m (1, 0));
  [%expect
    {|
    top-left = 0
    top-right = 1
    bottom-left = 2
    bottom-right = 3
    |}]

let%expect_test "grid =@ with wrapping" =
  let m = arrange_grid () in
  Printf.printf "(-1, 0) = %d\n" (at m (-1, 0));
  Printf.printf "(-2, 0) = %d\n" (at m (-2, 0));
  Printf.printf "(3, 0) = %d\n" (at m (3, 0));
  Printf.printf "(4, 0) = %d\n" (at m (4, 0));

  Printf.printf "(0, -1) = %d\n" (at m (0, -1));
  Printf.printf "(0, -2) = %d\n" (at m (0, -2));
  Printf.printf "(0, 3) = %d\n" (at m (0, 3));
  Printf.printf "(0, 4) = %d\n" (at m (0, 4));
  [%expect
    {|
    (-1, 0) = 3
    (-2, 0) = 2
    (3, 0) = 3
    (4, 0) = 2
    (0, -1) = 0
    (0, -2) = 2
    (0, 3) = 0
    (0, 4) = 2
    |}]

let%expect_test "edge evaluation and edge_at" =
  let m = arrange_grid () in
  Printf.printf "edge below top-left = %d\n" (edge_at m (0, 1) Down);
  Printf.printf "edge right of top-left = %d\n" (edge_at m (0, 1) Right);
  Printf.printf "edge left of top-right = %d\n" (edge_at m (1, 1) Left);
  Printf.printf "edge above of bottom-right = %d\n" (edge_at m (1, 0) Up);
  Printf.printf "edge right of bottom-right = %d\n" (edge_at m (1, 0) Right);
  Printf.printf "edge left of bottom-left = %d\n" (edge_at m (0, 0) Left);

  [%expect
    {|
    edge below top-left = 2
    edge right of top-left = 1
    edge left of top-right = 1
    edge above of bottom-right = 4
    edge right of bottom-right = 5
    edge left of bottom-left = 5
    |}]
