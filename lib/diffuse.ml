type node = {
  quant : float;
  d : float;
}

type t = (node, float) Mesh.t
(** [(node, edge) Mesh.t] *)

let grad a b = b.quant -. a.quant
let of_grid grid = Mesh.of_nodes ~edge_fn:grad grid

let diffuse mesh =
  let open Mesh in
  mapi ~edge_fn:grad
    (fun xy { quant; d } ->
      let edge_of = edge_at mesh xy in
      let delta =
        edge_of Left -. edge_of Right +. (edge_of Up -. edge_of Down)
      in
      let quant' = quant +. (-1.0 *. d *. delta) in
      { quant = quant'; d })
    mesh
