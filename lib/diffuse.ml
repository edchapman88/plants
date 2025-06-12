type t = (float, float) Mesh.t

(* let linear m c x = (m *. float_of_int x) +. c *)
(* let w_sat = 128 *)
(* let perm = 1.0/.32.0 *)
(* let k_unsat = linear 0.5 0.0 *)
(* let k_sat = linear 1.0 0.0 *)

(* let pressure_of_w w = if w < w_sat then k_unsat w else k_sat w *)
(* let pressure_grad wa wb = pressure_of_w wb -. pressure_of_w wa *)
(* let flux ~perm wa wb = perm *. (pressure_grad wa wb) *)

let grad a b = b -. a
let of_grid grid = Mesh.of_nodes ~edge_fn:grad grid

let diffuse ~d mesh =
  let open Mesh in
  mapi ~edge_fn:grad
    (fun xy node ->
      let edge_of = edge_at mesh xy in
      let delta =
        edge_of Left -. edge_of Right +. (edge_of Up -. edge_of Down)
      in
      node +. (-1.0 *. d *. delta))
    mesh
