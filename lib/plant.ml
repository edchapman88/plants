open Amount

let grazing_loss = 0.3
let uptake = 10.0
let uptake_saturation = 3.0
let growth = 0.05
let senescence = 0.1

let incr ~soil_w_mesh loc node =
  let soil_w = (Mesh.at soil_w_mesh loc).quant in
  let relative_growth =
    growth *. uptake *. soil_w /. (soil_w +. uptake_saturation)
  in
  let relative_loss = senescence +. grazing_loss in
  let quant = (relative_growth -. relative_loss) *. node.quant in
  { node with quant = node.quant +. quant }
