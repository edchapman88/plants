open Amount

let surface_water_frac = 0.1
let bare_soil_infiltration = 0.15
let water_infilt_saturation = 5.0

let incr ~plant_mesh ~rain loc node =
  let plant = (Mesh.at plant_mesh loc).quant in
  let rel_loss =
    surface_water_frac
    *. (plant +. (bare_soil_infiltration *. water_infilt_saturation))
    /. (plant +. water_infilt_saturation)
  in
  let quant = rain -. (node.quant *. rel_loss) in
  { node with quant = node.quant +. quant }
