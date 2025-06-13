open Amount
open Tiles

let soil_w_evap = 1.1

let incr ~base ~plant_mesh ~surface_w_mesh loc node =
  let open Mesh.Grid in
  if base =@ loc = Ocean then { node with quant = 0.0 }
  else
    let surface_w =
      (Mesh.at surface_w_mesh loc).quant *. SurfaceWater.surface_water_frac
    in
    let plant = (Mesh.at plant_mesh loc).quant in
    let soil_w = node.quant in

    let lost_to_plants =
      Plant.growth *. soil_w /. (soil_w +. Plant.uptake_saturation) *. plant
    in

    let rel_absorbed_from_surface =
      (plant
      +. SurfaceWater.water_infilt_saturation
         *. SurfaceWater.bare_soil_infiltration)
      /. (plant +. SurfaceWater.water_infilt_saturation)
    in

    let absorbed_from_surface = surface_w *. rel_absorbed_from_surface in
    let lost_to_evaporation = soil_w *. soil_w_evap in

    let quant =
      absorbed_from_surface -. lost_to_plants -. lost_to_evaporation
    in
    { node with quant = node.quant +. quant }
