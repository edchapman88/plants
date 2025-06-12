type ms = {
  plant : Amount.t;
  soil_w : Amount.t;
  sur_w : Amount.t;
}

type tile = Soil

type d = {
  d_soil : float;
  d_plant : float;
  d_surface : float;
}

let d_of_tile = function
  | Soil -> { d_soil = 0.1; d_plant = 0.1; d_surface = 10.0 }

let base (nx, ny) =
  Mesh.Grid.of_arrays (Array.init ny (fun _ -> Array.make nx Soil))

let world ~rain (nx, ny) =
  let open Mesh in
  let d_grid = Grid.mapi (fun _ tile -> d_of_tile tile) (base (nx, ny)) in

  let plant_mesh =
    Grid.mapi
      (fun _ { d_plant; _ } -> Amount.{ quant = Random.float 1.0; d = d_plant })
      d_grid
    |> Amount.of_grid
  in

  let soil_mesh =
    Grid.mapi
      (fun _ { d_soil; _ } ->
        Amount.{ quant = rain /. SoilWater.soil_w_evap; d = d_soil })
      d_grid
    |> Amount.of_grid
  in

  let surface_mesh =
    Grid.mapi
      (fun _ { d_surface; _ } ->
        Amount.
          {
            quant =
              rain
              /. (SurfaceWater.surface_water_frac
                *. SurfaceWater.bare_soil_infiltration);
            d = d_surface;
          })
      d_grid
    |> Amount.of_grid
  in

  { plant = plant_mesh; soil_w = soil_mesh; sur_w = surface_mesh }

let step ~rain meshes =
  let soil_w_mesh = meshes.soil_w in
  let plant_mesh = meshes.plant in
  let surface_w_mesh = meshes.sur_w in
  let plant_map = Plant.incr ~soil_w_mesh in
  let soil_map = SoilWater.incr ~plant_mesh ~surface_w_mesh in
  let sur_w_map = SurfaceWater.incr ~plant_mesh ~rain in
  let plant = Amount.map plant_map plant_mesh in
  let soil_w = Amount.map soil_map soil_w_mesh in
  let sur_w = Amount.map sur_w_map surface_w_mesh in

  { plant; soil_w; sur_w }

let emj_of_q q =
  if q <= 0.0001 then "🟫"
  else if q <= 0.1 then "🌱"
  else if q <= 1.0 then "🌿"
  else if q <= 10.0 then "🌳"
  else "🌲"

let rec sim ~rain n meshes =
  if n <= 0 then ()
  else
    (* let str_el Amount.{ quant; _ } = Printf.sprintf "%.2f" quant in *)
    let str_el Amount.{ quant; _ } = emj_of_q quant in
    Mesh.string_of_mesh ~str_el meshes.plant |> print_endline;
    sim ~rain (n - 1) (step ~rain meshes)
