open Tiles

type ms = {
  plant : Amount.t;
  soil_w : Amount.t;
  sur_w : Amount.t;
  base : tile Mesh.Grid.t;
}

type d = {
  d_soil : float;
  d_plant : float;
  d_surface : float;
}

let d_of_tile = function
  | Soil -> { d_soil = 0.1; d_plant = 0.1; d_surface = 1.0 }
  | Ocean -> { d_soil = 100.0; d_plant = 0.0; d_surface = 100.0 }
  | Rock -> { d_soil = 0.0; d_plant = 0.0; d_surface = 100.0 }

let base (nx, ny) =
  let arr =
    Array.init ny (fun j ->
        Array.init nx (fun x ->
            if
              (j > 3 && j < 8 && x > 12 && x < 18)
              || (j > 4 && j < 7 && x > 8 && x < 18)
            then Rock
            else if j > 18 && j < 30 && x > 4 && x < 15 then Ocean
            else Soil))
  in
  Mesh.Grid.of_arrays arr

let world ~rain ~plants (nx, ny) =
  let open Mesh in
  let base = base (nx, ny) in
  let d_grid = Grid.mapi (fun _ tile -> d_of_tile tile) base in

  let plant_mesh = plants d_grid |> Amount.of_grid in
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

  { plant = plant_mesh; soil_w = soil_mesh; sur_w = surface_mesh; base }

let step ~rain meshes =
  let base = meshes.base in
  let soil_w_mesh = meshes.soil_w in
  let plant_mesh = meshes.plant in
  let surface_w_mesh = meshes.sur_w in
  let plant_map = Plant.incr ~base ~soil_w_mesh in
  let soil_map = SoilWater.incr ~base ~plant_mesh ~surface_w_mesh in
  let sur_w_map = SurfaceWater.incr ~plant_mesh ~rain in
  let plant = Amount.map plant_map plant_mesh in
  let soil_w = Amount.map soil_map soil_w_mesh in
  let sur_w = Amount.map sur_w_map surface_w_mesh in

  { meshes with plant; soil_w; sur_w }

type point = {
  qplant : float;
  qsoil_w : float;
  qsur_w : float;
  base : tile;
}

let emj_of_qs qs =
  if qs.qsoil_w >= 100000000000000000000000.0 && qs.base = Soil then "🌀"
  else if qs.qplant <= 0.0001 then
    match qs.base with
    | Soil -> "🟫"
    | Ocean -> "🟦"
    | Rock -> "🪨"
  else if qs.qplant <= 0.1 then "🌱"
  else if qs.qplant <= 1.0 then "🌿"
  else if qs.qplant <= 10.0 then "🌳"
  else "🌲"

let quants_of_ms ms =
  let open Amount in
  let open Mesh.Grid in
  Mesh.mapi
    ~edge_fn:(fun a _ -> a)
    (fun xy node ->
      {
        qplant = node.quant;
        qsur_w = (Mesh.at ms.sur_w xy).quant;
        qsoil_w = (Mesh.at ms.soil_w xy).quant;
        base = ms.base =@ xy;
      })
    ms.plant

let rec sim ~rain n meshes =
  Unix.sleepf 0.1;
  if n <= 0 then ()
  else
    (* let str_el Amount.{ quant; _ } = Printf.sprintf "%.2f" quant in *)
    let quants = quants_of_ms meshes in
    Mesh.string_of_mesh ~str_el:emj_of_qs quants |> print_endline;
    sim ~rain (n - 1) (step ~rain:(rain n) meshes)
