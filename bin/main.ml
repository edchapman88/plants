open Plants
open Plants.Sim

let rain ~scale n =
  let years = n / 365 in
  let months = n / 30 in
  let drought_scale = if years mod 3 = 1 then 0.8 else 1.0 in
  drought_scale *. scale
  *.
  match months mod 12 with
  | 5 -> 1.0
  | 6 -> 1.3
  | _ -> 0.0000001

let init_plants () =
  Mesh.Grid.mapi (fun _ { d_plant; _ } ->
      let quant = if Random.float 1.0 < 0.1 then 0.1 else 0.0 in
      Amount.{ quant; d = d_plant })

let () =
  Random.init 0;
  let rain = rain ~scale:0.000000000001 in
  let meshes = world ~rain:(rain 5) ~plants:(init_plants ()) (40, 40) in
  sim ~rain 1000 meshes
