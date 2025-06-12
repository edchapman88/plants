open Plants.Sim

let () =
  let rain = 0.01 in
  let meshes = world ~rain (20, 20) in
  sim ~rain 50 meshes
