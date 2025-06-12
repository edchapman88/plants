type loc = int * int

let ( +> ) (x, y) n = (x + n, y)
let ( +^ ) (x, y) n = (x, y + n)

module Grid : sig
  type 'a t

  val of_arrays : 'a Array.t Array.t -> 'a t
  val ( =@ ) : 'a t -> loc -> 'a
  val mapi : (loc -> 'a -> 'b) -> 'a t -> 'b t
end = struct
  type 'a t = 'a Array.t Array.t

  (** The 'top' row first in memory. *)
  let of_arrays arrs = arrs

  let ( =@ ) grid (rawx, rawy) =
    let rec make_pos z ~period =
      if z >= 0 then z else make_pos (z + period) ~period
    in
    let height = Array.length grid in
    let width = Array.length grid.(0) in
    let x = make_pos ~period:width rawx mod width in
    let y = make_pos ~period:height rawy mod height in

    grid.(height - 1 - y).(x)

  let mapi f grid =
    let height = Array.length grid in
    Array.mapi
      (fun j row ->
        Array.mapi
          (fun x el ->
            let y = height - 1 - j in
            f (x, y) el)
          row)
      grid
end

type 'b edge_pair = {
  right : 'b;
  down : 'b;
}

type ('a, 'b) t = {
  nodes : 'a Grid.t;
  edges : 'b edge_pair Grid.t;
}

let at mesh xy = Grid.(mesh.nodes =@ xy)

type dir =
  | Up
  | Down
  | Left
  | Right

let edge_at mesh xy d =
  let open Grid in
  match d with
  | Right -> (mesh.edges =@ xy).right
  | Down -> (mesh.edges =@ xy).down
  | Up -> (mesh.edges =@ xy +^ 1).down
  | Left -> (mesh.edges =@ xy +> -1).right

let of_nodes nodes edge_of_nodes =
  let open Grid in
  let edges =
    nodes
    |> Grid.mapi (fun xy node ->
           let to_right = nodes =@ xy +> 1 in
           let below = nodes =@ xy +^ -1 in
           {
             right = edge_of_nodes node to_right;
             down = edge_of_nodes node below;
           })
  in
  { nodes; edges }
