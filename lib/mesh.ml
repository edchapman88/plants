
type loc = int * int

let ( +> ) (x,y) n = (x+n,y)
let ( +^ ) (x,y) n = (x,y+n)

type dir =
| Up
| Down
| Left
| Right


module Grid : sig
  type 'a t
  val at : 'a t -> loc -> 'a

end = struct
  type 'a t = 'a Array.t Array.t
  
  let at grid (rawx,rawy) =
          let rec make_pos z ~period =
                  if z >= 0 then z else make_pos (z+period) ~period in
          let height = Array.length grid in
          let width = Array.length grid.(0) in
          let x = (make_pos ~period:width rawx) mod width in
          let y = (make_pos ~period:height rawy) mod height in
          grid.(y).(x)
end

type 'b edge_pair = { right: 'b; down: 'b }

type ('a, 'b) t = { nodes: 'a Grid.t; edges: 'b edge_pair Grid.t }

let at mesh xy = Grid.at mesh.nodes xy

let edge_at mesh xy d =
        match d with
        | Right -> 
                (Grid.at mesh.edges xy).right
        | Down ->
                (Grid.at mesh.edges xy).down
        | Up ->
                (Grid.at mesh.edges (xy +^ 1)).down
        | Left ->
                (Grid.at mesh.edges (xy +> -1)).right

