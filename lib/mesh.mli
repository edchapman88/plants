type loc = int * int

val ( +> ) : loc -> int -> loc
val ( +^ ) : loc -> int -> loc

module Grid : sig
  type 'a t

  val of_arrays : 'a Array.t Array.t -> 'a t
  (** [[[|0; 1;|]; [|2;3|]]] is a [Grid.t] with [[|0; 1]] as the 'top' row, and [[|2; 3|]] as the 'bottom' row. *)

  val ( =@ ) : 'a t -> loc -> 'a
  (** [grid =@ (0,0)] is the first (left-most) item in the 'bottom' row of the grid. [grid =@ (n,0)] is the nth item in the 'bottom' row of the grid. *)

  val mapi : (loc -> 'a -> 'b) -> 'a t -> 'b t
end

type ('a, 'b) t

val at : ('a, 'b) t -> loc -> 'a

type dir =
  | Up
  | Down
  | Left
  | Right

val edge_at : ('a, 'b) t -> loc -> dir -> 'b

val of_nodes : 'a Grid.t -> ('a -> 'a -> 'b) -> ('a, 'b) t
(** [of_nodes node_grid edge_fn] is a [node edge mesh]. Where [edge_fn node_a node_b] is the edge between [node_a] and [node_b]. *)
