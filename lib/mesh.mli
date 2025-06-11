type ('a, 'b) t

type loc = int * int

val ( +> ) : loc -> int -> loc
val ( +^ ) : loc -> int -> loc

val at : ('a, 'b) t -> loc -> 'a

type dir =
| Up
| Down
| Left
| Right

val edge_at : ('a, 'b) t -> loc -> dir -> 'b

