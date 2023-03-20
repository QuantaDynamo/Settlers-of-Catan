type tile
type node 
type edge
type resource


val tile_list : tile list
val node_list : node list
val edge_list : edge list

val test_resource: string ->  resource

val init_tile: int -> resource -> int -> tile

val init_node: int -> node list -> edge list -> tile list -> node

val init_edge: int -> edge

val get_tile: int -> tile list -> tile
(** [get_tile ind tiles] returns the tile at the index [ind] in the list of tiles.
    Requires: [ind] is an integer in the range 0-18 and [tiles] is a list of type t. *)

val get_node: int -> node list -> node
(** [get_node ind nodes] returns the node at the index [ind] in the list of nodes.
    Requires: [ind] is an integer in the range 0-53 and [nodes] is a list of type n. *)

val get_edge: int -> edge list -> edge
(** [get_edge ind edges] returns the node at the index [ind] in the list of edges.
    Requires: [ind] is an integer in the range 0-71 and [edges] is a list of type e. *)

val build_road: int -> edge -> edge list
(** [build_road ind edge] returns the list of edges including the modified edge.
    Requires: [ind] is an integer in the range 0-71 and [edge] is of type e *)

val build_settlement: int -> node -> node list
(** [build_settlement ind node] returns the list of nodes including the modified node.
    Requires: [ind] is an integer in the range 0-53 and [node] is of type n. *)

val get_resource: int -> resource list
(** [get_resource ind] returns the list of nodes including the modified node.
    Requires: [ind] is an integer in the range 0-53 and [node] is of type n. *)