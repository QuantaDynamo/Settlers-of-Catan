type tile
type node
type edge
type settlement
type city
type road
type port
type intersection
type board
type resource

val tile_list : tile list
val node_list : node list
val edge_list : edge list
val test_resource : string -> resource
val init_tile : int -> resource -> int -> tile
val init_node : int -> int list -> int list -> int list -> node
val init_edge : int -> edge
val string_resource : resource -> string
val string_of_edge : edge -> string
val string_of_node : node -> string

(* val display_even: string -> string -> string -> string -> string ->
   string -> string val display_odd: string -> string -> string ->
   string -> string -> string -> string -> string *)
val display : node list -> string

val get_tile : int -> tile list -> tile
(** [get_tile ind tiles] returns the tile at the index [ind] in the list
    of tiles. Requires: [ind] is an integer in the range 0-18 and
    [tiles] is a list of type t. *)

val get_node : int -> node list -> node
(** [get_node ind nodes] returns the node at the index [ind] in the list
    of nodes. Requires: [ind] is an integer in the range 0-53 and
    [nodes] is a list of type n. *)

val get_edge : int -> edge list -> edge
(** [get_edge ind edges] returns the node at the index [ind] in the list
    of edges. Requires: [ind] is an integer in the range 0-71 and
    [edges] is a list of type e. *)

val build_road : int -> Player.player -> edge list
(** [build_road ind edge] returns the list of edges including the
    modified edge. Requires: [ind] is an integer in the range 0-71 and
    [edge] is of type e *)

val build_settlement : int -> Player.player -> node list -> node list
(** [build_settlement ind node] returns the list of nodes including the
    modified node. Requires: [ind] is an integer in the range 0-53 and
    [node] is of type n. *)

val get_resource : int -> (int * resource) list
(** [get_resource ind] returns the list of tiles correspodning to that
    number on the dice roll Requires: [ind] is an integer in the range
    2-12. *)

val draw_board : tile list -> node list -> edge list -> unit