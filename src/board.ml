open Player

type resource = Wheat | Sheep | Brick | Wood | Ore | Desert

let test_resource res = 
  match res with
  | "Wheat" -> Wheat
  | "Sheep" -> Sheep
  | "Brick" -> Brick
  | "Wood" -> Wood
  | "Ore" -> Ore
  | "Desert" -> Desert
  | _ -> Wheat

let string_resource res = 
  match res with
  | Wheat -> "Wheat"
  | Desert -> "Desert"
  | Sheep -> "Sheep"
  | Brick -> "Brick"
  | Wood -> "Wood"
  | Ore -> "Ore"

type port = { port_resource : resource; ratio : int }

let string_of_port p = Printf.sprintf "{port_resource=%s; ratio=%d;}" (string_resource p.port_resource) p.ratio

type tile = {
  tile_id : int;
  resource : resource;
  dice_num : int;
  has_robber : bool;
}

let string_of_tile t = Printf.sprintf "{tile_id=%d; resource=%s; dice_num=%d; has_robber=%b; \n}"
t.tile_id (string_resource t.resource) t.dice_num t.has_robber

let init_tile i res dice =
  { tile_id = i; resource = res; dice_num = dice; has_robber = false }

type edge = { edge_id : int; has_road : bool; owner : player option }

let string_of_edge e = Printf.sprintf "{edge_id=%d; has_road=%b; owner=%s; \n}"
e.edge_id e.has_road (match e.owner with | Some i -> string_of_player i | None -> "no owner")

let init_edge i = { edge_id = i; has_road = false; owner = None }

type node = {
  node_id : int;
  adj_nodes : node list;
  adj_edges : edge list;
  adj_tiles : tile list;
  port : port;
  has_settlement : bool;
  owner : player option;
}

let string_of_adj_node n = Printf.sprintf "{node_id=%d; port=%s; has_settlement=%b; owner=%s;}" 
n.node_id "d" n.has_settlement (match n.owner with | Some i -> string_of_player i | None -> "no owner")
let adj_nodes_to_string = fun l -> String.concat "; " (List.map string_of_adj_node l)
let adj_edges_to_string = fun l -> String.concat "; " (List.map string_of_edge l)
let adj_tiles_to_string = fun l -> String.concat "; " (List.map string_of_tile l)
let string_of_node n = Printf.sprintf "{node_id=%d; adj_nodes=%s; adj_edges=%s; adj_tiles=%s; port=%s; has_settlement=%b; owner=%s; \n}"
n.node_id (adj_nodes_to_string n.adj_nodes) (adj_edges_to_string n.adj_edges) (adj_tiles_to_string n.adj_tiles) (string_of_port n.port)
n.has_settlement (match n.owner with | Some i -> string_of_player i | None -> "no owner")

let init_node i node_list edge_list tile_list =
  {
    node_id = i;
    adj_nodes = node_list;
    adj_edges = edge_list;
    adj_tiles = tile_list;
    port = { port_resource = Desert; ratio = 3 };
    has_settlement = false;
    owner = None;
  }

let tile_list =
  [
    init_tile 0 Wheat 2;
    init_tile 1 Wheat 3;
    init_tile 2 Wheat 3;
    init_tile 3 Wheat 4;
    init_tile 4 Ore 4;
    init_tile 5 Sheep 5;
    init_tile 6 Sheep 5;
    init_tile 7 Sheep 6;
    init_tile 8 Sheep 6;
    init_tile 9 Wood 8;
    init_tile 10 Wood 8;
    init_tile 11 Wood 9;
    init_tile 12 Wood 9;
    init_tile 13 Brick 10;
    init_tile 14 Brick 10;
    init_tile 15 Brick 11;
    init_tile 16 Ore 11;
    init_tile 17 Ore 12;
    init_tile 18 Desert 0;
  ]

let edge_list =
  [
    init_edge 0;
    init_edge 1;
    init_edge 2;
    init_edge 3;
    init_edge 4;
    init_edge 5;
    init_edge 6;
    init_edge 7;
    init_edge 8;
    init_edge 9;
    init_edge 10;
    init_edge 11;
    init_edge 12;
    init_edge 13;
    init_edge 14;
    init_edge 15;
    init_edge 16;
    init_edge 17;
    init_edge 18;
    init_edge 19;
    init_edge 20;
    init_edge 21;
    init_edge 22;
    init_edge 23;
    init_edge 24;
    init_edge 25;
    init_edge 26;
    init_edge 27;
    init_edge 28;
    init_edge 29;
    init_edge 30;
    init_edge 31;
    init_edge 32;
    init_edge 33;
    init_edge 34;
    init_edge 35;
    init_edge 36;
    init_edge 37;
    init_edge 38;
    init_edge 39;
    init_edge 40;
    init_edge 41;
    init_edge 42;
    init_edge 43;
    init_edge 44;
    init_edge 45;
    init_edge 46;
    init_edge 47;
    init_edge 48;
    init_edge 49;
    init_edge 50;
    init_edge 51;
    init_edge 52;
    init_edge 53;
    init_edge 54;
    init_edge 55;
    init_edge 56;
    init_edge 57;
    init_edge 58;
    init_edge 59;
    init_edge 60;
    init_edge 61;
    init_edge 62;
    init_edge 63;
    init_edge 64;
    init_edge 65;
    init_edge 66;
    init_edge 67;
    init_edge 68;
    init_edge 69;
    init_edge 70;
    init_edge 71;
  ]

let node_list =
  [
    init_node 0 [] [] [];
    init_node 1 [] [] [];
    init_node 2 [] [] [];
    init_node 3 [] [] [];
    init_node 4 [] [] [];
    init_node 5 [] [] [];
    init_node 6 [] [] [];
    init_node 7 [] [] [];
    init_node 8 [] [] [];
    init_node 9 [] [] [];
    init_node 10 [] [] [];
    init_node 11 [] [] [];
    init_node 12 [] [] [];
    init_node 13 [] [] [];
    init_node 14 [] [] [];
    init_node 15 [] [] [];
    init_node 16 [] [] [];
    init_node 17 [] [] [];
    init_node 18 [] [] [];
    init_node 19 [] [] [];
    init_node 20 [] [] [];
    init_node 21 [] [] [];
    init_node 22 [] [] [];
    init_node 23 [] [] [];
    init_node 24 [] [] [];
    init_node 25 [] [] [];
    init_node 26 [] [] [];
    init_node 27 [] [] [];
    init_node 28 [] [] [];
    init_node 29 [] [] [];
    init_node 30 [] [] [];
    init_node 31 [] [] [];
    init_node 32 [] [] [];
    init_node 33 [] [] [];
    init_node 34 [] [] [];
    init_node 35 [] [] [];
    init_node 36 [] [] [];
    init_node 37 [] [] [];
    init_node 38 [] [] [];
    init_node 39 [] [] [];
    init_node 40 [] [] [];
    init_node 41 [] [] [];
    init_node 42 [] [] [];
    init_node 43 [] [] [];
    init_node 44 [] [] [];
    init_node 45 [] [] [];
    init_node 46 [] [] [];
    init_node 47 [] [] [];
    init_node 48 [] [] [];
    init_node 49 [] [] [];
    init_node 50 [] [] [];
    init_node 51 [] [] [];
    init_node 52 [] [] [];
    init_node 53 [] [] [];
  ]

let get_tile ind tiles = List.find (fun t -> t.tile_id = ind) tiles
let get_node ind nodes = List.find (fun n -> n.node_id = ind) nodes
let get_edge ind edges = List.find (fun e -> e.edge_id = ind) edges

let build_road ind player = 
  List.map
    (fun e -> if e.edge_id = ind then {edge_id = e.edge_id; has_road = true; owner = Some player} else get_edge e.edge_id edge_list)
    edge_list

let build_settlement ind player =
  List.map
    (fun n -> if n.node_id = ind 
      then 
        {
          node_id = n.node_id;
          adj_nodes = n.adj_nodes;
          adj_edges = n.adj_edges;
          adj_tiles = n.adj_tiles;
          port = n.port;
          has_settlement = true;
          owner = Some player;
        }
      else get_node n.node_id node_list)
    node_list

let get_resource ind =
  tile_list
  |> List.filter (fun dice -> dice.dice_num = ind)
  |> List.map (fun res -> (res.tile_id, res.resource))


let rec spacer n = if n = 0 then "" else " " ^ spacer (n - 1)
let generator10 n str = str^ (spacer (10 - n)) 
let generator5 n str = str^ (spacer (5 - n))
let display_even n1 n2 n3 n4 n5 n6 n7 = "[" ^ generator10 (String.length n1) n1 ^ generator10 (String.length n2) n2 ^ generator10 (String.length n3) n3 ^
generator10 (String.length n4) n4 ^ generator10 (String.length n5) n5 ^ generator10 (String.length n6) n6 ^ generator5 (String.length n7) n7 ^"]" ^ "\n"

let display_odd n1 n2 n3 n4 n5 n6 n7 n8 = "[" ^ generator10 (String.length n1) n1 ^ generator5 (String.length n2) n2 ^ generator10 (String.length n3) n3 ^
generator10 (String.length n4) n4 ^ generator10 (String.length n5) n5 ^ generator5 (String.length n6) n6 ^ generator10 (String.length n7) n7 ^ 
generator5 (String.length n8) n8 ^ "]" ^ "\n"

let display_mid n1 n2 n3 n4 n5 n6 n7 = "[" ^ generator5 (String.length n1) n1 ^ generator10 (String.length n2) n2 ^ generator10 (String.length n3) n3 ^
generator10 (String.length n4) n4 ^ generator10 (String.length n5) n5 ^ generator10 (String.length n6) n6 ^ generator10 (String.length n7) n7 ^ "]" ^ "\n"

let display_blank = "\n" ^ "\n"


let player_string ind nodes = (match (get_node ind nodes).owner with | Some i -> node_color i | None -> "none")
let display_3 n1 n2 n3 = display_even "" "" n1 n2 n3 "" ""
let display_4 n1 n2 n3 n4 = display_odd "" "" n1 n2 n3 n3 "" ""
let display_5 n1 n2 n3 n4 n5 = display_even "" n1 n2 n3 n4 n5 ""
let display nodes= "\n" ^
display_3 (player_string 0 nodes) (player_string 1 nodes) (player_string 2 nodes) ^
display_blank ^
display_4  (player_string 3 nodes) (player_string 4 nodes) (player_string 5 nodes) (player_string 6 nodes) ^
display_blank ^
display_4  (player_string 7 nodes) (player_string 8 nodes) (player_string 9 nodes) (player_string 10 nodes) ^
display_blank ^
display_5  (player_string 11 nodes) (player_string 12 nodes) (player_string 13 nodes) (player_string 14 nodes) (player_string 15 nodes) ^
display_blank ^
display_5  (player_string 16 nodes) (player_string 17 nodes) (player_string 18 nodes) (player_string 19 nodes) (player_string 20 nodes) ^
display_blank ^
(display_mid "" (player_string 21 nodes) (player_string 22 nodes) (player_string 23 nodes) (player_string 24 nodes) (player_string 25 nodes) (player_string 26 nodes)) ^
display_blank ^
(display_mid "" (player_string 27 nodes) (player_string 28 nodes) (player_string 29 nodes) (player_string 30 nodes) (player_string 31 nodes) (player_string 32 nodes)) ^
display_blank ^
display_5  (player_string 33 nodes) (player_string 34 nodes) (player_string 35 nodes) (player_string 36 nodes) (player_string 37 nodes) ^
display_blank ^
display_5  (player_string 38 nodes) (player_string 39 nodes) (player_string 40 nodes) (player_string 41 nodes) (player_string 42 nodes) ^
display_blank ^
display_4  (player_string 43 nodes) (player_string 44 nodes) (player_string 45 nodes) (player_string 46 nodes) ^
display_blank ^
display_4  (player_string 47 nodes) (player_string 48 nodes) (player_string 49 nodes) (player_string 50 nodes) ^
display_blank ^
display_3 (player_string 51 nodes) (player_string 51 nodes) (player_string 53 nodes) ^
display_blank ^
display_blank