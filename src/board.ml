open Player
type resource = Wheat | Sheep | Brick | Wood | Ore | Desert
type port = { port_resource : resource; ratio : int }

 

let test_resource res= 
  match res with
  | "Wheat" -> Wheat
  | "Sheep" -> Sheep
  | "Brick" -> Brick
  | "Wood" -> Wood
  | "Ore" -> Ore
  | "Desert" -> Desert
  | _ -> Wheat

type tile = {
  tile_id : int;
  resource : resource;
  dice_num : int;
  has_robber : bool;
}

let init_tile i res dice = {
  tile_id = i; 
  resource = res;
  dice_num = dice;
  has_robber = false
}

type edge = {
  edge_id : int;
  has_road : bool;
  owner : player option;

}

let init_edge i = {
  edge_id = i; 
  has_road = false;
  owner = None;
}

type node = {
  node_id : int;
  adj_nodes : node list;
  adj_edges : edge list;
  adj_tiles : tile list;
  port : port;
  has_settlement : bool;
  owner : player option;

}

let init_node i node_list edge_list tile_list = {
  node_id = i; 
  adj_nodes = node_list;
  adj_edges = edge_list;
  adj_tiles = tile_list;
  port = {port_resource = Desert;  ratio = 3};
  has_settlement = false;
  owner = None;

}

let tile_list = [
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

let edge_list = [
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

let node_list = [
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

let get_tile ind tiles = List.find(fun t -> t.tile_id = ind) tiles

let get_node ind nodes = List.find(fun n -> n.node_id = ind) nodes

let get_edge ind edges = List.find(fun e -> e.edge_id = ind) edges

let build_road ind road = List.map(fun e ->
  if e.edge_id = ind then road else get_edge ind edge_list) edge_list

let build_settlement ind sett = List.map(fun n ->
  if n.node_id = ind then sett else get_node ind node_list) node_list

let get_resource ind = tile_list |> List.filter (fun dice -> dice.dice_num = ind) |> List.map (fun res -> res.resource)