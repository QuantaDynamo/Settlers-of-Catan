type resource = Wheat | Sheep | Brick | Wood | Ore
type color = Red | Blue | Yellow | Green | Brown | White

type development_card =
  | Knight
  | VictoryPoint
  | Monopoly
  | YearofPlenty
  | RoadBuilding

type player = {
  player_color : color;
  resources : resource list;
  development_cards : development_card list;
  score : int;
  num_settlements : int;
  num_cities : int;
  num_roads : int;
}

type tile = {
  tile_robber : bool;
  tile_num : int;
  tile_resource : resource;
  tile_player : player;
}

(* Need to add more to the intersection portion (i.e. building type)*)
type settlement = { owner : player }
type city = { owner : player }
type road = { owner : player }
type port = { port_resource : resource; ratio : float }
type intersection = { owner : player option }

type board = {
  tiles : tile array;
  intersections : intersection array;
  roads : road array;
  settlements : settlement array;
  cities : city array;
  ports : port array;
}

type game_state = {
  current_player : player;
  game_over : bool;
  curr_board : board;
  dice : int;
  victory_points : int list;
  dc_bank : development_card list;
  r_bank : resource list;
  largest_army : player;
  longest_road : player;
  winner : player option;
}
