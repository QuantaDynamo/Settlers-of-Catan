type resource = Wheat | Sheep | Brick | Wood | Ore | Desert
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

let player_test = {player_color = Red; resources =[]; development_cards = []; score = 0; num_settlements =0 ; num_cities = 0; num_roads = 0;}

let string_color col = 
  match col with
  | Red -> "Red"
  | Blue -> "Blue"
  | Yellow -> "Yellow"
  | Green -> "Green"
  | Brown -> "Brown"
  | White -> "White"

let node_color player = string_color player.player_color

let string_of_resource res = 
  match res with
  | Wheat -> "Wheat"
  | Desert -> "Desert"
  | Sheep -> "Sheep"
  | Brick -> "Brick"
  | Wood -> "Wood"
  | Ore -> "Ore"

let string_of_card res = 
  match res with
  | Knight -> "Knight"
  | VictoryPoint -> "VictoryPoint"
  | Monopoly -> "Monopoly"
  | YearofPlenty -> "YearofPlenty"
  | RoadBuilding -> "RoadBuilding"

let string_of_resources = fun l -> String.concat "; " (List.map string_of_resource l)
let string_of_cards = fun l -> String.concat "; " (List.map string_of_card l)

let string_of_player (p: player) = Printf.sprintf "{player_color=%s; resources=%s; development_cards=%s; score=%d; num_settlements=%d; num_cities=%d; num_roads=%d;}"
(string_color p.player_color) (string_of_resources p.resources) (string_of_cards p.development_cards) p.score p.num_settlements p.num_cities p.num_roads

(* Need to add more to the intersection portion (i.e. building type)*)
type settlement = { owner : player }
type city = { owner : player }
type road = { owner : player }
type port = { port_resource : resource; ratio : int }
type intersection = { owner : player option }
