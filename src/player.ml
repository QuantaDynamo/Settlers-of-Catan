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

(* Need to add more to the intersection portion (i.e. building type)*)
type settlement = { owner : player }
type city = { owner : player }
type road = { owner : player }
type port = { port_resource : resource; ratio : int }
type intersection = { owner : player option }
