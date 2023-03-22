type resource
type color = Red | Blue | Yellow | Green | Brown | White
type development_card

type player = {
  player_color : color;
  resources : resource list;
  development_cards : development_card list;
  score : int;
  num_settlements : int;
  num_cities : int;
  num_roads : int;
}

type settlement
type city
type road
type port
type intersection

val player_test: player
val string_of_player: player -> string
val node_color: player -> string
