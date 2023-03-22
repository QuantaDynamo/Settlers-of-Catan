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

val string_of_color : color -> string
(** [string_of_color] converts a player color to a string version of their color *)
