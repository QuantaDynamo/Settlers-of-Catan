open Player
open Board

type game_state = {
  current_player : player;
  game_over : bool;
  dice : int;
  victory_points : int list;
  dc_bank : development_card list;
  r_bank : resource list;
  largest_army : player;
  longest_road : player;
  winner : player option;
}

let roll_dice () =
  Random.self_init ();
  (1 + Random.int 6, 1 + Random.int 6)
