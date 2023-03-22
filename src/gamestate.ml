open Player

type game_state = {
  current_player : player;
  game_over : bool;
  dice : int;
  victory_points : int list;
  dc_bank : development_card list;
  r_bank : resource list;
  largest_army : player option;
  longest_road : player option;
  winner : player option;
}

let roll_dice () =
  Random.self_init ();
  (1 + Random.int 6, 1 + Random.int 6)

let init_state =
  {
    current_player =
      {
        player_color = Red;
        resources = [];
        development_cards = [];
        score = 0;
        num_settlements = 0;
        num_cities = 0;
        num_roads = 0;
      };
    game_over = false;
    dice = 0;
    victory_points = [];
    dc_bank = [];
    r_bank = [];
    largest_army = None;
    longest_road = None;
    winner = None;
  }


let start_game =
  {
    current_player = player_test;
    game_over = false;
    dice = 2;
    victory_points = [4; 3];
    dc_bank = [];
    r_bank= [];
    largest_army = None;
    longest_road = None;
    winner = None;
  }