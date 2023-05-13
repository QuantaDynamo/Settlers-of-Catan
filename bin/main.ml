open Catan
open Command
open Gamestate
open Board
open Player

type game_state = {
  nodes : Board.node list;
  edges : Board.edge list;
  tiles : Board.tile list;
  players : Player.player list;
  mutable current_player : int;
  mutable dice_rolled : bool;
}

let player_one =
  {
    player_color = Red;
    resources = [];
    development_cards = [];
    score = 0;
    num_settlements = 0;
    num_cities = 0;
    num_roads = 0;
    has_rolled = false;
  }

let player_two =
  {
    player_color = Blue;
    resources = [];
    development_cards = [];
    score = 0;
    num_settlements = 0;
    num_cities = 0;
    num_roads = 0;
    has_rolled = false;
  }

let initial_game_state =
  {
    nodes = node_list;
    edges = edge_list;
    tiles = tile_list;
    players = [ player_one; player_two ];
    current_player = 0;
    dice_rolled = false;
  }

let string_of_color = function
  | Red -> "Red"
  | Blue -> "Blue"
  | Yellow -> "Yellow"
  | Green -> "Green"
  | Brown -> "Brown"
  | White -> "White"

let remove_resources (player : player) unwanted_resources : player =
  let filtered_resources =
    List.filter
      (fun res -> not (List.mem res unwanted_resources))
      player.resources
  in
  { player with resources = filtered_resources }

let rec game_loop game =
  ignore (Sys.command "clear");
  let current_player = List.nth game.players game.current_player in
  if current_player.score >= 10 then
    ANSITerminal.print_string [ ANSITerminal.green ]
      ("Congratulations, "
      ^ string_of_color current_player.player_color
      ^ " has won the game!\n")
  else
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ("It's "
      ^ string_of_color current_player.player_color
      ^ "'s turn.\n");
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "What would you like to do now? (roll, quit, settle, rob, trade, \
     play card, end turn) \n\
     >";
  let cmd_str = read_line () in
  let cmd = parse_string cmd_str in
  match cmd with
  | Start -> start game
  | Quit ->
      quit game;
      exit 0
  | Roll ->
      roll_and_process game;
      game_loop game
  | Settle ->
      let new_game = settle game current_player in
      game_loop new_game
  | BuildRoad ->
      let new_game = new_road game current_player in
      game_loop new_game
  | PlayCard -> play_card game
  | Rob -> failwith "Unimplemented"
  | Trade -> failwith "Unimplemented"
  | EndTurn ->
      Board.draw_board Board.tile_list game.nodes game.edges;
      let new_game =
        {
          game with
          current_player =
            (game.current_player + 1) mod List.length game.players;
        }
      in
      game_loop new_game
  | CheckResources ->
      check_resources game;
      game_loop game
  | CheckSettlements ->
      check_settlements game;
      game_loop game
  | CheckRoads ->
      check_roads game;
      game_loop game
  | CheckScore ->
      check_score game;
      game_loop game
  | CheckCards ->
      check_cards game;
      game_loop game
  | Empty ->
      empty game;
      game_loop game
  | Invalid ->
      invalid game;
      game_loop game

and start initial_game_state =
  let current_player =
    List.nth initial_game_state.players
      initial_game_state.current_player
  in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("Hello! You are now playing Settlers of Caml-tan. The current \
      player is "
    ^ string_of_color current_player.player_color
    ^ ". Please place two settlements and two roads coming from each \
       settlement. Then, end your turn. \n");
  Board.draw_board Board.tile_list Board.node_list Board.edge_list;
  let new_game = game_loop initial_game_state in
  start new_game

and quit game =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Thank you for playing Settlers of Caml-tan! See you next time! \n"

(* and roll game = let p, q = roll_dice () in let current_player =
   List.nth game.players game.current_player in
   ANSITerminal.print_string [ ANSITerminal.blue ] (string_of_color
   current_player.player_color ^ " has rolled a " ^ string_of_int (p +
   q) ^ "! \n") *)

and settle game player =
  print_endline "Enter the number of the node you'd like to settle: ";
  let cmd_str = read_line () in
  let cmd = int_of_string cmd_str in
  let current_player = List.nth game.players game.current_player in
  let updated_player =
    {
      current_player with
      num_settlements = current_player.num_settlements + 1;
      score = current_player.score + 1;
      has_rolled = true;
    }
  in
  let updated_players =
    List.mapi
      (fun i p -> if i = game.current_player then updated_player else p)
      game.players
  in
  let b =
    {
      nodes = build_settlement cmd current_player game.nodes;
      edges = game.edges;
      tiles = game.tiles;
      current_player = game.current_player;
      players = updated_players;
      dice_rolled = true;
    }
  in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "You've successfully settled!";
  Board.draw_board Board.tile_list b.nodes b.edges;
  b

and new_road game player =
  print_endline
    "Enter the number of the edge where you'd like to build your road: ";
  let cmd_str = read_line () in
  let cmd = int_of_string cmd_str in
  let current_player = List.nth game.players game.current_player in
  let updated_player =
    {
      current_player with
      num_roads = current_player.num_roads + 1;
      has_rolled = true;
    }
  in
  let updated_players =
    List.mapi
      (fun i p -> if i = game.current_player then updated_player else p)
      game.players
  in
  let b =
    {
      nodes = game.nodes;
      edges = build_road cmd current_player game.edges;
      tiles = game.tiles;
      current_player = game.current_player;
      players = updated_players;
      dice_rolled = true;
    }
  in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "You've successfully built a road!";
  Board.draw_board Board.tile_list b.nodes b.edges;
  b

and empty game =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Please enter a command. \n"

and invalid game =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Invalid command. Please try again. \n"

and check_resources game =
  let current_player = List.nth game.players game.current_player in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("Your resources are: "
    ^ string_of_resources current_player.resources
    ^ "\n")

and check_settlements game =
  let current_player = List.nth game.players game.current_player in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("You have "
    ^ string_of_int current_player.num_settlements
    ^ " settlements!  \n")

and check_roads game =
  let current_player = List.nth game.players game.current_player in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("You have " ^ string_of_int current_player.num_roads ^ " roads. \n")

and check_score game =
  let current_player = List.nth game.players game.current_player in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("Your score is:  " ^ string_of_int current_player.score ^ "\n")

and check_cards game =
  let current_player = List.nth game.players game.current_player in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("Your cards are:  "
    ^ string_of_cards current_player.development_cards
    ^ "\n")

and roll_and_process game =
  let current_player = List.nth game.players game.current_player in
  let dice_roll = roll_dice () in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    (string_of_color current_player.player_color
    ^ " has rolled a "
    ^ string_of_int (fst dice_roll + snd dice_roll)
    ^ "! \n");
  let resource =
    game.tiles
    |> List.find (fun tiles ->
           tiles.dice_num = fst dice_roll + snd dice_roll)
    |> fun tiles -> tiles.resource
  in
  let updated_player =
    {
      current_player with
      resources = resource :: current_player.resources;
      has_rolled = true;
    }
  in
  let updated_players =
    List.mapi
      (fun i p -> if i = game.current_player then updated_player else p)
      game.players
  in
  let b =
    {
      nodes = game.nodes;
      edges = game.edges;
      tiles = game.tiles;
      current_player = game.current_player;
      players = updated_players;
      dice_rolled = true;
    }
  in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    (string_of_color updated_player.player_color
    ^ "'s resources are:"
    ^ string_of_resources updated_player.resources
    ^ "\n");
  b

and play_card game =
  let current_player = List.nth game.players game.current_player in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("Your cards are:  "
    ^ string_of_cards current_player.development_cards
    ^ ". What would you like to play? \n");
  let cmd_str = read_line () in
  match cmd_str with
  | "knight" -> failwith "unimplemented"
  | "victory point" ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "Great news! You have been awarded one victory point.";
      let updated_player =
        { current_player with score = current_player.score + 1 }
      in
      let updated_players =
        List.mapi
          (fun i p ->
            if i = game.current_player then updated_player else p)
          game.players
      in
      let b =
        {
          nodes = game.nodes;
          edges = game.edges;
          tiles = game.tiles;
          current_player = game.current_player;
          players = updated_players;
          dice_rolled = true;
        }
      in
      ANSITerminal.print_string [ ANSITerminal.blue ]
        ("Your new scores is " ^ string_of_int updated_player.score);
      Board.draw_board Board.tile_list b.nodes b.edges;
      b
  | "monopoly" -> failwith "unimplemented"
  | "year of plenty" -> failwith "unimplemented"
  | "road building" ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "You may build two roads for free. Please type 'build road' to \
         do so.";
      game
  | _ -> game

let rec main () : unit =
  print_string "> ";
  start initial_game_state;
  ()

let () = main ()
