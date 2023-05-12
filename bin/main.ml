open Catan
open Command
open Gamestate
open Board
open Player

type game_state = {
  board : Board.node list;
  players : Player.player list;
  mutable current_player : int;
}

let initial_game_state =
  {
    board = Board.node_list;
    players = [ Player.player_test; Player.player_test ];
    current_player = 0;
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
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("It's "
    ^ string_of_color current_player.player_color
    ^ "'s turn. What would you like to do? (start, roll, quit, settle)"
    );
  let cmd_str = read_line () in
  let cmd = parse_string cmd_str in
  match cmd with
  | Start -> start game
  | Quit ->
      quit game;
      exit 0
  | Roll ->
      roll game;
      game_loop game
  | Settle ->
      let new_game = settle game current_player in
      game_loop new_game
  | BuildRoad ->
      (* build_road game current_player; *)
      game_loop game
  | PlayCard -> failwith "Unimplemented"
  | Rob -> failwith "Unimplemented"
  | Trade -> failwith "Unimplemented"
  | EndTurn -> failwith "Unimplemented"
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

and start game =
  let current_player = List.nth game.players game.current_player in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("Hello! You are now playing Settlers of Caml-tan. The current \
      player is "
    ^ string_of_color current_player.player_color
    ^ ".\n");
  Board.draw_board Board.tile_list Board.node_list Board.edge_list;
  let new_game = game_loop game in
  game_loop new_game

and quit game =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Thank you for playing Settlers of Caml-tan! See you next time! \n"

and roll game =
  let p, q = roll_dice () in
  let current_player = List.nth game.players game.current_player in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    (string_of_color current_player.player_color
    ^ " has rolled a "
    ^ string_of_int (p + q)
    ^ "! \n")

and settle game player =
  print_endline "Enter a number";
  let cmd_str = read_line () in
  let cmd = int_of_string cmd_str in
  let nodes = Board.node_list in
  let current_player = List.nth game.players game.current_player in
  let updated_player =
    {
      current_player with
      num_settlements = current_player.num_settlements + 1;
    }
  in
  let updated_players =
    List.mapi
      (fun i p -> if i = game.current_player then updated_player else p)
      game.players
  in
  let b =
    {
      current_player = game.current_player;
      board = build_settlement cmd current_player game.board;
      players = updated_players;
    }
  in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "You've successfully settled! ";
  Board.draw_board Board.tile_list b.board Board.edge_list;
  b

(* and build_road game player = print_endline "Enter the index of the
   starting node:"; let start_node_idx = read_int () in
   player.num_cities + 1 *)

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

let rec main () : unit =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\nWelcome to Settlers of Caml-tan. Please enter a command. \n";
  print_string "> ";
  ignore (game_loop initial_game_state);
  ()

let () = main ()
