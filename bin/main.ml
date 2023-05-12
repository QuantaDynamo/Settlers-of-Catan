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
  let nodes = Board.node_list in
  ANSITerminal.print_string [ ANSITerminal.blue ] (Board.display nodes);
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
  let b =
    {
      current_player = game.current_player;
      board = build_settlement cmd current_player;
      players = game.players;
    }
  in
  current_player.num_settlements + 1;
  (* let updated_player = remove_resources current_player [ Wood; Brick;
     Sheep; Wheat ] in *)
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("You've successfully settled! " ^ Board.display b.board);
  b.current_player <- (b.current_player + 1) mod 2;
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

let rec main () : unit =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\nWelcome to Settlers of Caml-tan. Please enter a command. \n";
  print_string "> ";
  ignore (game_loop initial_game_state);
  ()

let () = main ()
