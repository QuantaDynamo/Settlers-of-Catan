open Catan
open Command
open Gamestate
open Board
open Player

type game_state = {
  board : Board.node list;
  player : Player.player;
}

let b = { board = Board.node_list; player = Player.player_test }

let rec game_loop game =
  ignore (Sys.command "clear");
  print_endline "What would you like to do? (start, roll, quit, settle)";
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
      settle game;
      game_loop game
  | Empty ->
      empty game;
      game_loop game
  | Invalid ->
      invalid game;
      game_loop game

and start game =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("Hello! You are now playing Settlers of Caml-tan. The current \
      player is "
    ^ string_of_player player_test);
  let nodes = Board.node_list in
  ANSITerminal.print_string [ ANSITerminal.blue ] (Board.display nodes);
  let new_game = game_loop game in
  game_loop new_game

and quit game =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Thank you for playing Settlers of Caml-tan! See you next time! \n"

and roll game =
  let p, q = roll_dice () in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("You've rolled a " ^ string_of_int (p + q) ^ "! \n")

and settle game =
  print_endline "Enter a number";
  let cmd_str = read_line () in
  let cmd = int_of_string cmd_str in
  let nodes = Board.node_list in
  let b =
    { board = build_settlement cmd b.player; player = b.player }
  in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("You've successfully settled!" ^ Board.display b.board)

and empty game =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Please enter a command. \n"

and invalid game =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Invalid command. Please try again. \n"

let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\nWelcome to Settlers of Caml-tan. Please enter a command. \n";
  print_string "> ";
  print_string (game_loop "")

let () = main ()
