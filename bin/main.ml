open Catan
open Command
open Gamestate
open Board
open Player

type game_state = { board : Board.node list; player : Player.player }

let b = { board = Board.node_list; player = Player.player_test }

let rec play_game game =
  let rec game_loop game =
    ignore (Sys.command "clear");
    print_endline "What would you like to do? (start, roll, quit, settle)";
    let cmd_str = read_line () in
    let cmd = parse_string cmd_str in
    match cmd with
    | Start ->
        ANSITerminal.print_string [ ANSITerminal.blue ]
          ("Hello! You are now playing Settlers of Caml-tan. The current \
            player is "
          ^ string_of_player player_test);
        let nodes = Board.node_list in
        ANSITerminal.print_string [ ANSITerminal.blue ] (Board.display nodes);
        let new_game = play_game game in
        game_loop new_game
    | Quit ->
        ANSITerminal.print_string [ ANSITerminal.blue ]
          "Thank you for playing Settlers of Caml-tan! See you next time! \n";
        exit 0
    | Roll ->
        let p, q = roll_dice () in
        ANSITerminal.print_string [ ANSITerminal.blue ]
          ("You've rolled a " ^ string_of_int (p + q) ^ "!");
        game_loop game
    | Settle ->
        (* let nodes = Board.node_list in *)
        let b = { board = build_settlement 2 b.player; player = b.player } in
        ANSITerminal.print_string [ ANSITerminal.blue ] ("You've successfully settled!" ^ (Board.display b.board));
        game_loop game
    | Empty ->
        ANSITerminal.print_string [ ANSITerminal.blue ]
          "Please enter a command. \n";
        game_loop game
    | Invalid ->
        ANSITerminal.print_string [ ANSITerminal.blue ]
          "Invalid command. Please try again. \n";
        game_loop game
  in
  game_loop game

let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\nWelcome to Settlers of Caml-tan. Please enter a command. \n";
  print_string "> ";
  print_string (play_game "")

let () = main ()
