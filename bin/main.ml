(** [play_game f] starts the adventure in file [f]. *)
let play_game = raise (Failure "Unimplemented: Main.play_game")

let quit_game = raise (Failure "Unimplemented: Main.quit_game")
let data_dir_prefix = "data" ^ Filename.dir_sep

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to Settlers of Camelton.\n";
  print_string "> ";
  play_game

(* Execute the game engine. *)
let () = main ()
