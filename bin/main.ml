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

let remove_one_resource
    (player : player)
    (unwanted_resources : resource list) : resource list =
  List.filter
    (fun res -> not (List.mem res unwanted_resources))
    player.resources

let remove_resource (resources : resource list) (res : resource) :
    resource list =
  let rec remove acc = function
    | [] -> acc
    | r :: rs -> if r = res then remove acc rs else remove (r :: acc) rs
  in
  remove [] resources

let remove_card
    (player : player)
    (cards : development_card list)
    (card_to_remove : development_card) : development_card list =
  let rec remove acc = function
    | [] -> acc
    | c :: rest ->
        if c = card_to_remove then remove acc rest
        else remove (c :: acc) rest
  in
  remove [] cards

let rec game_loop game =
  ignore (Sys.command "clear");
  let current_player = List.nth game.players game.current_player in
  if current_player.score >= 10 then
    ANSITerminal.print_string [ ANSITerminal.green ]
      ("Congratulations, "
      ^ string_of_color current_player.player_color
      ^ " has won the game!\n")
  else Board.draw_board game.tiles game.nodes game.edges;
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("It's "
    ^ string_of_color current_player.player_color
    ^ "'s turn.\n");
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "What would you like to do now? (roll, quit, settle, rob, trade, \
     play card, end turn, buy card) \n\
     >";
  let cmd_str = read_line () in
  let cmd = parse_string cmd_str in
  match cmd with
  | Start -> start game
  | Quit ->
      quit game;
      exit 0
  | Roll ->
      let new_game = roll_and_process game in
      game_loop new_game
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
  | BuyCard ->
      let new_game = buy_card game in
      game_loop new_game
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

and buy_card game =
  let current_player = List.nth game.players game.current_player in
  if not (List.mem Wheat current_player.resources) then (
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "I'm sorry. You don't have enough resources to buy a card. \
       Please try a different command.";
    game)
  else (
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "What card would you like to buy? \n";
    let card = read_line () in
    let add_card = card_of_string card in
    let updated_player =
      {
        current_player with
        development_cards = add_card :: current_player.development_cards;
      }
    in
    let updated_players =
      List.mapi
        (fun i p ->
          if i = game.current_player then updated_player else p)
        (game.players
        |> List.map (fun p ->
               {
                 p with
                 development_cards =
                   List.filter
                     (fun c -> c <> add_card)
                     p.development_cards;
                 resources =
                   remove_one_resource current_player [ Wheat ];
               }))
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
      ("Congrats! "
      ^ string_of_color updated_player.player_color
      ^ " has bought " ^ card ^ ". Their development cards are: "
      ^ string_of_cards updated_player.development_cards
      ^ "\n");
    b)

and roll_and_process game =
  let current_player = List.nth game.players game.current_player in
  let dice_roll = roll_dice () in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    (string_of_color current_player.player_color
    ^ " has rolled a " ^ string_of_int dice_roll ^ "! \n");
  if dice_roll <> 7 && dice_roll <> 1 then (
    let new_resources =
      game.tiles
      |> List.filter (fun tiles -> tiles.dice_num = dice_roll)
      |> List.map (fun tiles -> tiles.resource)
    in
    let updated_player =
      {
        current_player with
        resources = new_resources @ current_player.resources;
        has_rolled = true;
      }
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
      (string_of_color updated_player.player_color
      ^ "'s resources are:"
      ^ string_of_resources updated_player.resources
      ^ "\n");
    b)
  else (
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "All players with 8 or more development cards must discard half \
       of them to the bank.";
    game)

and play_card game =
  let current_player = List.nth game.players game.current_player in
  if current_player.development_cards == [] then
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "I'm sorry. You don't have any development cards to play. Please \
       try a different command."
  else
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ("Your cards are:  "
      ^ string_of_cards current_player.development_cards
      ^ ". What would you like to play? \n");
  let cmd_str = read_line () in
  match cmd_str with
  | "knight" -> failwith "unimplemented"
  | "victory point" ->
      let updated_player =
        {
          current_player with
          score = current_player.score + 1;
          development_cards =
            remove_card current_player current_player.development_cards
              (card_of_string "victory point");
        }
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
        "Great news! You have been awarded one victory point.";
      ANSITerminal.print_string [ ANSITerminal.blue ]
        ("Your new score is " ^ string_of_int updated_player.score ^ "!");
      b
  | "monopoly" ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "Please choose a resource type to monopolize.";
      let resource_type = read_line () in
      let res = resource_of_string resource_type in
      let current_player = List.nth game.players game.current_player in
      let monopolized_resources =
        List.fold_left
          (fun acc p ->
            if p = current_player then acc
            else
              let resources = remove_resource p.resources res in
              let num_removed =
                List.length p.resources - List.length resources
              in
              List.append (List.init num_removed (fun _ -> res)) acc)
          [] game.players
      in
      let updated_player =
        {
          current_player with
          resources =
            List.append monopolized_resources current_player.resources;
          development_cards =
            remove_card current_player current_player.development_cards
              (card_of_string "monopoly");
        }
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
        ("You have monopolized " ^ string_of_resource res
       ^ " from all other players.");
      b
  | "year of plenty" ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "Please choose the first resource of your choice to add to \
         your resource bank.";
      let resource_one = read_line () in
      let res_one = resource_of_string resource_one in
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "Please choose the second resource of your choice to add to \
         your resource bank.";
      let resource_two = read_line () in
      let res_two = resource_of_string resource_two in
      let current_player = List.nth game.players game.current_player in
      let updated_player =
        {
          current_player with
          resources = res_one :: res_two :: current_player.resources;
          development_cards =
            remove_card current_player current_player.development_cards
              (card_of_string "year of plenty");
        }
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
        ("Great news! You have been awarded a "
        ^ string_of_resource res_one
        ^ " and a "
        ^ string_of_resource res_two
        ^ ".");
      b
  | "road building" ->
      let current_player = List.nth game.players game.current_player in
      let updated_player =
        {
          current_player with
          development_cards =
            remove_card current_player current_player.development_cards
              (card_of_string "road building");
        }
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
        "You may build two roads for free. Please type 'build road' to \
         do so.";
      b
  | _ -> game

let rec main () : unit =
  print_string "> ";
  ignore (start initial_game_state);
  ()

let () = main ()
