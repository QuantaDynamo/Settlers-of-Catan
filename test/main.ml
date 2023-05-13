open OUnit2
open Catan
open Board
open Gamestate
open Player

let get_tile_test
    (name : string)
    (ind : int)
    (tiles : Board.tile list)
    (expected_output : Board.tile) : test =
  name >:: fun _ -> assert_equal expected_output (get_tile ind tiles)

let print_node n = string_of_node n

let get_node_test
    (name : string)
    (ind : int)
    (nodes : Board.node list)
    (expected_output : Board.node) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_node ind nodes) ~printer:print_node

let get_edge_test
    (name : string)
    (ind : int)
    (edges : Board.edge list)
    (expected_output : Board.edge) : test =
  name >:: fun _ -> assert_equal expected_output (get_edge ind edges)

let printer_edges l = String.concat "; " (List.map string_of_edge l)

let build_road_test
    (name : string)
    (ind : int)
    (player : Player.player)
    (edges : Board.edge list)
    (expected_output : Board.edge list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (build_road ind player edges)
    ~printer:printer_edges

let printer_nodes l = String.concat "; " (List.map string_of_node l)

let build_settlement_test
    (name : string)
    (ind : int)
    (player : Player.player)
    (nodes : Board.node list)
    (expected_output : Board.node list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (build_settlement ind player nodes)
    ~printer:printer_nodes

let string_of_tuple (i, r) =
  Printf.sprintf "(%d, %s)" i (string_resource r)

let printer_resource l = String.concat "; " (List.map string_of_tuple l)

let get_resource_test
    (name : string)
    (input : int)
    (expected_output : (int * resource) list) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_resource input)
    ~printer:printer_resource

(* let display_even_test (name : string) (n1 : string) (n2 : string) (n3
   : string) (n4 : string) (n5 : string) (n6 : string) (expected_output
   : string) : test = name >:: fun _ -> assert_equal expected_output
   (display_even n1 n2 n3 n4 n5 n6) ~printer:(fun x -> x)

   let display_odd_test (name : string) (n1 : string) (n2 : string) (n3
   : string) (n4 : string) (n5 : string) (n6 : string) (n7 : string)
   (expected_output : string) : test = name >:: fun _ -> assert_equal
   expected_output (display_odd n1 n2 n3 n4 n5 n6 n7) ~printer:(fun x ->
   x) *)

let draw_test
    (name : string)
    (tiles : Board.tile list)
    (nodes : Board.node list)
    (edges : Board.edge list)
    (expected_output : unit) : test =
  name >:: fun _ ->
  assert_equal expected_output (draw_board tiles nodes edges)

let player_red =
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

let player_blue =
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

let tile_test = tile_list
let node_test = node_list
let edge_test = edge_list
let wheat_test = test_resource "Wheat"
let desert_test = test_resource "Desert"
let brick_test = test_resource "Brick"
let ore_test = test_resource "Ore"
let test_edges_basic = build_road 4 player_red Board.edge_list

let test_edges_sequence =
  let settle_4 = build_road 4 player_red Board.edge_list in
  let settle_35 = build_road 35 player_red settle_4 in
  let settle_17 = build_road 17 player_red settle_35 in
  build_road 53 player_red settle_17

let test_nodes_basic = build_settlement 4 player_red Board.node_list

let test_nodes_sequence =
  let settle_4 = build_settlement 4 player_red Board.node_list in
  let settle_35 = build_settlement 35 player_red settle_4 in
  let settle_17 = build_settlement 17 player_red settle_35 in
  build_settlement 53 player_red settle_17

let boardchange_tests =
  [
    get_tile_test "Check tile of index 0" 0 tile_test
      (init_tile 0 wheat_test 2);
    get_tile_test "Check tile of index " 18 tile_test
      (init_tile 18 desert_test 0);
    get_node_test "Check node of index 0" 0 node_test
      (init_node 0 [ 1; 8 ] [ 0; 6 ] [ 0 ]);
    get_node_test "Check node of index 53" 53 node_test
      (init_node 53 [ 45; 52 ] [ 65; 71 ] [ 18 ]);
    get_edge_test "Check edge of index 0" 0 edge_test (init_edge 0);
    get_edge_test "Check edge of index 71" 71 edge_test (init_edge 71);
    build_road_test "Basic test for build_road" 4 player_red edge_test
      test_edges_basic;
    build_road_test "Complex test for build_road" 43 player_red
      test_edges_sequence
      (build_road 43 player_red test_edges_sequence);
    build_road_test "Multiplayer test for build_road" 53 player_blue
      test_edges_sequence test_edges_sequence;
    build_road_test "Out of index test for build_road" 72 player_red
      test_edges_sequence test_edges_sequence;
    build_settlement_test "Basic test for build_settlement" 4 player_red
      node_test test_nodes_basic;
    build_settlement_test "Complex test for build_settlement" 43
      player_red test_nodes_sequence
      (build_settlement 43 player_red test_nodes_sequence);
    build_settlement_test "Multiplayer test for build_settlement" 53
      player_blue test_nodes_sequence test_nodes_sequence;
    build_settlement_test "Out of index test for build_settlement" 54
      player_red test_nodes_sequence test_nodes_sequence;
    get_resource_test "Check when dice roll has one resource" 11
      [ (15, brick_test); (16, ore_test) ];
    get_resource_test "Check when return has length one" 2
      [ (0, wheat_test) ];
    get_resource_test "Check when dice roll has two different resources"
      3
      [ (1, wheat_test); (2, wheat_test) ];
    (* display_even_test "Check" "1" "2" "3" "4" "5" "6" "TEST";
       display_even_test "Check2" "hi" "good" "p" "hi" "good" "p"
       "TEST"; display_odd_test "Check" "1" "2" "3" "4" "5" "6" "7"
       "TEST"; *)
    draw_test "Check" Board.tile_list Board.node_list Board.edge_list ();
  ]

let suite =
  "test suite for game" >::: List.flatten [ boardchange_tests ]

let _ = run_test_tt_main suite
