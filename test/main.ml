open OUnit2
open Catan
open Board
open Gamestate

let get_tile_test (name : string) (ind : int) (tiles : Board.tile list)
    (expected_output : Board.tile) : test =
  name >:: fun _ -> assert_equal expected_output (get_tile ind tiles)

let get_node_test (name : string) (ind : int) (nodes : Board.node list)
    (expected_output : Board.node) : test =
  name >:: fun _ -> assert_equal expected_output (get_node ind nodes)

let get_edge_test (name : string) (ind : int) (edges : Board.edge list)
    (expected_output : Board.edge) : test =
  name >:: fun _ -> assert_equal expected_output (get_edge ind edges)

(* let print_edge (e: edge) = Printf.sprintf "Edge is: {edge_id=%d, has_road = %b; owner = %a }\n" e.edge_id *)
let build_road_test (name : string) (ind : int) (edge : Board.edge)
    (expected_output : Board.edge list) : test =
  name >:: fun _ -> assert_equal expected_output (build_road ind edge)

let build_settlement_test (name : string) (ind : int) (node : Board.node)
    (expected_output : Board.node list) : test =
  name >:: fun _ -> assert_equal expected_output (build_settlement ind node)

let tile_test = tile_list
let node_test = node_list
let edge_test = edge_list
let wheat_test = test_resource "Wheat"
let desert_test = test_resource "Desert"

let boardchange_tests =
  [
    get_tile_test "Check tile of index 0" 0 tile_test
      (Board.init_tile 0 wheat_test 2);
    get_tile_test "Check tile of index " 18 tile_test
      (Board.init_tile 18 desert_test 0);
    get_node_test "Check node of index 0" 0 node_test
      (Board.init_node 0 [] [] []);
    get_node_test "Check node of index 53" 53 node_test
      (Board.init_node 53 [] [] []);
    get_edge_test "Check edge of index 0" 0 edge_test (Board.init_edge 0);
    get_edge_test "Check edge of index 71" 71 edge_test (Board.init_edge 71);
  ]

let suite = "test suite for game" >::: List.flatten [ boardchange_tests ]
let _ = run_test_tt_main suite
