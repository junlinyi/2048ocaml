open OUnit2
open Grid

let two_list_init_four = [
  [0; 0; 0; 0];
  [0; 0; 0; 0];
  [0; 0; 0; 0];
  [0; 0; 0; 0]
]

let two_list_init_five = [
  [0; 0; 0; 0; 0];
  [0; 0; 0; 0; 0];
  [0; 0; 0; 0; 0];
  [0; 0; 0; 0; 0];
  [0; 0; 0; 0; 0]
]

let two_list_init_six = [
  [0; 0; 0; 0; 0; 0];
  [0; 0; 0; 0; 0; 0];
  [0; 0; 0; 0; 0; 0];
  [0; 0; 0; 0; 0; 0];
  [0; 0; 0; 0; 0; 0];
  [0; 0; 0; 0; 0; 0];
]

let grid_1 = [
  [0; 0; 0; 0];
  [2; 0; 0; 2];
  [0; 0; 0; 0];
  [0; 0; 0; 0]
]

let grid_2 = [
  [8; 0; 0; 2];
  [8; 0; 0; 4];
  [2; 0; 0; 2];
  [0; 4; 2; 2]
]

let grid_3 = [
  [8; 16; 4; 2];
  [8; 2; 4; 4];
  [2; 4; 8; 32];
  [2; 4; 2; 2]
]

let grid_4 = [
  [8; 16; 4; 2];
  [4; 2; 32; 16];
  [2; 4; 8; 32];
  [16; 64; 2; 64]
]

let grid_5 = [
  [8; 256; 4; 2];
  [4; 2; 32; 16];
  [2; 4; 8; 32];
  [16; 64; 2; 64]
]

let grid_6 = [
  [8; 256; 4; 2];
  [4; 2; 32; 16];
  [2; 4; 16; 32];
  [16; 64; 2; 64]
]

let grid_7 = [
  [8; 256; 4; 2];
  [4; 2; 32; 16];
  [2; 4; 64; 32];
  [16; 64; 2; 64]
]

let grid_8 = [
  [8; 256; 4; 2];
  [4; 2; 32; 16];
  [2; 4; 8; 32];
  [16; 64; 2; 16]
]

let grid_tests =
  [
   "get_grid grid size 4 empty" >:: (fun _ -> 
        assert_equal (Grid.get_grid (Grid.get_state two_list_init_four 4 0 0 0)) 
        two_list_init_four);
    "get_grid grid size 5y empt" >:: (fun _ -> 
        assert_equal (Grid.get_grid (Grid.get_state two_list_init_five 5 0 0 0)) 
        two_list_init_five);
    "get_grid grid size 6 empty" >:: (fun _ -> 
        assert_equal (Grid.get_grid (Grid.get_state two_list_init_six 6 0 0 0)) 
        two_list_init_six);
    "transition right" >:: (fun _ -> 
        assert_equal (4) 
        (Grid.get_score (Grid.transition (Grid.get_state grid_1 4 0 23 0) "right")));
    "transition up" >:: (fun _ -> 
        assert_equal (20) 
        (Grid.get_score (Grid.transition (Grid.get_state grid_2 4 0 67 1) "up")));
    "transition down" >:: (fun _ -> 
        assert_equal (20) 
        (Grid.get_score (Grid.transition (Grid.get_state grid_2 4 0 63 0) "down")));
    "transition left" >:: (fun _ -> 
        assert_equal (4) 
        (Grid.get_score (Grid.transition (Grid.get_state grid_1 4 0 12 0) "left")));
    "get_score normal input1" >:: (fun _ -> 
        assert_equal (60) 
        (Grid.get_score (Grid.get_state two_list_init_five 5 60 18 1)));
    "get_score normal input2" >:: (fun _ -> 
        assert_equal (42) 
        (Grid.get_score (Grid.get_state two_list_init_four 4 42 898 1)));
    "get_score normal input3" >:: (fun _ -> 
        assert_equal (93) 
        (Grid.get_score (Grid.get_state two_list_init_four 4 93 7 0)));
    "get_score normal edge1" >:: (fun _ -> 
        assert_equal (0) 
        (Grid.get_score (Grid.get_state two_list_init_five 5 0 22 0)));
    "get_score normal edge2" >:: (fun _ -> 
        assert_equal (9999999) 
        (Grid.get_score (Grid.get_state two_list_init_five 5 9999999 3 1)));
    "game_over empty board size 4" >:: (fun _ -> 
        assert_equal (false) 
        (Grid.game_over (Grid.get_state two_list_init_four 4 0 1 0)));
    "game_over empty board size 5" >:: (fun _ -> 
        assert_equal (false) 
        (Grid.game_over (Grid.get_state two_list_init_five 5 0 34 1)));
    "game_over empty board size 6" >:: (fun _ -> 
        assert_equal (false) 
        (Grid.game_over (Grid.get_state two_list_init_six 6 0 33 0)));
    "game_over non-empty board size 4" >:: (fun _ -> 
        assert_equal (false) 
        (Grid.game_over (Grid.get_state grid_1 4 216 23 0)));
    "game_over filled board, valid moves remaining" >:: (fun _ -> 
        assert_equal (false) 
        (Grid.game_over (Grid.get_state grid_3 4 42 12 0)));
    "game_over no valid moves remaining" >:: (fun _ -> 
        assert_equal (true) 
        (Grid.game_over (Grid.get_state grid_4 4 36 13 0)));
    "game_over no valid moves remaining" >:: (fun _ -> 
        assert_equal (true) 
        (Grid.game_over (Grid.get_state grid_5 4 36 123 0)));
    "game_over no valid moves remaining" >:: (fun _ -> 
        assert_equal (true) 
        (Grid.game_over (Grid.get_state grid_6 4 36 31 0)));
    "game_over no valid moves remaining" >:: (fun _ -> 
        assert_equal (true) 
        (Grid.game_over (Grid.get_state grid_7 4 36 313 0)));
    "game_over no valid moves remaining" >:: (fun _ -> 
        assert_equal (true) 
        (Grid.game_over (Grid.get_state grid_8 4 36 12 0)));
    "get_size grid size 6 empty" >:: (fun _ -> 
        assert_equal 6
        (Grid.get_size (Grid.get_state two_list_init_six 6 0 0 0)));
    "get_size grid size 5 empty" >:: (fun _ -> 
        assert_equal 5
        (Grid.get_size (Grid.get_state two_list_init_five 5 0 0 0)));
    "get_size grid size 4 empty" >:: (fun _ -> 
        assert_equal 4
        (Grid.get_size (Grid.get_state two_list_init_four 4 0 0 0)));
    "get_mode grid size 4 normal" >:: (fun _ -> 
        assert_equal 0
        (Grid.get_mode (Grid.get_state two_list_init_four 4 0 0 0)));
    "get_mode grid size 4 hard" >:: (fun _ -> 
        assert_equal 1
        (Grid.get_mode (Grid.get_state two_list_init_four 4 0 0 1)));
    "get_mode grid size 5 hard" >:: (fun _ -> 
        assert_equal 1
        (Grid.get_mode (Grid.get_state two_list_init_five 5 0 0 1)));
    "get_mode grid size 5 normal" >:: (fun _ -> 
        assert_equal 0
        (Grid.get_mode (Grid.get_state two_list_init_five 5 0 0 0)));
    "get_mode grid size 6 normal" >:: (fun _ -> 
        assert_equal 0
        (Grid.get_mode (Grid.get_state two_list_init_six 6 0 0 0)));
    "get_mode grid size 6 hard" >:: (fun _ -> 
        assert_equal 1
        (Grid.get_mode (Grid.get_state two_list_init_six 6 0 0 1)));
    "get_move_id grid size 6" >:: (fun _ -> 
        assert_equal 13
        (Grid.get_move_id (Grid.get_state two_list_init_six 6 0 13 1)));
    "get_move_id grid size 5" >:: (fun _ -> 
        assert_equal 38
        (Grid.get_move_id (Grid.get_state two_list_init_five 5 0 38 0)));
    "get_move_id grid size 4" >:: (fun _ -> 
        assert_equal 2
        (Grid.get_move_id (Grid.get_state two_list_init_four 4 0 2 0)));
    "get_move_id grid size 6 edge" >:: (fun _ -> 
        assert_equal 0
        (Grid.get_move_id (Grid.get_state two_list_init_six 6 0 0 1)));
    "get_move_id grid size 5 edge" >:: (fun _ -> 
        assert_equal 0
        (Grid.get_move_id (Grid.get_state two_list_init_five 5 0 0 0)));
    "get_move_id grid size 4 edge" >:: (fun _ -> 
        assert_equal 0
        (Grid.get_move_id (Grid.get_state two_list_init_four 4 0 0 0)));
    "get_move_id grid size 6 edge2" >:: (fun _ -> 
        assert_equal 1
        (Grid.get_move_id (Grid.get_state two_list_init_six 6 0 1 0)));
    "get_move_id grid size 5 edge2" >:: (fun _ -> 
        assert_equal 1
        (Grid.get_move_id (Grid.get_state two_list_init_five 5 0 1 0)));
    "get_move_id grid size 4 edge2" >::(fun _ -> 
        assert_equal 1
        (Grid.get_move_id (Grid.get_state two_list_init_four 4 0 1 0)));
    "get_move_id grid size 6 edge3" >:: (fun _ -> 
        assert_equal 9999
        (Grid.get_move_id (Grid.get_state two_list_init_six 6 0 9999 1)));
    "get_move_id grid size 5 edge3" >:: (fun _ -> 
        assert_equal 9999
        (Grid.get_move_id (Grid.get_state two_list_init_five 5 0 9999 0)));
    "get_move_id grid size 4 nonempty1" >:: (fun _ -> 
        assert_equal 9999
        (Grid.get_move_id (Grid.get_state grid_1 4 0 9999 0)));
    "get_move_id grid size 4 nonempty2" >:: (fun _ -> 
        assert_equal 9999
        (Grid.get_move_id (Grid.get_state grid_2 4 0 9999 0)));
    "get_move_id grid size 4 nonempty3" >:: (fun _ -> 
        assert_equal 9999
        (Grid.get_move_id (Grid.get_state grid_3 4 0 9999 0)));
    "get_move_id grid size 4 nonempty4" >:: (fun _ -> 
        assert_equal 9999
        (Grid.get_move_id (Grid.get_state grid_4 4 0 9999 0)));
    "get_move_id grid size 4 nonempty5" >:: (fun _ -> 
        assert_equal 9999
        (Grid.get_move_id (Grid.get_state grid_5 4 0 9999 0)));
    "get_move_id grid size 4 nonempty6" >:: (fun _ -> 
        assert_equal 9999
        (Grid.get_move_id (Grid.get_state grid_6 4 0 9999 0)));
    "get_move_id grid size 4 nonempty7" >:: (fun _ -> 
        assert_equal 9999
        (Grid.get_move_id (Grid.get_state grid_7 4 0 9999 0)));
    "get_move_id grid size 4 nonempty8" >:: (fun _ -> 
        assert_equal 9999
        (Grid.get_move_id (Grid.get_state grid_8 4 0 9999 0)));

  ]
let suite =
  "test suite for 2048"  >::: List.flatten [
    grid_tests;
  ]

let _ = run_test_tt_main suite
