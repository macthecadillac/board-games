open OUnit2
open Common

let create_halfboard player tally holes =
  let holes = Array.map Count.of_int holes in
  let tally = Count.of_int tally in
  let halfBoard = HalfBoard.init player in
  let rec aux i halfBoard =
    if i < 0 then HalfBoard.update_tally tally halfBoard
    else let newBoard = HalfBoard.set_hole holes.(i) (Index.of_int i) halfBoard
    in aux (i - 1) newBoard
  in aux 5 halfBoard

let test_board_play_1 test_ctxt =
  let pieces = Count.of_int 8 and loc = Index.of_int 5 in
  let boardBeforePlay =
    let otherSide = create_halfboard Two 2 [|2; 3; 4; 0; 12; 8|] in
    let thisSide = create_halfboard One 3 [| 5; 1; 0; 0; 0; 0 |] in
    Board.build thisSide otherSide in
  let expectedBoard =
    let thisSide = create_halfboard Two 2 [|3; 4; 5; 1; 13; 9|] in
    let otherSide = create_halfboard One 4 [|6; 1; 0; 0; 0; 0|] in
    Board.build thisSide otherSide in
  let boardAfterPlay = Board.dist (Index.inc loc) pieces boardBeforePlay
  in
  assert_equal boardAfterPlay expectedBoard

let test_board_play_2 test_ctxt =
  let pieces = Count.of_int 13 and loc = Index.of_int 5 in
  let boardBeforePlay =
    let otherSide = create_halfboard One 2 [|9; 0; 1; 5; 4; 3|] in
    let thisSide = create_halfboard Two 4 [|6; 1; 0; 0; 0; 0|] in
    Board.build thisSide otherSide in
  let expectedBoard =
    let thisSide = create_halfboard One 2 [|0; 1; 2; 6; 5; 4|] in
    let otherSide = create_halfboard Two 15 [|7; 2; 1; 1; 1; 1|] in
    Board.build thisSide otherSide in
  let boardAfterPlay = Board.dist (Index.inc loc) pieces boardBeforePlay
  in
  assert_equal boardAfterPlay expectedBoard

(* let test_board_is_empty_1 test_ctxt = *)

let test_board_suite =
  "test_board_suite" >:::
    [ "test_board_play_1" >:: test_board_play_1;
      "test_board_play_2" >:: test_board_play_2 ]
;;

let () =
  run_test_tt_main test_board_suite
