open Containers
open Common

let init_board () = Board.build (HalfBoard.init One) (HalfBoard.init Two)

let print_tally halfBoard =
  let aux s =
    Printf.printf "Player ";
    print_player (HalfBoard.get_player s);
    Printf.printf " has %i pieces\n" (HalfBoard.get_tally s |> Count.to_int); in
  let talliedBoard = Board.final_tally halfBoard in
  aux (Board.curr_side talliedBoard);
  aux (Board.other_side talliedBoard)

let rec acquire_input () =
  print_string "\nEnter your move (0-5): ";
  let humanMove =
    try read_int () |> Index.of_int
    with Failure _ ->
      print_endline "Invalid input.";
      acquire_input () in
  if Index.to_int humanMove > 5 || Index.to_int humanMove < 0 then
    (print_endline "Invalid input.";
    acquire_input ())
  else humanMove

let rec two_player_game board = match Board.is_finished board with
  | false ->
      Printf.printf "Current player: ";
      print_player (Board.curr_player board);
      print_newline ();
      Board.print board;
      let n = acquire_input () in
      (match Board.remove_pieces n board with
      | Some (cnt, b) ->
          let newBoard = Board.dist (Index.inc n) cnt b in
          print_endline "\nAfter your move:\n";
          Board.print b;
          print_endline "\n================================\n";
          two_player_game newBoard
      | None          -> print_endline "\nThe bowl is empty!";
                         two_player_game board)
  | true ->
      print_tally board;
      match Board.winner_is board with
      | None   -> print_endline "The game is a draw.";
      | Some p -> print_string "The winner is ";
                  print_player p;
                  print_endline "."

let rec play_vs_ai searchLimit humanSide board =
  let currSide = Board.curr_player board in
  match Board.is_finished board with
  | false -> (match humanSide, currSide with
      | One, One | Two, Two ->
        Board.print board;
        let humanMove = acquire_input () in
        (match Board.remove_pieces humanMove board with
          | Some (count, newBoard) ->
              let b = Board.dist (Index.inc humanMove) count newBoard in
              print_endline "\nAfter your move:\n";
              Board.print b;
              print_endline "\n================================\n";
              play_vs_ai searchLimit humanSide b
          | None                   -> print_endline "\nThe bowl is empty!\n";
                                      play_vs_ai searchLimit humanSide board)
      | _ ->
        let aiMove = Ai.most_favored_move searchLimit currSide board in
        (match Board.remove_pieces aiMove board with
          | Some (count, newBoard) ->
              let b = Board.dist (Index.inc aiMove) count newBoard in
              play_vs_ai searchLimit humanSide b
          | None                   -> ())) (* Impossible branch *)
  | true  ->
      print_tally board;
      match Board.winner_is board with
      | None   -> print_endline "The game is a draw.";
      | Some p -> print_string "The winner is ";
                  print_player p;
                  print_endline "."
;;

let () =
  let searchLimit =
    try int_of_string Sys.argv.(1)
    with Invalid_argument _ ->
      print_endline "Usage mancala <searchLimit>";
      exit 1;
  in
  init_board () |> play_vs_ai searchLimit One

(* let () = *)
(*   init_board () |> two_player_game *)
