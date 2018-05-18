open Containers
open Common

let is_finished board =
  HalfBoard.is_empty board.thisSide || HalfBoard.is_empty board.otherSide 

let final_tally board =
  let thisSide = HalfBoard.clear_board board.thisSide in
  let otherSide = HalfBoard.clear_board board.otherSide in
  { thisSide; otherSide }

let winner_is board =
  let module H = HalfBoard in
  let module C = Count in
  let aux a b =
    let (aTally, bTally) = H.get_tally a, H.get_tally b in
    if C.to_int aTally > C.to_int bTally then Some (H.get_player a)
    else if C.to_int bTally > C.to_int aTally then Some (H.get_player b)
    else None
  in
  let talliedBoard = final_tally board in
  aux talliedBoard.thisSide talliedBoard.otherSide

let print_tally board =
  let aux s =
    Printf.printf "Player ";
    print_player (HalfBoard.get_player s);
    Printf.printf " has %i pieces\n" (HalfBoard.get_tally s |> Count.to_int); in
  let talliedBoard = final_tally board in
  aux talliedBoard.thisSide;
  aux talliedBoard.otherSide

let rec mancala_game board = match is_finished board with
  | false ->
      print_string "\n------------------------------\n";
      print_string "------------------------------\n\n";
      Printf.printf "%s" "Current player: ";
      print_player (HalfBoard.get_player board.thisSide);
      print_newline ();
      print_board board;
      Printf.printf "\nEnter your move (0-5): ";
      let n = read_int () |> Index.of_int in (
      match Mechanics.remove_pieces n board with
      | Some (count, board) -> Mechanics.play n count board |> mancala_game
      | None                -> print_endline "\nThe bowl is empty!";
                               mancala_game board
      )
  | true ->
      print_tally board;
      match winner_is board with
      | None   -> print_endline "The game is a draw.";
      | Some p -> print_string "The winner is ";
                  print_player p;
                  print_endline "."
;;

let () =
  let initBoard = { thisSide = HalfBoard.init One;
                    otherSide = HalfBoard.init Two; }
  in
  mancala_game initBoard
