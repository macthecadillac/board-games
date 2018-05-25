open Containers
open Common

let print_tally board =
  let aux s =
    Printf.printf "Player ";
    print_player (HalfBoard.get_player s);
    Printf.printf " has %i pieces\n" (HalfBoard.get_tally s |> Count.to_int); in
  let talliedBoard = Mechanics.final_tally board in
  aux talliedBoard.thisSide;
  aux talliedBoard.otherSide

let rec mancala_game board = match Mechanics.is_finished board with
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
      match Mechanics.winner_is board with
      | None   -> print_endline "The game is a draw.";
      | Some p -> print_string "The winner is ";
                  print_player p;
                  print_endline "."

let init_board () = { thisSide = HalfBoard.init One;
                      otherSide = HalfBoard.init Two; }
;;

let () =
  init_board () |> mancala_game
