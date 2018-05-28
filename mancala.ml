open Containers
open Common

let init_board () = { thisSide = HalfBoard.init One;
                      otherSide = HalfBoard.init Two; }

let print_tally board =
  let aux s =
    Printf.printf "Player ";
    print_player (HalfBoard.get_player s);
    Printf.printf " has %i pieces\n" (HalfBoard.get_tally s |> Count.to_int); in
  let talliedBoard = Mechanics.final_tally board in
  aux talliedBoard.thisSide;
  aux talliedBoard.otherSide

let rec two_player_game board = match Mechanics.is_finished board with
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
      | Some (count, board) -> Mechanics.play n count board |> two_player_game
      | None                -> print_endline "\nThe bowl is empty!";
                               two_player_game board
      )
  | true ->
      print_tally board;
      match Mechanics.winner_is board with
      | None   -> print_endline "The game is a draw.";
      | Some p -> print_string "The winner is ";
                  print_player p;
                  print_endline "."

let rec play_vs_ai searchLimit humanSide board =
  let currSide = HalfBoard.get_player board.thisSide in
  match Mechanics.is_finished board with
  | false -> (match humanSide, currSide with
      | One, One | Two, Two ->
        print_board board;
        Printf.printf "\nEnter your move (0-5): ";
        let humanMove = read_int () |> Index.of_int in (
        match Mechanics.remove_pieces humanMove board with
          | Some (count, newBoard) ->
              let b = Mechanics.play humanMove count newBoard in
              play_vs_ai searchLimit humanSide b
          | None                -> print_endline "\nThe bowl is empty!";
                                   play_vs_ai searchLimit humanSide board)
      | _ ->
        let aiMove = Engine.most_favored_move searchLimit currSide board in
        (match Mechanics.remove_pieces aiMove board with
          | Some (count, newBoard) ->
              let b = Mechanics.play aiMove count newBoard in
              play_vs_ai searchLimit humanSide b
          | None                -> print_endline "\nThe bowl is empty!";
                                   play_vs_ai searchLimit humanSide board))
  | true  ->
      print_tally board;
      match Mechanics.winner_is board with
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
