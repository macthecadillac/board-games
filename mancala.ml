(* TODO: Add command line arguments using michipili/getopts *)
open Containers
open Common
open Cmdliner

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
      print_string "\n\n";
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


(****************************************************************************)
(***** Parse the commandline and start game with appropriate parameters *****)
(****************************************************************************)

let launch_game mode side numPlayouts = match mode with
  | false -> init_board () |> two_player_game
  | true  -> match side with
      | 1 -> init_board () |> play_vs_ai numPlayouts One
      | 2 -> init_board () |> play_vs_ai numPlayouts Two
      | _ -> print_endline "The only acceptable values for PLAYER are 1 or 2."

let mode =
  let doc = "Play against an AI." in
  Arg.(value & flag & info ["a"] ~doc)

let side =
  let doc = "Player 1 or player 2. Player 1 always goes first. "
          ^ "For AI games only." in
  Arg.(value & opt int 1 & info ["p"; "player"] ~docv:"PLAYER" ~doc)

let numPlayouts =
  let doc = "The number of playouts to be performed per branch. "
          ^ "The higher the number, the stronger the game play." in
  Arg.(value & opt int 200 & info ["n"] ~docv:"NPLAYOUTS" ~doc)

let info =
  let doc = "A simple implementation of the mancala game" in
  Term.info "mancala" ~doc ~exits:Term.default_exits

let game_t = Term.(const launch_game $ mode $ side $ numPlayouts)
let () = Term.exit @@ Term.eval (game_t, info)
