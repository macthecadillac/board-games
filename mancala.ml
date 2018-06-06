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

let rec play_vs_ai searchLimit humanSide aifun board =
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
              play_vs_ai searchLimit humanSide aifun b
          | None                   -> print_endline "\nThe bowl is empty!\n";
                                      play_vs_ai searchLimit humanSide
                                                 aifun board)
      | _ ->
        let aiMove = aifun searchLimit currSide board in
        (* Printf.printf "%i\n" (Index.to_int aiMove); *)
        (match Board.remove_pieces aiMove board with
          | Some (count, newBoard) ->
              let b = Board.dist (Index.inc aiMove) count newBoard in
              play_vs_ai searchLimit humanSide aifun b
          | None                   ->
              print_endline "Something is wrong")) (* Impossible branch *)
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

let launch_game mode isMiniMax humanSecond nplayouts searchDepth =
  let open Ai in
  let game =
    match mode with
    | false -> two_player_game
    | true  ->
        match isMiniMax, humanSecond with
        | false, false -> play_vs_ai searchDepth One MCSearch.most_favored_move
        | false, true  -> play_vs_ai searchDepth Two MCSearch.most_favored_move
        | true, false  -> play_vs_ai nplayouts One MiniMax.most_favored_move
        | true, true   -> play_vs_ai nplayouts Two MiniMax.most_favored_move
  in game (init_board ())

let mode =
  let doc = "Play against an AI." in
  Arg.(value & flag & info ["a"] ~doc)

let isMiniMax =
  let doc = "Opt for the Minimax AI." in
  Arg.(value & flag & info ["m"] ~doc)

let humanSecond =
  let doc = "Let the AI make the first move." in
  Arg.(value & flag & info ["s"] ~doc)

let nplayouts =
  let doc = "The number of playouts to be performed per branch with "
          ^ "the Monte Carlo AI. The higher the number, the stronger "
          ^ "the game play." in
  Arg.(value & opt int 30 & info ["n"] ~docv:"NPLAYOUTS" ~doc)

let searchDepth =
  let doc = "The search depth of the minimax algorithm. The deeper down "
          ^ "the game tree the AI searches, the stronger the game play." in
  Arg.(value & opt int 6 & info ["d"] ~docv:"SEARCHDEPTH" ~doc)

let info =
  let doc = "A simple implementation of the mancala game" in
  Term.info "mancala" ~doc ~exits:Term.default_exits

let game_t =
  Term.(const launch_game $ mode $ isMiniMax $ humanSecond $ nplayouts $
        searchDepth)
let () = Term.exit @@ Term.eval (game_t, info)
