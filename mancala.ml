open Common
open Cmdliner

let init_board () = Board.build (HalfBoard.init One) (HalfBoard.init Two)

let print_tally halfBoard =
  let aux s =
    Printf.printf "Player ";
    print_player (HalfBoard.get_player s);
    Printf.printf " has %i pieces\n" (HalfBoard.get_tally s |> Count.to_int); in
  let talliedBoard = Board.final_tally halfBoard in
  print_newline ();
  aux (Board.curr_side talliedBoard);
  aux (Board.other_side talliedBoard)

let rec acquire_input () =
  print_string "\nEnter your move (1-6): ";
  let humanMove =
    try read_int () - 1 |> Index.of_int
    with Failure _ ->
      print_endline "Invalid input.";
      acquire_input () in
  if Index.to_int humanMove > 5 || Index.to_int humanMove < 0 then
    (print_endline "Invalid input.";
    acquire_input ())
  else humanMove

let rec two_player_game board = match Board.is_finished board with
    false ->
      Printf.printf "Current player: ";
      print_player (Board.curr_player board);
      print_string "\n\n";
      Board.print board;
      let n = acquire_input () in
      (match Board.is_valid_move n board with
          true  ->
            let newBoard = Board.move n board in
            print_endline "\nAfter your move:\n";
            Board.print newBoard;
            print_endline "\n================================\n";
            two_player_game newBoard
        | false -> print_endline "\nThe bowl is empty!";
                   two_player_game board)
  | true ->
      print_tally board;
      match Board.winner_is board with
        None   -> print_endline "The game is a draw.";
      | Some p -> print_string "The winner is ";
                  print_player p;
                  print_endline "."

let rec play_vs_ai searchLimit humanSide aifun board =
  let currSide = Board.curr_player board in
  match Board.is_finished board with
    false -> (match humanSide, currSide with
        One, One | Two, Two ->
          print_endline "\n================================\n";
          Board.print board;
          let n = acquire_input () in
          (match Board.is_valid_move n board with
              true  ->
                let newBoard = Board.move n board in
                print_endline "\nAfter your move:\n";
                Board.print newBoard;
                print_endline "\n================================\n";
                play_vs_ai searchLimit humanSide aifun newBoard
            | false -> print_endline "\nThe bowl is empty!";
                       play_vs_ai searchLimit humanSide aifun board)
      | _ ->
          let aiMove = aifun searchLimit currSide board in
          let newBoard = Board.move aiMove board in
          Index.to_int aiMove + 1 |> Printf.printf "\nAI MOVE: %i\n\n";
          Board.print newBoard;
          play_vs_ai searchLimit humanSide aifun newBoard)
  | true  ->
      print_tally board;
      match Board.winner_is board with
        None   -> print_endline "The game is a draw.";
      | Some p -> print_string "The winner is ";
                  print_player p;
                  print_endline "."

let rec ai_vs_ai searchLimit aifun board =
  let currSide = Board.curr_player board in
  match Board.is_finished board with
    false ->
      let aiMove = aifun searchLimit currSide board in
      let newBoard = Board.move aiMove board in
      print_string "\nAI ";
      Board.curr_player board |> print_player;
      Index.to_int aiMove + 1 |> Printf.printf " move: %i\n\n";
      Board.print newBoard;
      ai_vs_ai searchLimit aifun newBoard
  | true  ->
      print_tally board;
      match Board.winner_is board with
        None   -> print_endline "The game is a draw.";
      | Some p -> print_string "The winner is ";
                  print_player p;
                  print_endline "."



(****************************************************************************)
(***** Parse the commandline and start game with appropriate parameters *****)
(****************************************************************************)

let launch_game mode aivai isMiniMax humanSecond nplayouts searchDepth =
  let open Ai in
  let game =
    match aivai with
      true  -> ai_vs_ai nplayouts MCSearch.most_favored_move
    | false ->
      match mode with
        false -> two_player_game
      | true  ->
          match isMiniMax, humanSecond with
            false, false -> play_vs_ai nplayouts One MCSearch.most_favored_move
          | false, true  -> play_vs_ai nplayouts Two MCSearch.most_favored_move
          | true, false  -> play_vs_ai searchDepth One MiniMax.most_favored_move
          | true, true   -> play_vs_ai searchDepth Two MiniMax.most_favored_move
  in game (init_board ())

let mode =
  let doc = "Play against an AI." in
  Arg.(value & flag & info ["a"] ~doc)

let aivai =
  let doc = "Make the computer play itself." in
  Arg.(value & flag & info ["v"] ~doc)

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
  Term.(const launch_game $ mode $ aivai $ isMiniMax $ humanSecond $
        nplayouts $ searchDepth)
let () = Term.exit @@ Term.eval (game_t, info)
