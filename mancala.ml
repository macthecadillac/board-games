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

let two_player_game () =
  let rec aux board = match Board.is_finished board with
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
              aux newBoard
          | false -> print_endline "\nThe bowl is empty!";
                     aux board)
    | true ->
        print_tally board;
        match Board.winner_is board with
          None   -> print_endline "The game is a draw.";
        | Some p -> print_string "The winner is ";
                    print_player p;
                    print_endline "."
  in
  let initBoard = init_board () in
  aux initBoard

let play_vs_ai searchLimit humanSide =
  let open Ai.MCSearch in
  let rec aux searchLimit humanSide board histTree =
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
                  (* print_endline "\n================================\n"; *)
                  aux searchLimit humanSide newBoard histTree
              | false -> print_endline "\nThe bowl is empty!";
                         aux searchLimit humanSide board histTree)
        | _ ->
            let aiMove = most_favored_move searchLimit currSide board histTree in
            let newBoard = Board.move aiMove board in
            Index.to_int aiMove + 1 |> Printf.printf "\nCOMPUTER MOVE: %i\n\n";
            Board.print newBoard;
            aux searchLimit humanSide newBoard histTree)
    | true  ->
        print_tally board;
        match Board.winner_is board with
          None   -> print_endline "The game is a draw.";
        | Some p -> print_string "The winner is ";
                    print_player p;
                    print_endline "."
  in
  let initBoard = init_board () in
  let histTree = FavHistTree.load "histtree.cache" in
  aux searchLimit humanSide initBoard histTree

let ai_vs_ai searchLimit =
  let open Ai.MCSearch in
  let rec aux searchLimit board histTree record =
    let currSide = Board.curr_player board in
    match Board.is_finished board with
      false ->
        let aiMove = most_favored_move searchLimit currSide board histTree in
        let newBoard = Board.move aiMove board in
        print_string "\nComputer ";
        Board.curr_player board |> print_player;
        Index.to_int aiMove + 1 |> Printf.printf " move: %i\n\n";
        Board.print newBoard;
        let _, nextBranch = FavHistTree.choose_branch aiMove histTree in
        aux searchLimit newBoard nextBranch (aiMove :: record)
    | true  ->
        print_tally board;
        let winner = Board.winner_is board in
        (match winner with
            None   -> print_endline "The game is a draw.";
          | Some p -> print_string "The winner is ";
                      print_player p;
                      print_endline ".");
  in
  let histTree = FavHistTree.load "histtree.cache" in
  let initBoard = init_board () in
  aux searchLimit initBoard histTree []

let train_ai searchLimit =
  let open Ai.MCSearch in
  let rec game i histTree =
    let rec aux searchLimit board histTree record =
      let currSide = Board.curr_player board in
      match Board.is_finished board with
        false ->
          let aiMove = most_favored_move searchLimit currSide board histTree in
          let newBoard = Board.move aiMove board in
          let _, nextBranch = FavHistTree.choose_branch aiMove histTree in
          aux searchLimit newBoard nextBranch (aiMove :: record)
      | true  ->
          let winner = Board.winner_is board in
          winner, record
    in
    let initBoard = init_board () in
    let winner, record = aux searchLimit initBoard histTree [] in
    let histTree = FavHistTree.update_from_last_playout winner histTree record in
    FavHistTree.save histTree "histtree.cache";
    print_newline ();
    List.iter (fun x -> Printf.printf "%i " (Index.to_int x)) record;
    print_newline ();
    Printf.printf "Made %i moves\n" (List.length record);
    Printf.printf "histTree depth = %i\n" (FavHistTree.depth histTree);
    Printf.printf "Finished round %i" i;
    print_newline ();
    (* print_newline (); *)
    game (i + 1) histTree
  in
  let histTree = FavHistTree.load "histtree.cache" in
  Printf.printf "histTree depth = %i\n" (FavHistTree.depth histTree);
  print_newline ();
  game 0 histTree



(****************************************************************************)
(***** Parse the commandline and start game with appropriate parameters *****)
(****************************************************************************)

let launch_game mode aivai training isMiniMax humanSecond nplayouts searchDepth =
  let open Ai in
  match training with
    true  -> train_ai 20
  | false ->
      (match aivai with
        true  -> ai_vs_ai nplayouts
      | false ->
        (match mode with
          false -> two_player_game ()
        | true  ->
            (match isMiniMax, humanSecond with
              false, false -> play_vs_ai nplayouts One
            | false, true  -> play_vs_ai nplayouts Two
            | true, false  -> ()
            | true, true   -> ())))
(* in game (init_board ()) *)

let mode =
  let doc = "Play against the computer." in
  Arg.(value & flag & info ["a"] ~doc)

let aivai =
  let doc = "Make the computer play itself." in
  Arg.(value & flag & info ["v"] ~doc)

let training =
  let doc = "Train the computer player." in
  Arg.(value & flag & info ["t"] ~doc)

let isMiniMax =
  let doc = "Opt for the Minimax AI." in
  Arg.(value & flag & info ["m"] ~doc)

let humanSecond =
  let doc = "Let the computer make the first move." in
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
  Term.(const launch_game $ mode $ aivai $ training $ isMiniMax $ humanSecond $
        nplayouts $ searchDepth)
let () = Term.exit @@ Term.eval (game_t, info)
