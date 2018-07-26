open Common
open Cmdliner

let rec acquire_input () =
  print_string "\nEnter your move (1-6): " ;
  let humanMove =
    try read_int () - 1 |> Index.of_int with Failure _ ->
      print_endline "Invalid input." ;
      acquire_input ()
  in
  if Index.to_int humanMove > 5 || Index.to_int humanMove < 0 then (
    print_endline "Invalid input." ;
    acquire_input () )
  else humanMove

let two_player_game () =
  let rec aux board =
    match Board.is_finished board with
    | false -> (
        Printf.printf "Current player: " ;
        print_player (Board.curr_player board) ;
        print_string "\n\n" ;
        Board.print board ;
        let n = acquire_input () in
        match Board.is_valid_move n board with
        | true ->
            let newBoard = Board.move n board in
            print_endline "\nAfter your move:\n" ;
            Board.print newBoard ;
            print_endline "\n================================\n" ;
            aux newBoard
        | false ->
            print_endline "\nThe bowl is empty!" ;
            aux board )
    | true ->
        Board.print_tally board ;
        match Board.winner_is board with
        | None -> print_endline "The game is a draw."
        | Some p ->
            print_string "The winner is " ;
            print_player p ;
            print_endline "."
  in
  let initBoard = Board.init () in
  aux initBoard

let play_vs_ai searchLimit humanSide histFile =
  let open Ai.MCSearch in
  let rec aux searchLimit humanSide board histTree =
    let currSide = Board.curr_player board in
    match Board.is_finished board with
    | false -> (
      match (humanSide, currSide) with
      | One, One | Two, Two -> (
          print_endline "\n================================\n" ;
          Board.print board ;
          let n = acquire_input () in
          match Board.is_valid_move n board with
          | true ->
              let newBoard = Board.move n board in
              print_endline "\nAfter your move:\n" ;
              Board.print newBoard ;
              (* print_endline "\n================================\n"; *)
              let _, _, nextBranch = HistTree.choose_branch n histTree in
              aux searchLimit humanSide newBoard nextBranch
          | false ->
              print_endline "\nThe bowl is empty!" ;
              aux searchLimit humanSide board histTree )
      | _ ->
          let aiMove = most_favored_move searchLimit currSide board histTree in
          let newBoard = Board.move aiMove board in
          Index.to_int aiMove + 1 |> Printf.printf "\nCOMPUTER MOVE: %i\n\n" ;
          Board.print newBoard ;
          let _, _, nextBranch = HistTree.choose_branch aiMove histTree in
          aux searchLimit humanSide newBoard nextBranch )
    | true ->
        Board.print_tally board ;
        match Board.winner_is board with
        | None -> print_endline "The game is a draw."
        | Some p ->
            print_string "The winner is " ;
            print_player p ;
            print_endline "."
  in
  let initBoard = Board.init () in
  let histTree = HistTree.load histFile in
  aux searchLimit humanSide initBoard histTree

let ai_vs_ai searchLimit histFile =
  let open Ai.MCSearch in
  let rec aux searchLimit board histTree record =
    let currSide = Board.curr_player board in
    match Board.is_finished board with
    | false ->
        let aiMove = most_favored_move searchLimit currSide board histTree in
        let newBoard = Board.move aiMove board in
        print_string "\nComputer " ;
        Board.curr_player board |> print_player ;
        Index.to_int aiMove + 1 |> Printf.printf " move: %i\n\n" ;
        Board.print newBoard ;
        let _, _, nextBranch = HistTree.choose_branch aiMove histTree in
        aux searchLimit newBoard nextBranch (aiMove :: record)
    | true ->
        Board.print_tally board ;
        let winner = Board.winner_is board in
        match winner with
        | None -> print_endline "The game is a draw."
        | Some p ->
            print_string "The winner is " ;
            print_player p ;
            print_endline "."
  in
  let histTree = HistTree.load histFile in
  let initBoard = Board.init () in
  aux searchLimit initBoard histTree []

let train_ai searchLimit histFile =
  let open Ai.MCSearch in
  let rec game i histTree =
    (* firstMove tells the program whether the current move is the first move of
     * the player. This is to ensure the program explores all possible first
     * moves so it doesn't get blindsided when encountering outlandish openings.
     * *)
    let rec aux searchLimit board histTree record n =
      let currSide = Board.curr_player board in
      match Board.is_finished board with
      | false ->
          let aiMove =
            (* most_favored_move searchLimit currSide board histTree in *)
          if n = 0 then Index.of_int (i mod 6)
          else most_favored_move searchLimit currSide board histTree in
          if n = 0 then Printf.printf "\nMandatory move: %i\n\n" (i mod 6);
          let newBoard = Board.move aiMove board in
          let _, _, nextBranch = HistTree.choose_branch aiMove histTree in
          aux searchLimit newBoard nextBranch (aiMove :: record) (n + 1)
      | true ->
          let winner = Board.winner_is board in
          (winner, record)
    in
    let initBoard = Board.init () in
    let winner, revRecord = aux searchLimit initBoard histTree [] 0 in
    (* records are reversed because of the way lists are constructed *)
    let record = List.rev revRecord in
    let histTree = HistTree.update_from_last_playout winner histTree record in
    HistTree.save histTree histFile ;
    List.iter (fun x -> Printf.printf "%i " (Index.to_int x)) record ;
    print_newline () ;
    Printf.printf "Made %i moves\n" (List.length record) ;
    Printf.printf "histTree depth = %i\n" (HistTree.depth histTree) ;
    Printf.printf "Finished round %i" i ;
    print_newline () ;
    print_newline () ;
    (* print_newline (); *)
    game (i + 1) histTree
  in
  let histTree = HistTree.load histFile in
  Printf.printf "histTree depth = %i\n" (HistTree.depth histTree) ;
  print_newline () ;
  game 0 histTree

(****************************************************************************)
(***** Parse the commandline and start game with appropriate parameters *****)
(****************************************************************************)

let launch_game mode aivai training isMiniMax humanSecond nplayouts searchDepth
    histFile =
  let open Ai in
  match training with
  | true -> train_ai 20 histFile
  | false ->
    match aivai with
    | true -> ai_vs_ai nplayouts histFile
    | false ->
      match mode with
      | false -> two_player_game ()
      | true ->
        match (isMiniMax, humanSecond) with
        | false, false -> play_vs_ai nplayouts One histFile
        | false, true -> play_vs_ai nplayouts Two histFile
        | true, false -> () (* currently unavailable *)
        | true, true -> ()

(* in game (Board.init ()) *)

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

let histFile =
  let doc = "Pick a history file for the AI." in
  Arg.(value & opt string "histtree.cache" & info ["h"] ~doc)

let nplayouts =
  let doc =
    "The number of playouts to be performed per branch with "
    ^ "the Monte Carlo AI. The higher the number, the stronger "
    ^ "the game play."
  in
  Arg.(value & opt int 30 & info ["n"] ~docv:"NPLAYOUTS" ~doc)

let searchDepth =
  let doc =
    "The search depth of the minimax algorithm. The deeper down "
    ^ "the game tree the AI searches, the stronger the game play."
  in
  Arg.(value & opt int 6 & info ["d"] ~docv:"SEARCHDEPTH" ~doc)

let info =
  let doc = "A simple implementation of the mancala game" in
  Term.info "mancala" ~doc ~exits:Term.default_exits

let game_t =
  let open Term in
  const launch_game $ mode $ aivai $ training $ isMiniMax $ humanSecond
  $ nplayouts $ searchDepth $ histFile

let () = Term.exit @@ Term.eval (game_t, info)
