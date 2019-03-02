open Common
open Cmdliner

module MancalaGame = MCTS.Make (Mancala)

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
    match MancalaGame.is_finished board with
    | false -> (
        Printf.printf "Current player: " ;
        print_player (MancalaGame.curr_player board) ;
        print_string "\n\n" ;
        MancalaGame.print board ;
        let n = acquire_input () in
        if MancalaGame.is_valid_move n board then
          let newMancalaGame = MancalaGame.move n board in
          print_endline "\nAfter your move:\n" ;
          MancalaGame.print newMancalaGame ;
          print_endline "\n================================\n" ;
          aux newMancalaGame
        else
          print_endline "\nThe bowl is empty!" ;
          aux board )
    | true ->
        MancalaGame.print board ;
        match MancalaGame.winner_is board with
        | None -> print_endline "The game is a draw."
        | Some p ->
            print_string "The winner is " ;
            print_player p ;
            print_endline "."
  in
  let initMancalaGame = MancalaGame.init () in
  aux initMancalaGame

let play_vs_ai searchLimit humanSide =
  let rec aux searchLimit humanSide board =
    let currSide = MancalaGame.curr_player board in
    if MancalaGame.is_finished board then
      match (humanSide, currSide) with
      | One, One | Two, Two -> (
          print_endline "\n================================\n" ;
          MancalaGame.print board ;
          let n = acquire_input () in
          if MancalaGame.is_valid_move n board then
            let newMancalaGame = MancalaGame.move n board in
            print_endline "\nAfter your move:\n" ;
            MancalaGame.print newMancalaGame ;
            aux searchLimit humanSide newMancalaGame
          else
            print_endline "\nThe bowl is empty!" ;
            aux searchLimit humanSide board)
      | _ ->
          let aiMove = MancalaGame.most_favored_move searchLimit board in
          let newMancalaGame = MancalaGame.move aiMove board in
          Index.to_int aiMove + 1 |> Printf.printf "\nCOMPUTER MOVE: %i\n\n" ;
          MancalaGame.print newMancalaGame ;
          aux searchLimit humanSide newMancalaGame
    else
      MancalaGame.print board ;
      match MancalaGame.winner_is board with
      | None -> print_endline "The game is a draw."
      | Some p ->
          print_string "The winner is " ;
          print_player p ;
          print_endline "."
  in
  let initMancalaGame = MancalaGame.init () in
  aux searchLimit humanSide initMancalaGame

(****************************************************************************)
(***** Parse the commandline and start game with appropriate parameters *****)
(****************************************************************************)

let launch_game mode humanSecond nplayouts =
  match mode with
  | false -> two_player_game ()
  | true ->
    match humanSecond with
    | true -> play_vs_ai nplayouts Two
    | false -> play_vs_ai nplayouts One

(* in game (MancalaGame.init ()) *)

let mode =
  let doc = "Play against the computer." in
  Arg.(value & flag & info ["a"] ~doc)

let humanSecond =
  let doc = "Let the computer make the first move." in
  Arg.(value & flag & info ["s"] ~doc)

let nplayouts =
  let doc =
    "The number of playouts to be performed per branch with "
    ^ "the Monte Carlo AI. The higher the number, the stronger "
    ^ "the game play."
  in
  Arg.(value & opt int 30 & info ["n"] ~docv:"NPLAYOUTS" ~doc)

let info =
  let doc = "A simple implementation of the mancala game" in
  Term.info "mancala" ~doc ~exits:Term.default_exits

let game_t =
  let open Term in
  const launch_game $ mode $ humanSecond $ nplayouts

let () = Term.exit @@ Term.eval (game_t, info)
