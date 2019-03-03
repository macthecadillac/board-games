open Common
open Cmdliner

module MancalaAI = MCTS.Make (Mancala)

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

(* TODO: Parameterize two_player_game and play_vs_ai over different board games
 * using a functor *)
let two_player_game () =
  let rec aux board =
    if Mancala.is_finished board then (
      (* Mancala.print board ; *)
      match Mancala.winner_is board with
      | None -> print_endline "The game is a draw."
      | Some p ->
          print_string "The winner is " ;
          print_player p ;
          print_endline ".")
    else (
      Printf.printf "Current player: " ;
      print_player (Mancala.curr_player board) ;
      print_string "\n\n" ;
      Mancala.print board ;
      let n = acquire_input () in
      if Mancala.is_valid_move n board then
        let board' = Mancala.move n board in
        print_endline "\nAfter your move:\n" ;
        Mancala.print board' ;
        print_endline "\n================================\n" ;
        aux board'
      else
        print_endline "\nThe bowl is empty!" ;
        aux board)
  in
  let initGame = Mancala.init () in
  aux initGame

let play_vs_ai nplayouts humanSide =
  let rec aux nplayouts humanSide board =
    let currSide = Mancala.curr_player board in
    if Mancala.is_finished board then
      match Mancala.winner_is board with
      | None -> print_endline "The game is a draw."
      | Some p ->
          Mancala.print_tally board;
          print_string "The winner is " ;
          print_player p ;
          print_endline ".";
    else
      match (humanSide, currSide) with
      | One, One | Two, Two -> (
          print_endline "\n================================\n" ;
          Mancala.print board ;
          let n = acquire_input () in
          if Mancala.is_valid_move n board then
            let board' = Mancala.move n board in
            print_endline "\nAfter your move:\n" ;
            Mancala.print board' ;
            aux nplayouts humanSide board'
          else
            print_endline "\nThe bowl is empty!" ;
            aux nplayouts humanSide board)
      | _ ->
          let aiMove = MancalaAI.most_favored_move nplayouts board in
          let board' = Mancala.move aiMove board in
          Index.to_int aiMove + 1 |> Printf.printf "\nCOMPUTER MOVE: %i\n\n" ;
          Mancala.print board' ;
          aux nplayouts humanSide board'
  in
  let initGame = Mancala.init () in
  aux nplayouts humanSide initGame

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

let mode =
  let doc = "Play against the computer." in
  Arg.(value & flag & info ["a"] ~doc)

let humanSecond =
  let doc = "Let the computer make the first move." in
  Arg.(value & flag & info ["s"] ~doc)

let nplayouts =
  let doc =
    "The number of playouts to be performed with "
    ^ "the Monte Carlo AI. The higher the number, the stronger "
    ^ "the game play. The default is 1600."
  in
  Arg.(value & opt int 1600 & info ["n"] ~docv:"NPLAYOUTS" ~doc)

let info =
  let doc = "A simple implementation of the mancala game" in
  Term.info "mancala" ~doc ~exits:Term.default_exits

let game_t =
  let open Term in
  const launch_game $ mode $ humanSecond $ nplayouts

let () = Term.exit @@ Term.eval (game_t, info)
