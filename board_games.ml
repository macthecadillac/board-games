open Common
open Cmdliner

module MancalaAI = MCTS.Make (Mancala)

type cls = ClearBuffer | KeepBuffer

(* TODO: move into the mancala specific module/functor once that is done since
 * this is not a general function *)
let rec acquire_input () =
  print_string "\nEnter your move (1-6): ";
  let humanMove =
    try read_int () - 1 |> Index.of_int with Failure _ ->
      print_endline "Invalid input.";
      acquire_input ()
  in
  if Index.to_int humanMove > 5 || Index.to_int humanMove < 0 then (
    print_endline "Invalid input.";
    acquire_input ())
  else humanMove

(* TODO: Parameterize two_player_game and play_vs_ai over different board games
 * using a functor *)
let two_player_game () =
  let rec aux board =
    if Mancala.is_finished board then
      match Mancala.winner_is board with
      | None -> print_endline "The game is a draw."; exit 0
      | Some p ->
          print_string "The winner is ";
          print_player p;
          print_endline ".";
          exit 0  (* Don't really know why this is necessary but meh *)
    else
      Printf.printf "Current player: ";
      let s =
        match (Mancala.curr_player board) with
        | One -> "ONE"
        | Two -> "TWO" in
      print_string s;
      print_string "\n\n";
      Mancala.print board;
      let n = acquire_input () in
      let _ = Sys.command "clear" in
      if Mancala.is_valid_move n board then
        let board' = Mancala.move n board in
        aux board'
      else
        print_endline "\nThe bowl is empty!";
        aux board
  in
  let initGame = Mancala.init () in
  aux initGame

let play_vs_ai nplayouts humanSide debug =
  let rec aux clear nplayouts humanSide board =
    let currSide = Mancala.curr_player board in
    if Mancala.is_finished board then
      match Mancala.winner_is board with
      | None -> print_endline "The game is a draw."; exit 0
      | Some p ->
          Mancala.print_tally board;
          print_string "The winner is ";
          print_player p;
          print_endline ".";
          exit 0  (* Don't really know why this is necessary but meh *)
    else
      let _ =
        match clear with
        | KeepBuffer -> ()
        | ClearBuffer ->
            let _ = Sys.command "clear" in
            print_newline ();
            Mancala.print board in
      match (humanSide, currSide) with
      | One, One | Two, Two -> (
          let n = acquire_input () in
          if Mancala.is_valid_move n board then
            let board' = Mancala.move n board in
            print_newline ();
            Mancala.print board';
            let clear' =
              match Mancala.curr_player board', currSide with
              | One, One | Two, Two -> KeepBuffer
              | _ -> ClearBuffer in
            aux clear' nplayouts humanSide board'
          else
            print_endline "\nThe bowl is empty!";
            aux KeepBuffer nplayouts humanSide board)
      | _ ->
          let aiMove = MancalaAI.most_favored_move nplayouts board debug in
          let board' = Mancala.move aiMove board in
          Index.to_int aiMove + 1 |> Printf.printf "\nCOMPUTER MOVE: %i\n\n";
          Mancala.print board';
          aux KeepBuffer nplayouts humanSide board'
  in
  let initGame = Mancala.init () in
  print_newline ();
  Mancala.print initGame;
  aux KeepBuffer nplayouts humanSide initGame

let launch_game mode humanSecond nplayouts dbg =
  let _ = Sys.command "clear" in
  let debug = if dbg then Debug else Release in
  match mode with
  | false -> two_player_game ()
  | true ->
    match humanSecond with
    | true -> play_vs_ai nplayouts Two debug
    | false -> play_vs_ai nplayouts One debug

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
    ^ "the game play. The default is 3000."
  in
  Arg.(value & opt int 3000 & info ["n"] ~docv:"NPLAYOUTS" ~doc)

let info =
  let doc = "A simple implementation of several board games" in
  Term.info "games" ~doc ~exits:Term.default_exits

let debug =
  let doc = "Turn on debug mode" in
  Arg.(value & flag & info ["d"] ~doc)

let game_t =
  let open Term in
  const launch_game $ mode $ humanSecond $ nplayouts $ debug

let () = Term.exit @@ Term.eval (game_t, info)
