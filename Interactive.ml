open Common
open Cmdliner

module type S = sig
  val run : unit -> unit
end

module Make (G : GAME) = struct
  module AI = MCTS.Make(G)

  let rec acquire_input board =
    print_string "\nEnter your move: ";
    let humanMove =
      match read_int_opt () with
      | Some i -> Index.of_int (i - 1)
      | None -> 
          print_endline "Invalid input.";
          acquire_input board in
    if List.mem humanMove (G.available_moves board) then humanMove
    else (
      print_endline "Not a valid move.";
      acquire_input board)

  let two_player_game () =
    let rec aux board =
      if G.is_finished board then
        match G.winner_is board with
        | None -> print_endline "The game is a draw."; exit 0
        | Some p ->
            G.game_end_screen board;
            print_string "The winner is ";
            print_player p;
            print_endline ".";
            exit 0  (* Don't really know why this is necessary but meh *)
      else
        Printf.printf "Current player: ";
        let s =
          match (G.curr_player board) with
          | One -> "ONE"
          | Two -> "TWO" in
        print_string s;
        print_string "\n\n";
        G.print board;
        let n = acquire_input board in
        let _ = Sys.command "clear" in
        if G.is_valid_move n board then
          let board' = G.move n board in
          aux board'
        else
          print_endline "\nThe bowl is empty!";
          aux board
    in
    let initGame = G.init () in
    aux initGame

  let play_vs_ai nplayouts humanSide debug =
    let rec aux clear nplayouts humanSide board =
      let currSide = G.curr_player board in
      if G.is_finished board then
        match G.winner_is board with
        | None -> print_endline "The game is a draw."; exit 0
        | Some p ->
            G.game_end_screen board;
            print_string "The winner is ";
            print_player p;
            print_endline ".";
            exit 0  (* Don't really know why this is necessary but meh *)
      else
        let _ =
          match clear, debug with
          | `KeepBuffer, _ -> ()
          | `ClearBuffer, Debug -> ()
          | `ClearBuffer, Release ->
              let _ = Sys.command "clear" in
              print_newline ();
              G.print board in
        match (humanSide, currSide) with
        | One, One | Two, Two -> (
            let n = acquire_input board in
            if G.is_valid_move n board then
              let board' = G.move n board in
              print_newline ();
              G.print board';
              let clear' =
                match G.curr_player board', currSide with
                | One, One | Two, Two -> `KeepBuffer
                | _ -> `ClearBuffer in
              aux clear' nplayouts humanSide board'
            else
              print_endline "\nThe bowl is empty!";
              aux `KeepBuffer nplayouts humanSide board)
        | _ ->
            let aiMove = AI.most_favored_move nplayouts board debug in
            let board' = G.move aiMove board in
            Index.to_int aiMove + 1 |> Printf.printf "\nCOMPUTER MOVE: %i\n\n";
            G.print board';
            aux `KeepBuffer nplayouts humanSide board'
    in
    let initGame = G.init () in
    print_newline ();
    G.print initGame;
    aux `KeepBuffer nplayouts humanSide initGame

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
    let doc = "A simple implementation of " ^ G.name in
    Term.info G.name ~doc ~exits:Term.default_exits

  let debug =
    let doc = "Turn on debug mode" in
    Arg.(value & flag & info ["d"] ~doc)

  let game_t =
    let open Term in
    const launch_game $ mode $ humanSecond $ nplayouts $ debug

  let run () = Term.exit @@ Term.eval (game_t, info)
end
