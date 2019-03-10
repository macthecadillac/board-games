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
        | None -> print_endline "\nThe game is a draw."; exit 0
        | Some p ->
            G.game_end_screen board Release;
            print_string "Player ";
            print_player p;
            print_endline " wins.";
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
        | None -> print_endline "\nThe game is a draw."; exit 0
        | Some p ->
            G.game_end_screen board debug;
            print_string "Player ";
            print_player p;
            print_endline " wins.";
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

  let demo_game nplayouts debug =
    let rec aux nplayouts board =
      if G.is_finished board then
        match G.winner_is board with
        | None -> print_endline "\nThe game is a draw."; exit 0
        | Some p ->
            print_string "\nPlayer ";
            print_player p;
            print_endline " wins.";
            exit 0  (* Don't really know why this is necessary but meh *)
      else
        let aiMove = AI.most_favored_move nplayouts board debug in
        let board' = G.move aiMove board in
        print_string "Player ";
        print_player (G.curr_player board);
        Index.to_int aiMove + 1 |> Printf.printf "'s move: %i\n\n";
        G.print board';
        aux nplayouts board'
    in
    let initGame = G.init () in
    print_newline ();
    G.print initGame;
    aux nplayouts initGame

  let launch_game aigame demo humanSecond nplayouts dbg =
    let _ = Sys.command "clear" in
    let debug = if dbg then Debug else Release in
    match aigame, demo with
    | false, false -> two_player_game ()
    | false, true -> demo_game nplayouts debug
    | true, true -> print_endline "Not a valid combination of options. See help.";
    | true, false ->
        if humanSecond then play_vs_ai nplayouts Two debug
        else play_vs_ai nplayouts One debug

  let aigame =
    let doc = "Play against the computer." in
    Arg.(value & flag & info ["a"] ~doc)

  let demo =
    let doc = "Demo game." in
    Arg.(value & flag & info ["m"] ~doc)

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
    const launch_game $ aigame $ demo $ humanSecond $ nplayouts $ debug

  let run () = Term.exit @@ Term.eval (game_t, info)
end
