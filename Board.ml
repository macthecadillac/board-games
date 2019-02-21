open Containers
open Common

module HalfBoard : sig
  type t
  val init : player -> t
  val copy : t -> t
  val get_player : t -> player
  val get_tally : t -> Count.t
  val get_pit : Index.t -> t -> Count.t
  val bump_pit : Index.t -> t -> t
  val zero_pit : Index.t -> t -> t
  val bump_tally : Count.t -> t -> t
  val nth : t -> Index.t -> Count.t
  val is_empty : t -> bool
  val clear_board : t -> t
  val rm_pieces : Index.t -> t -> Count.t * t
  val pits_repr : t -> String.t
end = struct
  type t = {player: player; tally: Count.t; pits: Count.t array}

  let init player =
    { player;
      tally = Count.of_int 0;
      pits = Array.init 6 (fun _ -> Count.of_int 4) }

  let copy halfBoard = { halfBoard with pits = Array.copy halfBoard.pits }

  let get_player halfBoard = halfBoard.player

  let get_tally halfBoard = halfBoard.tally

  let get_pit indx halfBoard = (halfBoard.pits).(Index.to_int indx)

  let set_pit c indx halfBoard =
    (halfBoard.pits).(Index.to_int indx) <- c;
    halfBoard

  let bump_pit indx halfBoard =
    let i = Index.to_int indx in
    (halfBoard.pits).(i) <- Count.inc (halfBoard.pits).(i);
    halfBoard

  let zero_pit = set_pit (Count.of_int 0)

  let bump_tally n halfBoard =
    { halfBoard with tally = Count.(halfBoard.tally + n) }

  let nth halfBoard indx = (halfBoard.pits).(Index.to_int indx)

  let is_empty halfBoard =
    Array.fold (fun a b -> Count.to_int b = 0 && a) true halfBoard.pits

  let clear_board halfBoard =
    { halfBoard with
      tally =
        (let open Count in
        halfBoard.tally + Array.fold_right ( + ) halfBoard.pits (of_int 0));
      pits= Array.map (fun _ -> Count.of_int 0) halfBoard.pits }

  let rm_pieces n halfBoard =
    let n = Index.to_int n in
    let count = (halfBoard.pits).(n) in
    (halfBoard.pits).(n) <- Count.of_int 0;
    (count, {halfBoard with pits= halfBoard.pits})

  let pits_repr halfBoard =
    let rawRepr =
      match halfBoard.player with
      | One -> halfBoard.pits
      | Two -> Array.rev halfBoard.pits
    in
    let pad b a =
      let cnt = Count.to_int b in
      if cnt < 10 then string_of_int cnt ^ "   " ^ a
      else string_of_int cnt ^ "  " ^ a
    in
    "  " ^ Array.fold_right pad rawRepr ""
end

type t = { currSide: HalfBoard.t; otherSide: HalfBoard.t }

let copy board =
  { currSide = HalfBoard.copy board.currSide;
    otherSide = HalfBoard.copy board.otherSide }

let init () =
  { currSide = HalfBoard.init One;
    otherSide = HalfBoard.init Two }

let curr_player board = HalfBoard.get_player board.currSide

let bump_pit_curr n b = { b with currSide = HalfBoard.bump_pit n b.currSide }

let bump_pit_other n b = { b with otherSide = HalfBoard.bump_pit n b.otherSide }

let bump_tally b =
  { b with currSide = HalfBoard.bump_tally (Count.of_int 1) b.currSide }

let switch_sides board =
  { currSide = board.otherSide;
    otherSide = board.currSide }

let remove_pieces n board =
  let oldCurrSide = HalfBoard.copy board.currSide in
  let count, currSide = HalfBoard.rm_pieces n oldCurrSide in
  if Count.to_int count > 0 then Some (count, { board with currSide })
  else None

let dist n count board =
  let module H = HalfBoard in
  let module I = Index in
  let module C = Count in

  let rec dispense n cnt board =
    if C.to_int cnt = 0 then
      let finalIndx = I.to_int (I.dec n) mod 13 |> I.of_int in
      (finalIndx, board)
    else
      let i = I.to_int n mod 13 in
      (* Printf.printf "%i\n" i; *)
      let indxMod = I.of_int i in
      let nextN = I.inc n and nextCnt = C.dec cnt in
      if i <= 5 then dispense nextN nextCnt (bump_pit_curr indxMod board)
      else if i = 6 then dispense nextN nextCnt (bump_tally board)
      else
        dispense nextN nextCnt (bump_pit_other I.(indxMod - of_int 7) board) in

  let finalIndx, newBoard = dispense n count (copy board) in
  (* Printf.printf "\n%i\n" (Index.to_int finalIndx); *)
  if I.to_int finalIndx <= 5 then
    if C.to_int (H.get_pit finalIndx newBoard.currSide) = 1 then
      let captureIndx = I.(of_int 5 - finalIndx) in
      let capturedPieces = H.get_pit captureIndx newBoard.otherSide in
      let otherSide = H.zero_pit captureIndx newBoard.otherSide
      and currSide = H.bump_tally capturedPieces newBoard.currSide in
      { currSide; otherSide }
      |> switch_sides
    else switch_sides newBoard
  else if I.to_int finalIndx = 6 then newBoard
  else switch_sides newBoard

let move indx board =
  match remove_pieces indx board with
  | None -> print_endline "Empty pit!"; board
  | Some (count, b) -> dist (Index.inc indx) count b

let is_valid_move indx board =
  match Count.to_int (HalfBoard.nth board.currSide indx) with
  | 0 -> false
  | _ -> true

let available_moves board =
  let rec filter indx =
    if Index.to_int indx > 5 then []
    else
      match is_valid_move indx board with
      | true -> indx :: filter (Index.inc indx)
      | false -> filter (Index.inc indx)
  in
  filter (Index.of_int 0)

let is_finished board =
  HalfBoard.is_empty board.currSide || HalfBoard.is_empty board.otherSide

let get_curr_player_tally board = HalfBoard.get_tally board.currSide

let get_other_player_tally board = HalfBoard.get_tally board.otherSide

let final_tally board =
  let currSide = HalfBoard.clear_board board.currSide
  and otherSide = HalfBoard.clear_board board.otherSide in
  { currSide; otherSide }

let winner_is board =
  let module H = HalfBoard in
  let module C = Count in
  let aux a b =
    let tallyOne, tallyTwo = (H.get_tally a, H.get_tally b) in
    if C.to_int tallyOne > C.to_int tallyTwo then Some (H.get_player a)
    else if C.to_int tallyTwo > C.to_int tallyOne then Some (H.get_player b)
    else None
  in
  let talliedBoard = final_tally board in
  aux talliedBoard.currSide talliedBoard.otherSide

let print b =
  let sideOneRepr, sideTwoRepr, sideOneTally, sideTwoTally =
    match curr_player b with
    | One ->
        ( HalfBoard.pits_repr b.currSide,
          HalfBoard.pits_repr b.otherSide,
          HalfBoard.get_tally b.currSide,
          HalfBoard.get_tally b.otherSide )
    | Two ->
        ( HalfBoard.pits_repr b.otherSide,
          HalfBoard.pits_repr b.currSide,
          HalfBoard.get_tally b.otherSide,
          HalfBoard.get_tally b.currSide )
  in
  let boardLen =
    Int.max (String.length sideOneRepr) (String.length sideTwoRepr)
  in
  Printf.printf "    ";
  print_endline sideTwoRepr;
  let spaces = String.repeat " " boardLen in
  Printf.printf "%i    %s  %i\n"
    (sideTwoTally |> Count.to_int)
    spaces
    (sideOneTally |> Count.to_int);
  Printf.printf "    ";
  print_endline sideOneRepr

let print_tally board =
  let aux s =
    Printf.printf "Player ";
    print_player (HalfBoard.get_player s);
    Printf.printf " has %i pieces\n" (HalfBoard.get_tally s |> Count.to_int)
  in
  let talliedBoard = final_tally board in
  print_newline ();
  aux talliedBoard.currSide;
  aux talliedBoard.otherSide
