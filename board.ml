open Containers
open Common

type t = {
  currSide  : HalfBoard.t;
  otherSide : HalfBoard.t;
}

let build currSide otherSide = { currSide; otherSide }

let curr_player board = HalfBoard.get_player board.currSide

let curr_side board = board.currSide

let other_side board = board.otherSide

let set_curr_side currSide board = { board with currSide }

let set_other_side otherSide board = { board with otherSide }

let bump_hole_curr n b =
  { b with currSide = HalfBoard.bump_hole n b.currSide }

let bump_hole_other n b =
  { b with otherSide = HalfBoard.bump_hole n b.otherSide }

let bump_tally b =
  { b with currSide = HalfBoard.bump_tally (Count.of_int 1) b.currSide }

let switch_sides board =
  { currSide = board.otherSide; otherSide = board.currSide }

let remove_pieces n board =
  let count, currSide = HalfBoard.rm_pieces n (curr_side board) in
  if Count.to_int count > 0 then Some (count, set_curr_side currSide board)
  else None

let rec dist n count board =
  let module H = HalfBoard in
  let module I = Index in
  let module C = Count in
  let rec dispense n cnt board =
    if C.to_int cnt = 0 then
      let finalIndx = I.to_int (I.dec n) mod 13 |> I.of_int
      in finalIndx, board
    else
      let i = I.to_int n mod 13 in
      (* Printf.printf "%i\n" i; *)
      let indxMod = I.of_int i in
      let nextN = I.inc n and nextCnt = C.dec cnt in

      if i <= 5 then dispense nextN nextCnt (bump_hole_curr indxMod board)
      else if i = 6 then dispense nextN nextCnt (bump_tally board)
      else dispense nextN nextCnt (bump_hole_other I.(indxMod - (of_int
      7)) board)
  in
  let finalIndx, newBoard = dispense n count board in
  (* Printf.printf "\n%i\n" (Index.to_int finalIndx); *)
  if I.to_int finalIndx <= 5 then
    if C.to_int (H.get_hole finalIndx (curr_side newBoard)) = 1 then
      let captureIndx = I.(of_int 5 - finalIndx) in
      let capturedPieces = H.get_hole captureIndx (other_side newBoard) in
      let otherSide = H.zero_hole captureIndx (other_side newBoard)
      and currSide = H.bump_tally capturedPieces (curr_side newBoard) in
      newBoard |> set_curr_side currSide
               |> set_other_side otherSide
               |> switch_sides
    else switch_sides newBoard
  else if I.to_int finalIndx = 6 then newBoard
  else switch_sides newBoard

let move indx board =
  match remove_pieces indx board with
      None            -> print_endline "Empty pit!"; board
    | Some (count, b) -> dist (Index.inc indx) count b

let is_valid_move indx board =
  match Count.to_int (HalfBoard.nth board.currSide indx) with
    0 -> false
  | _ -> true

let available_moves board =
  let rec filter indx =
    if Index.to_int indx > 5 then []
    else
      match is_valid_move indx board with
        true  -> indx :: (filter (Index.inc indx))
      | false -> filter (Index.inc indx) in
  filter (Index.of_int 0)

let is_finished board =
  HalfBoard.is_empty board.currSide || HalfBoard.is_empty board.otherSide

let final_tally board =
  let currSide = HalfBoard.clear_board board.currSide
  and otherSide = HalfBoard.clear_board board.otherSide in
  { currSide; otherSide }

let winner_is board =
  let module H = HalfBoard in
  let module C = Count in
  let aux a b =
    let (tallyOne, tallyTwo) = H.get_tally a, H.get_tally b in
    if C.to_int tallyOne > C.to_int tallyTwo then Some (H.get_player a)
    else if C.to_int tallyTwo > C.to_int tallyOne then Some (H.get_player b)
    else None
  in
  let talliedBoard = final_tally board in
  aux talliedBoard.currSide talliedBoard.otherSide

let print b =
  let sideOneRepr, sideTwoRepr, sideOneTally, sideTwoTally =
    match curr_player b with
      One ->
        HalfBoard.holes_repr b.currSide, HalfBoard.holes_repr b.otherSide,
        HalfBoard.get_tally b.currSide, HalfBoard.get_tally
    b.otherSide
    | Two ->
        HalfBoard.holes_repr b.otherSide, HalfBoard.holes_repr b.currSide,
        HalfBoard.get_tally b.otherSide, HalfBoard.get_tally b.currSide in
  let boardLen = Int.max (String.length sideOneRepr)
                         (String.length sideTwoRepr) in
  Printf.printf "    ";
  print_endline sideTwoRepr;
  let spaces = (String.repeat " " boardLen) in
  Printf.printf "%i    %s  %i\n"
                (sideTwoTally |> Count.to_int)
                spaces
                (sideOneTally |> Count.to_int);
  Printf.printf "    ";
  print_endline sideOneRepr;
