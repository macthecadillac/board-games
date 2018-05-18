open Containers
open Common

let switch_sides board =
  let thisSide = HalfBoard.rev board.otherSide in
  let otherSide = HalfBoard.rev board.thisSide in
  { thisSide; otherSide }

(* This function assumes we don't need to completely wrap around when
 * distributing the pieces *)
let last_piece_indx n count =
  let n, count = Index.to_int n, Count.to_int count in
  let s =
    if count + n = 6 then Neither
    else if count + n < 6 || count + n > 12 then This
    else Other in
  let indx =
    if count + n = 6 then 0
    else if count + n < 6 then count + n
    else if count + n > 12 then count + n - 13
    else 12 - (count + n) in
  s, Index.of_int indx

let remove_pieces n board =
  let count, thisSide = HalfBoard.rm_pieces n board.thisSide in
  if Count.to_int count > 0 then Some (count, { board with thisSide })
  else None

(* Play a piece *)
let rec play n count board =
  let module H = HalfBoard in
  let module I = Index in
  let module C = Count in
  (* Add 1 to every hole on the board *)
  let bump_every_hole board =
    let thisSide =
      let newHoles = List.map C.inc (H.get_holes board.thisSide) in
      H.bump_tally (C.of_int 1) board.thisSide
        |> H.update_holes newHoles in
    let otherSide = 
      let newHoles = List.map C.inc (H.get_holes board.otherSide) in
      H.update_holes newHoles board.thisSide in
    { thisSide; otherSide } in
  let distribute_pieces n count board =
    let b = board in
    (* Break current-side board at n *)
    let left, right = List.take_drop (I.to_int n) (H.get_holes b.thisSide) in
    (* Straighten board (plus current-side tally bowl) into single list *)
    let unfurledFromN =
      List.hd right :: []  (* move the current bowl to the end of the queue *)
        |> List.append left
        |> List.append (H.rev b.otherSide |> H.get_holes)
        |> List.append [H.get_tally b.thisSide]
        |> List.append (List.tl right) in
    (* Distribute the pieces into the first "count" holes *)
    let newBoardState =
      let changed, unchanged = List.take_drop (C.to_int count) unfurledFromN in
      List.append (List.map C.inc changed) unchanged in
    (* Break apart the list into the two half-boards *)
    let nright, rest = List.take_drop (5 - I.to_int n) newBoardState in
    let tally, rest = List.hd_tl rest in
    let otherHolesRev, nleft = List.take_drop 6 rest in
    let otherHolesNC = List.rev otherHolesRev
    in
    let ownHoles = nleft @ nright in
    let thisSide = H.update_tally tally b.thisSide |> H.update_holes ownHoles in
    let otherSide = H.update_holes otherHolesNC b.otherSide in
    { thisSide; otherSide } in
  (* Decide whether to switch sides and/or capture pieces and execute *)
  let switch_n_capture n count board =
    match last_piece_indx n count with
    | (Other, _)       -> switch_sides board
    | (Neither, _)     -> board
    | (This, indx) ->
        (* match with 1 because this is done after the pieces have been
         * distributed *)
        match C.to_int (H.nth board.thisSide indx) = 1 with
        | false -> switch_sides board
        | true  ->
            let b = board in
            let tally = C.add (H.get_tally b.thisSide)
                              (H.nth b.otherSide indx) in
            let otherSide = H.zero_hole indx b.otherSide in
            let thisSide = H.update_tally tally b.thisSide in
            switch_sides { thisSide; otherSide } in
  (* if number of pieces larger than the total number of holes *)
  if C.to_int count >= 13 then
    bump_every_hole board
      |> play n (C.to_int count - 13 |> C.of_int)
  else
    distribute_pieces n count board
      |> switch_n_capture n count
