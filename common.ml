open Containers

module Count = Abstype.MakeMInt (Abstype.I)
module Index = Abstype.MakeMInt (Abstype.I)

type player =
  | One
  | Two

let switch_player = function
  | One -> Two
  | Two -> One

let print_player = function
  | One -> Printf.printf "%s" "player 1"
  | Two -> Printf.printf "%s" "player 2"

module HalfBoard = struct
  type t = {
    player : player;
    tally  : Count.t;
    holes  : Count.t array;
  }
  let init player = { player = player; tally = Count.of_int 0;
                      holes = Array.init 6 (fun x -> Count.of_int 4); }

  let copy halfBoard = { halfBoard with holes = Array.copy halfBoard.holes }

  let get_player halfBoard = halfBoard.player
  let get_tally halfBoard = halfBoard.tally
  let get_hole indx halfBoard = halfBoard.holes.(Index.to_int indx)
  let set_hole c indx halfBoard =
    let newHalfBoard = copy halfBoard in
    newHalfBoard.holes.(Index.to_int indx) <- c;
    newHalfBoard
  let bump_hole indx halfBoard =
    let i = Index.to_int indx in
    let newHalfBoard = copy halfBoard in
    newHalfBoard.holes.(i) <- Count.inc (newHalfBoard.holes.(i));
    newHalfBoard
  let zero_hole = set_hole (Count.of_int 0)

  let bump_tally n halfBoard = { halfBoard with
                                 tally = Count.(halfBoard.tally + n) }
  let update_tally tally halfBoard = { halfBoard with tally }
  let bump_all_holes halfBoard =
    let newHalfBoard = copy halfBoard in
    let holes = Array.map (fun x -> Count.(x + of_int 1)) newHalfBoard.holes in
    { newHalfBoard with holes }

  let nth halfBoard i = halfBoard.holes.(Index.to_int i)

  let is_empty halfBoard =
    Array.fold (fun a b -> Count.to_int b = 0 && a) true halfBoard.holes

  let clear_board halfBoard =
    let newHalfBoard = copy halfBoard in {
      newHalfBoard with
      tally = Count.(newHalfBoard.tally +
                     Array.fold_right (+) newHalfBoard.holes (of_int 0));
      holes = Array.map (fun x -> Count.of_int 0) newHalfBoard.holes;
  }

  let rm_pieces n halfBoard =
    let n = Index.to_int n in
    let newHalfBoard = copy halfBoard in
    let count = newHalfBoard.holes.(n) in
    newHalfBoard.holes.(n) <- Count.of_int 0;
    count, { newHalfBoard with holes = newHalfBoard.holes }

  let holes_repr halfBoard =
    let rawRepr = match halfBoard.player with
    | One -> halfBoard.holes
    | Two -> Array.rev halfBoard.holes in
    let pad b a =
      let cnt = Count.to_int b in
      if cnt < 10 then string_of_int cnt ^ "   " ^ a
      else string_of_int cnt ^ "  " ^ a
    in "  " ^ (Array.fold_right pad rawRepr "")
end

type side =
  | This
  | Other
  | Neither
