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
    halfBoard.holes.(Index.to_int indx) <- c;
    halfBoard
  let bump_hole indx halfBoard =
    let i = Index.to_int indx in
    halfBoard.holes.(i) <- Count.inc (halfBoard.holes.(i));
    halfBoard
  let zero_hole = set_hole (Count.of_int 0)

  let bump_tally n halfBoard = { halfBoard with
                                 tally = Count.(halfBoard.tally + n) }
  let update_tally tally halfBoard = { halfBoard with tally }
  let bump_all_holes halfBoard =
    let holes = Array.map (fun x -> Count.(x + of_int 1)) halfBoard.holes in
    { halfBoard with holes }

  let nth halfBoard indx = halfBoard.holes.(Index.to_int indx)

  let is_empty halfBoard =
    Array.fold (fun a b -> Count.to_int b = 0 && a) true halfBoard.holes

  let clear_board halfBoard = {
      halfBoard with
      tally = Count.(halfBoard.tally +
                     Array.fold_right (+) halfBoard.holes (of_int 0));
      holes = Array.map (fun x -> Count.of_int 0) halfBoard.holes;
  }

  let rm_pieces n halfBoard =
    let n = Index.to_int n in
    let count = halfBoard.holes.(n) in
    halfBoard.holes.(n) <- Count.of_int 0;
    count, { halfBoard with holes = halfBoard.holes }

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
