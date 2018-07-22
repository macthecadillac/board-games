open Containers
module Count = Abstype.MakeMInt (Abstype.I)
module Index = Abstype.MakeMInt (Abstype.I)

type player = One | Two

let switch_player = function One -> Two | Two -> One

let print_player = function
  | One -> Printf.printf "%s" "player 1"
  | Two -> Printf.printf "%s" "player 2"

module HalfBoard = struct
  type t = {player: player; tally: Count.t; pits: Count.t array}

  let init player =
    { player
    ; tally= Count.of_int 0
    ; pits= Array.init 6 (fun x -> Count.of_int 4) }

  let copy halfBoard = {halfBoard with pits= Array.copy halfBoard.pits}

  let get_player halfBoard = halfBoard.player

  let get_tally halfBoard = halfBoard.tally

  let get_pit indx halfBoard = (halfBoard.pits).(Index.to_int indx)

  let set_pit c indx halfBoard =
    (halfBoard.pits).(Index.to_int indx) <- c ;
    halfBoard

  let bump_pit indx halfBoard =
    let i = Index.to_int indx in
    (halfBoard.pits).(i) <- Count.inc (halfBoard.pits).(i) ;
    halfBoard

  let zero_pit = set_pit (Count.of_int 0)

  let bump_tally n halfBoard =
    {halfBoard with tally= Count.(halfBoard.tally + n)}

  let update_tally tally halfBoard = {halfBoard with tally}

  let bump_all_pits halfBoard =
    let pits = Array.map (fun x -> Count.(x + of_int 1)) halfBoard.pits in
    {halfBoard with pits}

  let nth halfBoard indx = (halfBoard.pits).(Index.to_int indx)

  let is_empty halfBoard =
    Array.fold (fun a b -> Count.to_int b = 0 && a) true halfBoard.pits

  let clear_board halfBoard =
    { halfBoard with
      tally=
        (let open Count in
        halfBoard.tally + Array.fold_right ( + ) halfBoard.pits (of_int 0))
    ; pits= Array.map (fun x -> Count.of_int 0) halfBoard.pits }

  let rm_pieces n halfBoard =
    let n = Index.to_int n in
    let count = (halfBoard.pits).(n) in
    (halfBoard.pits).(n) <- Count.of_int 0 ;
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
