open Containers

type player =
  | One
  | Two

let print_player = function
  | One -> Printf.printf "%s" "player 1"
  | Two -> Printf.printf "%s" "player 2"

module Index = struct
  type t = int
  let to_int n = n
  let of_int n = n
  let inc n = to_int n |> (+) 1 |> of_int
end

module Count = struct
  type t = int
  let to_int n = n
  let of_int n = n
  let inc n = to_int n |> (+) 1 |> of_int
  let (+) a b = to_int a + (to_int b) |> of_int
end

module HalfBoard = struct
  type t = {
    player : player;
    tally  : Count.t;
    holes  : Count.t list;
  }
  let init player = { player; tally = Count.of_int 0;
                      holes = List.init 6 (fun x -> Count.of_int 4); }

  let get_tally halfBoard = halfBoard.tally
  let get_player halfBoard = halfBoard.player
  let get_holes halfBoard = halfBoard.holes
  let set_hole c indx halfBoard =
    let holes = List.set_at_idx (Count.to_int indx) c halfBoard.holes in
    { halfBoard with holes }
  let zero_hole = set_hole (Count.of_int 0)

  let bump_tally n halfBoard = { halfBoard with
                                 tally = Count.(halfBoard.tally + n) }
  let update_tally tally halfBoard = { halfBoard with tally }
  let update_holes holes halfBoard = { halfBoard with holes }

  let nth halfBoard i = List.nth halfBoard.holes (Index.to_int i)
  let rev halfBoard = { halfBoard with holes = List.rev halfBoard.holes }

  let is_empty halfBoard = List.count (fun x -> Count.to_int x > 0)
                                      halfBoard.holes = 0
  let clear_board halfBoard = { halfBoard with
    tally = Count.(halfBoard.tally +
                   List.fold_right (+) halfBoard.holes (of_int 0));
    holes = List.map (fun x -> Count.of_int 0) halfBoard.holes;
  }
  let rm_pieces n halfBoard =
    let n = Index.to_int n in
    let count = List.nth halfBoard.holes n in
    let newHalfBoard = { halfBoard with
                         holes = List.set_at_idx n (Count.of_int 0) halfBoard.holes }
    in
    count, newHalfBoard
end

type mancala_board = {
  thisSide  : HalfBoard.t;
  otherSide : HalfBoard.t;
}

type side =
  | This
  | Other
  | Neither

let print_board board =
  let string_of_int_list s = List.map Int.to_string s |> String.concat "  " in
  let len s = String.length (string_of_int_list s) in
  let boardLen =
    Int.max (len board.thisSide.holes) (len board.otherSide.holes) in
  Printf.printf "    ";
  List.iter (Printf.printf " %i ") board.otherSide.holes;
  let spaces = (String.repeat " " boardLen) in
  Printf.printf "\n%i    %s    %i\n" board.otherSide.tally spaces board.thisSide.tally;
  Printf.printf "    ";
  List.iter (Printf.printf " %i ") board.thisSide.holes;
  print_newline ()
