open Containers
module Count = AbsType.MakeMInt (AbsType.I)
module Index = AbsType.MakeMInt (AbsType.I)

type player = One | Two

let switch_player = function One -> Two | Two -> One

let print_player = function
  | One -> Printf.printf "%s" "player 1"
  | Two -> Printf.printf "%s" "player 2"
