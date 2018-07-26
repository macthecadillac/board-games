open Containers
module Count = Abstype.MakeMInt (Abstype.I)
module Index = Abstype.MakeMInt (Abstype.I)

type player = One | Two

let switch_player = function One -> Two | Two -> One

let print_player = function
  | One -> Printf.printf "%s" "player 1"
  | Two -> Printf.printf "%s" "player 2"
