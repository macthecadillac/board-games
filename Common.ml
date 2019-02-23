open Containers

module type I = sig
  type t = int
  val to_int : t -> int
  val of_int : int -> t
end

module type S = functor (M : I) -> sig
  type t
  val init : unit -> t
  val to_int : t -> int
  val of_int : int -> t
  val inc : t -> t
  val dec : t -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( > ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( = ) : t -> t -> bool
end

module MakeI : S =
  functor (M : I) -> struct
  type t = M.t

  let init () = M.of_int 0

  let of_int = M.of_int

  let to_int = M.to_int

  let inc n = to_int n |> ( + ) 1 |> of_int

  let dec n = to_int n - 1 |> of_int

  let ( + ) a b = to_int a + to_int b |> of_int

  let ( - ) a b = to_int a - to_int b |> of_int

  let ( > ) a b = to_int a > to_int b

  let ( < ) a b = to_int a < to_int b

  let ( = ) a b = to_int a = to_int b
end

module I = struct
  type t = int

  let of_int n = n

  let to_int n = n
end

module Count = MakeI (I)
module Index = MakeI (I)

type player = One | Two

let switch_player = function One -> Two | Two -> One

let print_player = function
  | One -> Printf.printf "%s" "player 1"
  | Two -> Printf.printf "%s" "player 2"
