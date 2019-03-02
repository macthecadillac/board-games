open Common

module type BOARD = sig
  type t
  val init : unit -> t
  val available_moves : t -> Index.t list
  val curr_player : t -> player
  val is_finished : t -> bool
  val is_valid_move : Index.t -> t -> bool
  val move : Index.t -> t -> t
  val winner_is : t -> player option
  val print : t -> unit
end

module type S = functor (M : BOARD) -> sig
  type t
  val init : unit -> t
  val curr_player : t -> player
  val is_finished : t -> bool
  val is_valid_move : Index.t -> t -> bool
  val most_favored_move : int -> t -> Index.t
  val move : Index.t -> t -> t
  val winner_is : t -> player option
  val print : t -> unit
end

module Make : S
