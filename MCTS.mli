open Common

module type BOARD = sig
  type t
  val curr_player : t -> player
  val move : Index.t -> t -> t
  val available_moves : t -> Index.t list
  val is_finished : t -> bool
  val winner_is : t -> player option
end

module type S = sig
  type t
  val most_favored_move : int -> player -> t -> Index.t
end

module Make : S
