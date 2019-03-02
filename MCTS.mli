open Common

module type BOARD = sig
  type t
  val available_moves : t -> Index.t list
  val curr_player : t -> player
  val is_finished : t -> bool
  val move : Index.t -> t -> t
  val winner_is : t -> player option
end

module type S = sig
  type t
  val most_favored_move : int -> t -> Index.t
end

module Make (M : BOARD) : S
  with type t = M.t
