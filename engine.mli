open Common

module Favorability : sig
  type t
  type outcome =
    | Positive
    | Negative
    | Indecisive
  val init : unit -> t
  val promote : t -> t
  val demote : t -> t
  val (>) : t -> t -> bool
end

type sim_mode =
  | Manual of Index.t
  | Random

val available_moves :
  mancala_board -> (Index.t * (Count.t * mancala_board)) list

val random_move :
  (Index.t * (Count.t * mancala_board)) list ->
    (Index.t * (Count.t * mancala_board))

val random_playout :
  sim_mode -> player -> mancala_board -> Favorability.outcome

val compute_favorability : int -> player -> mancala_board -> Favorability.t list

val most_favored_move : int -> player -> mancala_board -> Index.t
