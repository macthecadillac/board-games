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
  | All of Index.t
  | Random

val available_moves :
  Board.t -> (Index.t * (Count.t * Board.t)) list

val random_move :
  (Index.t * (Count.t * Board.t)) list ->
    (Index.t * (Count.t * Board.t))

val random_playout :
  sim_mode -> player -> Board.t -> Favorability.outcome

val compute_favorability : int -> player -> Board.t -> Favorability.t list

val most_favored_move : int -> player -> Board.t -> Index.t
