open Containers

type player = 
  | One
  | Two

val print_player : player -> unit

module Index : sig
  type t
  val to_int : t -> int
  val of_int : int -> t
  val inc : t -> t
end

module Count : sig
  type t
  val to_int : t -> int
  val of_int : int -> t
  val inc : t -> t
  val add : t -> t -> t
end

module HalfBoard : sig
  type t
  val init : player -> t

  val get_tally : t -> Count.t
  val get_player : t -> player
  val get_holes : t -> Count.t list
  val set_hole : Count.t -> Index.t -> t -> t
  val zero_hole : Index.t -> t -> t

  val bump_tally : Count.t -> t -> t
  val update_tally : Count.t -> t -> t
  val update_holes : Count.t list -> t -> t

  val nth : t -> Index.t -> Count.t
  val rev : t -> t

  val is_empty : t -> bool
  val clear_board : t -> t
  val rm_pieces : Index.t -> t -> Count.t * t
end

type mancala_board = {
  thisSide  : HalfBoard.t;
  otherSide : HalfBoard.t;
}

type side =
  | This
  | Other
  | Neither

val print_board : mancala_board -> unit
