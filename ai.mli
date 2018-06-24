open Common

module MCSearch : sig

  module FavHist : sig
    type t
  end

  module FavHistTree : sig
    type t
    val init : Index.t -> t
    val depth : t -> int
    val choose_branch : Index.t -> t -> FavHist.t * t
    val update_from_last_playout : player option -> t -> Index.t list -> t
    val load : string -> t
    val save : t -> string -> unit
  end

  val most_favored_move : int -> player -> Board.t -> FavHistTree.t -> Index.t
end

module MiniMax : sig
  val most_favored_move : int -> player -> Board.t -> Index.t
end
