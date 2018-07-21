open Common

module MCSearch : sig
  module Hist : sig
    type t
  end

  module Depth : sig
    type t
  end

  module HistTree : sig
    type t

    val init : Index.t -> Depth.t -> t

    val depth : t -> int

    val choose_branch : Index.t -> t -> Depth.t * Hist.t * t

    val update_from_last_playout : player option -> t -> Index.t list -> t

    val load : string -> t

    val save : t -> string -> unit
  end

  val most_favored_move : int -> player -> Board.t -> HistTree.t -> Index.t
end

module MiniMax : sig
  val most_favored_move : int -> player -> Board.t -> Index.t
end
