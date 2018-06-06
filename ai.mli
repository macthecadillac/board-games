open Common

module MCSearch : sig
  val most_favored_move : int -> player -> Board.t -> Index.t
end

module MiniMax : sig
  val most_favored_move : int -> player -> Board.t -> Index.t
end
