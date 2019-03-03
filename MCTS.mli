open Common

module type S = sig
  type t
  val most_favored_move : int -> t -> debug -> Index.t
end

module Make (M : GAME) : S
  with type t = M.t
