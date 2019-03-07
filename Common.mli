module CustomInt : sig
  module type I = sig
    type t = int
    val to_int : t -> int
    val of_int : int -> t
  end

  module type S = sig
    type t
    val init : unit -> t
    val to_int : t -> int
    val of_int : int -> t
    val inc : t -> t
    val dec : t -> t
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( > ) : t -> t -> bool
    val ( < ) : t -> t -> bool
    val ( = ) : t -> t -> bool
  end

  module Make (M : I) : S
end

type player = One | Two
val switch_player : player -> player
val print_player : player -> unit

type debug = Debug | Release

module Index : CustomInt.S

module Count : CustomInt.S

module type GAME = sig
  type t
  val name : string
  val init : unit -> t
  val available_moves : t -> Index.t list
  val curr_player : t -> player
  val is_finished : t -> bool
  val is_valid_move : Index.t -> t -> bool
  val move : Index.t -> t -> t
  val winner_is : t -> player option
  val print : t -> unit
  val game_end_screen : t -> unit
end
