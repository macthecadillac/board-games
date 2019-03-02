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

module Index : CustomInt.S

module Count : CustomInt.S
