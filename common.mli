type player = One | Two

val switch_player : player -> player

val print_player : player -> unit

module Index : sig
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

module Count : sig
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
