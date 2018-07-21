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

module HalfBoard : sig
  type t

  val init : player -> t

  val copy : t -> t

  val get_player : t -> player

  val get_tally : t -> Count.t

  val get_hole : Index.t -> t -> Count.t

  val set_hole : Count.t -> Index.t -> t -> t

  val bump_hole : Index.t -> t -> t

  val zero_hole : Index.t -> t -> t

  val bump_tally : Count.t -> t -> t

  val update_tally : Count.t -> t -> t

  val bump_all_holes : t -> t

  val nth : t -> Index.t -> Count.t

  val is_empty : t -> bool

  val clear_board : t -> t

  val rm_pieces : Index.t -> t -> Count.t * t

  val holes_repr : t -> String.t
end
