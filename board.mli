type t

val build : Common.HalfBoard.t -> Common.HalfBoard.t -> t

val curr_player : t -> Common.player

val curr_side : t -> Common.HalfBoard.t

val other_side : t -> Common.HalfBoard.t

val set_curr_side : Common.HalfBoard.t -> t -> t

val set_other_side : Common.HalfBoard.t -> t -> t

val bump_hole_other : Common.Index.t -> t -> t

val bump_hole_curr : Common.Index.t -> t -> t

val bump_tally : t -> t

val switch_sides : t -> t

val remove_pieces : Common.Index.t -> t ->
  (Common.Count.t * t) option

val dist : Common.Index.t -> Common.Count.t -> t -> t

val move : Common.Index.t -> t -> t

val available_moves : t -> Common.Index.t list

val is_finished : t -> bool

val final_tally : t -> t

val winner_is : t -> Common.player option

val print : t -> unit
