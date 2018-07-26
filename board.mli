type t

val init : unit -> t

val copy : t -> t

val curr_player : t -> Common.player

val move : Common.Index.t -> t -> t

val is_valid_move : Common.Index.t -> t -> bool

val available_moves : t -> Common.Index.t list

val is_finished : t -> bool

val get_curr_player_tally : t -> Common.Count.t

val get_other_player_tally : t -> Common.Count.t

val final_tally : t -> t

val winner_is : t -> Common.player option

val print : t -> unit

val print_tally : t -> unit
