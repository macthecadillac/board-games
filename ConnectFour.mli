open Common
open Containers

type t

val init : unit -> t

val available_moves : t -> Index.t list

val curr_player : t -> player

val is_finished : t -> bool

val is_valid_move : Index.t -> t -> bool

val move : Index.t -> t -> t

val winner_is : t -> player option

val print : t -> unit
