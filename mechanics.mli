val switch_sides : Common.mancala_board -> Common.mancala_board

val last_piece_indx : Common.Index.t -> Common.Count.t ->
  Common.side * Common.Index.t

val remove_pieces : Common.Index.t -> Common.mancala_board ->
  (Common.Count.t * Common.mancala_board) option

val play : Common.Index.t -> Common.Count.t -> Common.mancala_board ->
  Common.mancala_board

val is_finished : Common.mancala_board -> bool

val final_tally : Common.mancala_board -> Common.mancala_board

val winner_is : Common.mancala_board -> Common.player option