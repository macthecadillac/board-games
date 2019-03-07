open Common

module type S = sig
  val run : unit -> unit
end

module Make (G : GAME) : S
