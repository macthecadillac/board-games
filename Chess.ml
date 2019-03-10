open Containers
open Common

module C = struct

  let rec _pow2 n = Int64.(of_int 2 * _pow2 Int.(n - 1))
  let pow2 = Array.init 64 _pow2

  module Move : sig
    type t
    val of_string : string -> t
  end = struct
    type t
    let aloc = [|'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'|]
    let iloc = [|'1', '2', '3', '4', '5', '6', '7', '8'|]
    let of_string s =
      let pos_ix = Array.find_idx (Char.equal s.(0)) aloc
      and pos_iy = Array.find_idx (Char.equal s.(1)) iloc
      and pos_fx = Array.find_idx (Char.equal s.(2)) aloc
      and pos_fy = Array.find_idx (Char.equal s.(3)) iloc in
      let pos_i = pow2.(pos_ix + 8 * pos_iy)
      and pos_f = pow2.(pos_fx + 8 * pos_fy)
  end

  type pieces = {
    king : Int64.t;
    queens : Int64.t;
    bishops : Int64.t;
    knights : Int64.t;
    rooks : Int64.t;
    pawns : Int64.t;
  }

  type t = {
    currPlayer : player;
    currSide : pieces;
    otherSide : pieces;
  }

  let name = "chess"

  let initWhite = {
    king = Int64.of_int 16;
    queens = Int64.of_int 8;
    bishops = Int64.of_int 36;
    knights = Int64.of_int 66;
    rooks = Int64.of_int 129;
    pawns = Int64.of_int 65280;
  }

  let initBlack = {
    king = Int64.of_int 1152921504606846976;
    queens = Int64.of_int 576460752303423488;
    bishops = Int64.of_int 2594073385365405696;
    knights = Int64.(pow2.(57) + pow2.(62));
    rooks = Int64.(pow2.(56) + pow2.(63));
    pawns = Int64.of_int 71776119061217280;
  }

  let init () = {
    currPlayer = One;
    currSide = initWhite;
    otherSide = initBlack;
  }

  let curr_player b = b.currPlayer

  let is_finished b =
    Int64.(b.currSide.king = zero) || Int64.(b.otherSide.king = zero)
end
