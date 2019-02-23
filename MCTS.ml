open Containers
open Common

module Tree : sig
  type 'a t =
    | Leaf of 'a
    | Node of 'a * 'a t list
  val map : ('a -> 'b) -> 'a t -> 'b t
  val collapse : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
end = struct
  type 'a t =
    | Leaf of 'a
    | Node of 'a * 'a t list

  let rec map f = function
    | Leaf a -> Leaf (f a)
    | Node (a, l) -> Node (f a, List.map (map f) l)

  let rec collapse f acc = function
    | Leaf a -> f acc a
    | Node (a, l) -> List.fold_left (collapse f) (f acc a) l
end

module Favorability : sig
  type t = { win : int; total : int }
  type outcome = Positive | Negative | Draw
  val init : unit -> t
  val build : int -> int -> t
  val is_unity : t -> bool
  val as_float : t -> float
  val promote : t -> t
  val demote : t -> t
  val mote : t -> t
  val ( + ) : t -> t -> t
  val ( > ) : t -> t -> bool
  val ( = ) : t -> t -> bool
  val print : t -> unit
end = struct
  type t = { win : int; total : int }
  type outcome = Positive | Negative | Draw

  let init () = { win = 0; total = 0 }

  let build a b = { win = a; total = b }

  let is_unity a = a.win = a.total

  let as_float s =
    if s.total = 0 then 0.
    else Float.of_int s.win /. Float.of_int s.total

  let promote s = { win = s.win + 2; total = s.total + 2 }

  let demote s = { s with total = s.total + 2 }

  let mote s = { win = s.win + 1; total = s.total + 2 }

  let ( + ) a b = { win = a.win + b.win; total = a.total + b.total }

  let ( > ) a b = as_float a -. as_float b >. 1e-10

  let ( = ) a b = Float.abs (as_float a -. as_float b) <. 1e-10

  let print a = Printf.printf "{ win = %i; total = %i }" a.win a.total
end

module Score : sig
  type t
  val from_fav : Favorability.t -> t
  val as_float : t -> float
  val accum : t -> t -> t
end = struct
  type t = { q : float; total : int }

  let from_fav f = { q = Favorability.as_float f; total = f.total }

  let as_float s = s.q +. (1. /. (float_of_int s.total))

  let accum s1 s2 =
    let t1 = float_of_int s1.total
    and t2 = float_of_int s2.total in
    let q = (s1.q /. t1 +. s2.q /. t2) /. (1. /. t1 +. 1. /. t2)
    and total = s1.total + s2.total in
    { q; total }
end

let expand_leaf levels board favi =
  let rec f depth i b = function
    | [] -> []
    | n :: tl ->
        let node' =
          let p = Board.curr_player b in
          let b' = Board.move n b in
          let p' = Board.curr_player b' in
          let depth' =
            match (p, p') with
            | One, One | Two, Two -> depth
            | _ -> depth - 1 in
          Tree.Node ((n, p, favi, b), [aux depth' i b'])
        in node' :: f depth i b tl
  and aux depth i b =
    if depth = 0 then Tree.Leaf (i, Board.curr_player b, favi, b)
    else
      let p = Board.curr_player board in
      match Board.available_moves b with
      | [] -> Tree.Leaf (i, p, favi, b)
      | moves -> Tree.Node ((i, p, favi, b), f depth i b moves) in
  aux levels (Index.init ()) board

let random_move availableMoves =
  let state = Random.pick_list availableMoves in
  Random.run state

let rec random_playout player board =
  if Board.is_finished board then
    match Board.winner_is board with
    | None -> Favorability.Draw
    | Some p ->
      match (p, player) with
      | One, One | Two, Two -> Favorability.Positive
      | _ -> Favorability.Negative
  else
    let availableMoves = Board.available_moves board in
    let n = random_move availableMoves in
    let newBoard = Board.move n board in
    random_playout player newBoard

(* TODO: Rewrite the following (to the end of file) so we expand from the root
 * for every simulation according to the scores *)
let rec compute_favorability n player tree =
  let rec random_play sl (f0, b) =
    if sl = 0 then (f0, b)
    else
      let f =
        match random_playout player b with
        | Favorability.Positive -> Favorability.promote f0
        | Favorability.Negative -> Favorability.demote f0
        | Favorability.Draw -> Favorability.mote f0 in
      random_play (sl - 1) (f, b) in
  Tree.map (random_play n) tree

let compute_score fav = Favorability.as_float fav

let most_favored_move searchLimit player board =
  let module F = Favorability in
  let module T = Tree in
  let pick_max a b =
    let _, fA = a and _, fB = b in
    (* if the favorabilities substantially differ *)
    if fA -. fB >=. 0.01 then a
    (* pick one at random if both favorabilities as within 0.01 of one another
     * so the AI is more willing to try out different moves *)
    else if abs_float (fA -. fB) <. 0.01 then
      let r = Random.pick_list [a; b] in
      Random.run r
    else b in
  (* extract contents of a leaf with the assumption that it only receives
   * a leaf as its argument *)
  let unpack_leaves = function
    | T.Node (i, _, l) -> (i, 0.) (* Inaccessible branch in this context *)
    | T.Leaf (i, _, a) -> (i, compute_score a) in
  (* expand 2 levels down the game tree *)
  let tree =
    expand_leaf 2 player board (Favorability.init ())
    |> compute_favorability searchLimit player
    |> T.map (fun (f, _) -> f) in
  (* filters out only the favorabilities *)
  let moves =
    match tree with
    | T.Leaf _ as l -> [l]
    | T.Node (_, _, l) -> List.map (T.collapse F.( + ) (F.init ())) l in
  let scores = List.map unpack_leaves moves in
  let indx, _ = List.fold_left pick_max (List.hd scores) scores in
  indx
