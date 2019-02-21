(* TODO: Make the simulations more efficient by narrowing/extending game trees
 * from previous moves instead of building a new one from the ground up for
 * every move *)
(* TODO: Change the training mode such that the first AI is forced to evaluate
 * every first move so the the second AI won't be blinsided when I'm playing an
 * opening move that it has never seen *)
open Containers
open Common

module T = struct
  type 'a t =
    | Leaf of Index.t * player * 'a
    | Node of Index.t * player * 'a t list

  (* TODO: parallelize *)
  let rec map f tree =
    let rec aux = function
      | [] -> []
      | Leaf (j, p, b) :: tl -> Leaf (j, p, f b) :: aux tl
      | Node (j, p, m) :: tl -> Node (j, p, aux m) :: aux tl
    in
    match tree with
    | Leaf (i, p, a) -> Leaf (i, p, f a)
    | Node (i, p, l) -> Node (i, p, aux l)

  let rec collapse f acc tree =
    let rec aux = function
      | [] -> acc
      | Leaf (j, p, b) :: tl -> f b (aux tl)
      | Node (j, p, m) :: tl -> f (aux m) (aux tl)
    in
    match tree with Leaf _ as l -> l | Node (i, p, l) -> Leaf (i, p, aux l)
end

(* TODO: make this a functor that takes module F as an argument *)
module Tree = struct
  include T

  let expand_to_leaves levels player board initFav =
    let rec aux depth player indx board =
      if depth = 0 then T.Leaf (indx, Board.curr_player board, (initFav, board))
      else
        let currPlayer = Board.curr_player board in
        match Board.available_moves board with
        | [] -> T.Leaf (indx, currPlayer, (initFav, board))
        | availableMoves ->
            let rec aux2 = function
              | [] -> []
              | n :: tl ->
                  let newNode =
                    let newBoard = Board.move n board in
                    let nextPlayer = Board.curr_player newBoard in
                    match (currPlayer, nextPlayer) with
                    | One, One | Two, Two ->
                        T.Node (n, currPlayer, [aux depth player indx newBoard])
                    | _ ->
                        T.Node
                          ( n
                          , currPlayer
                          , [aux (depth - 1) player indx newBoard] )
                  in
                  newNode :: aux2 tl
            in
            T.Node (indx, currPlayer, aux2 availableMoves)
    in
    aux levels player (Index.init ()) board
end

module Favorability : sig
  type t = { win : int; total : int }
  type outcome = Positive | Negative | Indecisive
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

  type outcome = Positive | Negative | Indecisive

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

let random_move availableMoves =
  let state = Random.pick_list availableMoves in
  Random.run state

let rec random_playout player board =
  match Board.is_finished board with
  | true -> (
    match Board.winner_is board with
    | None -> Favorability.Indecisive
    | Some p ->
      match (p, player) with
      | One, One | Two, Two -> Favorability.Positive
      | _ -> Favorability.Negative )
  | false ->
      let availableMoves = Board.available_moves board in
      let n = random_move availableMoves in
      let newBoard = Board.move n board in
      random_playout player newBoard

(* TODO: Rewrite the following (to the end of file) so we expand from the root
 * for every simulation according to the scores *)
let rec compute_favorability searchLimit player tree =
  let rec random_play sl (f0, b) =
    (* Printf.printf "%i " sl; *)
    if sl = 0 then (f0, b)
    else
      let f =
        match random_playout player b with
        | Favorability.Positive -> Favorability.promote f0
        | Favorability.Negative -> Favorability.demote f0
        | Favorability.Indecisive -> Favorability.mote f0
      in
      random_play (sl - 1) (f, b)
  in
  Tree.map (random_play searchLimit) tree

let compute_score fav = Favorability.as_float fav

let most_favored_move searchLimit player board =
  let module F = Favorability in
  let pick_max a b =
    let _, fA = a and _, fB = b in
    (* if the favorabilities substantially differ *)
    if fA -. fB >=. 0.01 then a
    (* pick one at random if both favorabilities as within 0.01 of one another
     * so the AI is more willing to try out different moves *)
    else if abs_float (fA -. fB) <. 0.01 then
      let r = Random.pick_list [a; b] in
      Random.run r
    else b
  in
  (* extract contents of a leaf with the assumption that it only receives
   * a leaf as its argument *)
  let unpack_leaves = function
    | Tree.Node (i, _, l) -> (i, 0.) (* Inaccessible branch in this context *)
    | Tree.Leaf (i, _, a) -> (i, compute_score a)
  in
  (* expand 2 levels down the game tree *)
  let tree =
    Tree.expand_to_leaves 2 player board (Favorability.init ())
    |> compute_favorability searchLimit player
    |> Tree.map (fun (f, _) -> f)
  in
  (* filters out only the favorabilities *)
  let moves =
    match tree with
    | Tree.Leaf _ as l -> [l]
    | Tree.Node (_, _, l) -> List.map (Tree.collapse F.( + ) (F.init ())) l
  in
  let scores = List.map unpack_leaves moves in
  (* print_newline () ; *)
  (* print_string "Scores: " ; *)
  (* List.iter *)
  (*   (fun (i, x) -> F.(Printf.printf "%i=%f " (Index.to_int i) x)) *)
  (*   scores ; *)
  (* print_newline () ; *)
  let indx, _ = List.fold_left pick_max (List.hd scores) scores in
  (* Index.to_int indx |> Printf.printf "Best move: %i\n\n" ; *)
  indx
