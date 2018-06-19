(* TODO: Make the simulations more efficient by narrowing/extending game trees
 * from previous moves instead of building a new one from the ground up for
 * every move *)
(* TODO: Make the decision process learn from past mistakes *)
open Containers
open Common

module T = struct
  type 'a t =
      Leaf of Index.t * player * 'a
    | Node of Index.t * player * 'a t list

  (* TODO: parallelize *)
  let rec map f tree =
    let rec aux = function
        []                -> []
      | Leaf (j, p, b) :: tl -> Leaf (j, p, f b) :: aux tl
      | Node (j, p, m) :: tl -> Node (j, p, aux m) :: aux tl in
    match tree with
        Leaf (i, p, a) -> Leaf (i, p, f a)
      | Node (i, p, l) -> Node (i, p, aux l)

  let rec collapse f acc tree =
    let rec aux = function
        [] -> acc
      | Leaf (j, p, b) :: tl -> f b (aux tl)
      | Node (j, p, m) :: tl -> f (aux m) (aux tl) in
    match tree with
        Leaf _ as l -> l
      | Node (i, p, l) -> Leaf (i, p, aux l)
end

(* TODO: make this a functor that takes module F as an argument *)
module Tree = struct
  include T
  let expand_to_leaves levels player board initFav =
    let rec aux depth player indx board =
      if depth = 0 then T.Leaf (indx, (Board.curr_player board),
                                (initFav, board))
      else
        let currPlayer = Board.curr_player board in
        match Board.available_moves board with
          []             -> T.Leaf (indx, currPlayer, (initFav, board))
        | availableMoves ->
            let rec aux2 = function
                []      -> []
              | n :: tl ->
                  let newNode =
                    let newBoard = Board.move n board in
                    let nextPlayer = Board.curr_player newBoard in
                    match currPlayer, nextPlayer with
                        One, One | Two, Two ->
                          T.Node (n, currPlayer,
                                  [aux depth player indx newBoard])
                      | _                   ->
                          T.Node (n, currPlayer,
                                  [aux (depth - 1) player indx newBoard])
                  in
                  newNode :: (aux2 tl)
            in
            T.Node (indx, currPlayer, aux2 availableMoves)
    in aux levels player (Index.of_int 0) board
end

module MCSearch = struct
  module Favorability = struct
    type t = {
      numerator   : int;
      denominator : int;
    }
    type outcome =
        Positive
      | Negative
      | Indecisive
    let init () = { numerator = 0; denominator = 0 }
    let as_float s = match s.denominator with
        0 -> 0.
      | _ -> Float.of_int s.numerator /. (Float.of_int s.denominator)
    let promote s =
      { numerator = s.numerator + 2; denominator = s.denominator + 2 }
    let demote s = { s with denominator = s.denominator + 2 }
    let mote s = { numerator = s.numerator + 1; denominator = s.denominator + 2 }
    let (+) a b = { numerator = a.numerator + b.numerator;
                    denominator = a.denominator + b.denominator }
    let (>) a b = as_float a >. (as_float b)
    let (=) a b = Float.abs (as_float a -. (as_float b)) <. 1e-8
  end

  type sim_mode =
      Manual of Index.t
    | Random

  let random_move availableMoves =
    let state = Random.pick_list availableMoves in
    Random.run state

  let rec random_playout mode player board =
    match Board.is_finished board with
      true  -> (match Board.winner_is board with
          None   -> Favorability.Indecisive
        | Some p -> (match p, player with
              One, One | Two, Two -> Favorability.Positive
            | _                   -> Favorability.Negative))
    | false ->
        let availableMoves = Board.available_moves board in
        match mode with
          Random    ->
            let n = random_move availableMoves in
            let newBoard = Board.move n board in
            random_playout Random player newBoard
        | Manual n  -> match Board.is_valid_move n board with
              false -> Favorability.Indecisive
            | true  ->
                let newBoard = Board.move n board in
                random_playout Random player newBoard

  let rec compute_favorability searchLimit player tree =
    let rec random_play sl (f0, b) =
      (* Printf.printf "%i " sl; *)
      if sl = 0 then f0, b
      else
        let f =
          match random_playout Random player b with
            Favorability.Positive   -> Favorability.promote f0
          | Favorability.Negative   -> Favorability.demote f0
          | Favorability.Indecisive -> Favorability.mote f0 in
        random_play (sl - 1) (f, b)
    in
    Tree.map (random_play searchLimit) tree

  let most_favored_move searchLimit player board =
    let module F = Favorability in
    let pick_max a b =
      let _, fA = a and _, fB = b in
      F.(if fA > fB then a
         (* pick one at random if both have the same favorability *)
         else if fA = fB then let r = Random.pick_list [a; b] in Random.run r
         else b) in
    (* extract contents of a leaf with the assumption that it only receives
     * a leaf as its argument *)
    let unpack_leaves default = function
        Tree.Leaf (i, _, a) -> i, a
      | Tree.Node (i, _, l) -> i, default (* Inaccessible branch in this context *)
    in

    (* expand 3 levels down the game tree *)
    let tree = Tree.expand_to_leaves 3 player board (Favorability.init ())
      |> compute_favorability searchLimit player
      |> Tree.map (fun (f, _) -> f) in (* filters out only the favorabilities *)
    let moves = match tree with
        Tree.Leaf _ as l    -> [l] (* Inaccessible branch considering the code *)
      | Tree.Node (_, _, l) -> List.map (Tree.collapse F.(+) (F.init ())) l in
    let unpacked_branches = List.map (unpack_leaves (F.init ())) moves in
    let indx, _ = List.fold_left pick_max (List.hd unpacked_branches)
                                          unpacked_branches
    in indx
end

(* FIXME: Doesn't work all too well yet. Take into account of which player made
 * which move when collapsing the game tree to make it truly minimax *)
module MiniMax = struct
  module Favorability = Abstype.MakeMInt (Abstype.I)

  let scorer aiPlayer board = match Board.is_finished board with
      true  -> (match Board.winner_is board with
          None   -> Favorability.of_int 0
        | Some p -> (match p, aiPlayer with
              One, One | Two, Two -> Favorability.of_int 1000
            | _                   -> Favorability.of_int (-1000)))
    | false ->
        let get_score hb = HalfBoard.get_tally hb |> Count.to_int in
        let relScore =
          (Board.curr_side board |> get_score)
        - (Board.other_side board |> get_score) in
        match aiPlayer, Board.curr_player board with
          One, One | Two, Two -> Favorability.of_int relScore
        | _                   -> (-1) * relScore |> Favorability.of_int

  let max_indx a b =
    let _, fA = a and _, fB = b in
    Favorability.(
      if fA > fB then a
      (* pick one at random if both have the same favorability *)
      else if fA = fB then let r = Random.pick_list [a; b] in Random.run r
      else b
    )

  let pick_max a b =
    Favorability.(
      if a > b then a
      else if a = b then let r = Random.pick_list [a; b] in Random.run r
      else b
    )

  let rec compute_favorability aiPlayer tree =
    let aux (_, b) = scorer aiPlayer b in
    Tree.map aux tree

  let most_favored_move searchDepth aiPlayer board =
    let module F = Favorability in
    let unpack_leaves default = function
        Tree.Leaf (i, _, a) -> i, a
      | Tree.Node (i, _, l) -> i, default (* Inaccessible branch in this context *)
    in

    let tree = Tree.expand_to_leaves searchDepth aiPlayer board (F.init ())
        |> compute_favorability aiPlayer in
    let moves = match tree with
        Tree.Leaf _ as l -> [l]
      | Tree.Node (_, _, l) -> List.map (Tree.collapse pick_max (F.init ())) l in
    let unpacked_branches = List.map (unpack_leaves (F.init ())) moves in
    let indx, _ = List.fold_left max_indx (List.hd unpacked_branches) unpacked_branches
    in indx
end
