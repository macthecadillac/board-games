open Containers
open Common

module Tree : sig
  type 'a t =
    | Leaf of 'a
    | Node of 'a * 'a t list
  val node_elt : 'a t -> 'a
end = struct
  type 'a t =
    | Leaf of 'a
    | Node of 'a * 'a t list

  let node_elt = function
    | Leaf a -> a
    | Node (a, _) -> a
end

module Favorability : sig
  type t = { win : int; total : int }
  type outcome = Win | Loss | Draw
  val init : unit -> t
  val t_of_outcome : outcome -> t
  val as_float : t -> float
  val ( + ) : t -> t -> t
end = struct
  type t = { win : int; total : int }
  type outcome = Win | Loss | Draw

  let init () = { win = 0; total = 0 }

  let t_of_outcome = function
    | Win -> { win = 2; total = 2}
    | Draw -> { win = 1; total = 2}
    | Loss -> { win = 0; total = 2}

  let as_float s =
    if s.total = 0 then 0.
    else Float.of_int s.win /. Float.of_int s.total

  let ( + ) a b = { win = a.win + b.win; total = a.total + b.total }
end

module Score : sig
  type t
  val from_fav : Favorability.t -> t
  val ( > ) : t -> t -> bool
  val ( $> ) : t -> t -> bool
end = struct
  type t = { q : float; total : int }

  let from_fav f = { q = Favorability.as_float f; total = f.total }

  let as_float s = s.q +. (1. /. (float_of_int s.total))

  let ( > ) a b = a.total > b.total

  let ( $> ) a b = as_float a >. as_float b
end

module type S = sig
  type t
  val most_favored_move : int -> t -> Index.t
end

module Make (M : GAME) : S
  with type t = M.t = struct

  module F = Favorability

  type t = M.t
  type elt = Index.t * player * F.t * M.t

  exception ExpansionError

  (* Scoring functions for "pick". See below. *)
  let _ord f a b =
    let _, _, x, _ = Tree.node_elt a
    and _, _, y, _ = Tree.node_elt b in
    let x' = Score.from_fav x
    and y' = Score.from_fav y in
    f x' y'
  
  let _branch_ord = _ord Score.( $> )
  let _comp = _ord Score.( > )

  let _branch_eq a b =
    let i, _, _, _ = Tree.node_elt a
    and j, _, _, _ = Tree.node_elt b in
    Index.(i = j)

  (* Pick the branch that shows the most promise. If multiple branches have the
   * highest score then one will be picked from among them at random *)
  let pick f l =
    let rec aux acc = function
      | [] -> acc
      | hd :: tl ->
          match acc with
          | [] -> aux [hd] tl
          | fst :: _ ->
              if f hd fst then aux [hd] tl
              else aux acc tl in
    let l' = aux [] l in
    let r = Random.pick_list l' in
    Random.run r

  let rec replace_branch f b b' = function
    | [] -> []
    | hd :: tl ->
        if f hd b then b' :: tl
        else hd :: (replace_branch f b b' tl)

  let rec expand_one_level tree =
    let rec list_branches brd = function
      | [] -> []
      | n :: tl ->
          let node' =
            let p = M.curr_player brd in
            let brd' = M.move n brd in
            let p' = M.curr_player brd' in
            let leaf = Tree.Leaf (n, p', F.init (), brd') in
            match (p, p') with
            | One, Two | Two, One -> leaf
            | One, One | Two, Two ->
                if M.is_finished brd' then leaf
                else
                  match expand_one_level leaf with
                  | Tree.Leaf _ -> raise ExpansionError
                  | Tree.Node ((_, p', favi, brd'), l) ->
                      Tree.Node ((n, p', favi, brd'), l)
          in node' :: list_branches brd tl in
    let i, p, f, b =
      match tree with
      | Tree.Leaf (indx, plyr, fav, brd) -> indx, plyr, fav, brd
      | Tree.Node _ -> raise ExpansionError in
    match M.available_moves b with
    | [] -> raise ExpansionError
    | moves -> Tree.Node ((i, p, f, b), list_branches b moves)

  let rec playout player = function
    | Tree.Node ((i, p, f, b), branches) ->
        let branch = pick _branch_ord branches in
        let fav, branch' = playout player branch in
        let f' = F.(fav + f) in
        let branches' = replace_branch _branch_eq branch branch' branches in
        fav, Tree.Node ((i, p, f', b), branches')
    | Tree.Leaf (i, p, _, board) as leaf ->
        if M.is_finished board then
          let f = (
            match M.winner_is board with
            | None -> F.Draw
            | Some p ->
              match (p, player) with
              | One, One | Two, Two -> F.Win
              | _ -> F.Loss)
            |> F.t_of_outcome in
          f, Tree.Leaf (i, p, f, board)
        else playout player (expand_one_level leaf)

  let most_favored_move nplayouts board =
    let rec aux n acc player (fav, tree) =
      if n = 0 then acc, tree
      else
        let acc' = F.(acc + fav) in
        aux (n - 1) acc' player (playout player tree) in
    let player = M.curr_player board in
    let fav = F.init () in
    let root = Tree.Leaf (Index.init (), player, fav, board) in
    let _, root' = aux nplayouts (F.init ()) player (F.init (), root) in
    match root' with
    | Leaf _ -> Index.init () (* inaccessible branch *)
    | Node (_, branches) ->
        let i, _, _, _ = Tree.node_elt (pick _comp branches) in
        i
end
