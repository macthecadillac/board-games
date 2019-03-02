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
  (* val print : t -> unit *)
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

  (* let print a = Printf.printf "{ win = %i; total = %i }" a.win a.total *)
end

module Score : sig
  type t
  val from_fav : Favorability.t -> t
  (* val as_float : t -> float *)
  (* val accum : t -> t -> t *)
  val ( > ) : t -> t -> bool
end = struct
  type t = { q : float; total : int }

  let from_fav f = { q = Favorability.as_float f; total = f.total }

  let as_float s = s.q +. (1. /. (float_of_int s.total))

  (* let accum s1 s2 = *)
  (*   let t1 = float_of_int s1.total *)
  (*   and t2 = float_of_int s2.total in *)
  (*   let q = (s1.q /. t1 +. s2.q /. t2) /. (1. /. t1 +. 1. /. t2) *)
  (*   and total = s1.total + s2.total in *)
  (*   { q; total } *)

  let ( > ) a b = as_float a >. as_float b
end

module type BOARD = sig
  type t
  val available_moves : t -> Index.t list
  val curr_player : t -> player
  val is_finished : t -> bool
  val move : Index.t -> t -> t
  val winner_is : t -> player option
end

module type S = sig
  type t
  val most_favored_move : int -> t -> Index.t
end

module Make (M : BOARD) : S
  with type t = M.t = struct
  include M

  module F = Favorability

  let next_level board favi =
    let rec f i b = function
      | [] -> []
      | n :: tl ->
          let node' =
            let p = M.curr_player b in
            let b' = M.move n b in
            let p' = M.curr_player b' in
            match (p, p') with
            | One, One | Two, Two -> Tree.Node ((n, p, favi, b), [aux i b'])
            | _ -> Tree.Leaf (n, p, favi, b)
          in node' :: f i b tl
    and aux i b =
      let p = M.curr_player board in
      match M.available_moves b with
      | [] -> Tree.Leaf (i, p, favi, b)
      | moves -> Tree.Node ((i, p, favi, b), f i b moves) in
    aux (Index.init ()) board

  let pick_max f l =
    let rec aux acc = function
      | [] -> []
      | hd :: tl ->
          match acc with
          | [] -> aux [hd] tl
          | fst :: _ ->
              if f hd fst then aux [hd] tl
              else aux acc tl in
    let r = Random.pick_list (aux [] l) in
    Random.run r

  let rec replace_branch f b b' = function
    | [] -> []
    | hd :: tl ->
        if f hd b then b' :: tl
        else hd :: (replace_branch f b b' tl)

  let _branch_ord a b =
    let _, _, x, _ = Tree.node_elt a
    and _, _, y, _ = Tree.node_elt b in
    let x' = Score.from_fav x
    and y' = Score.from_fav y in
    Score.(x' > y')

  let _branch_eq a b =
    let i, _, _, _ = Tree.node_elt a
    and j, _, _, _ = Tree.node_elt b in
    Index.(i = j)

  let rec playout player l = match l with
    | Tree.Node ((i, p, f, b), branches) ->
        let branch = pick_max _branch_ord branches in
        let fav, branch' = playout player branch in
        let f' = F.(fav + f) in
        let branches' = replace_branch _branch_eq branch branch' branches in
        fav, Tree.Node ((i, p, f', b), branches')
    | Tree.Leaf (i, p, _, board) ->
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
        else
          next_level board (F.init ())
            |> playout player

  let most_favored_move nPlayouts board =
    let rec aux n acc player (fav, tree) =
      if n = 0 then acc, tree
      else
        let acc' = F.(acc + fav) in
        aux (n - 1) acc' player (playout player tree) in
    let player = M.curr_player board in
    let fav = F.init () in
    let root = Tree.Leaf (Index.init (), player, fav, board) in
    let _, root' = aux nPlayouts (F.init ()) player (F.init (), root) in
    match root' with
    | Leaf _ -> Index.init () (* inaccessible branch *)
    | Node (_, branches) ->
        let i, _, _, _ = pick_max _branch_ord branches in
        i
end
