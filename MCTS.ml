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

type outcome = Win | Loss | Draw

module Score : sig
  type t
  val init : unit -> t
  val of_outcome : outcome -> t
  val ( + ) : t -> t -> t
  val ( > ) : t -> t -> bool
  val ( $> ) : t -> t -> bool
  val ( $< ) : t -> t -> bool
  val ( = ) : t -> t -> bool
  val ( $= ) : t -> t -> bool
  val print : t -> unit
end = struct
  type t = { q : float;
             u : float;
             win : int;
             loss : int;
             draw : int;
             total : int; }

  let init () = { q = 1.; u = 1.; win = 0; loss = 0; draw = 0; total = 0 }

  let of_outcome = function
    | Win -> { q = 1.0; u = 0.; win = 1; loss = 0; draw = 0; total = 1 }
    | Loss -> { q = 0.; u = 0.; win = 0; loss = 1; draw = 0; total = 1 }
    | Draw -> { q = 0.5; u = 0.; win = 0; loss = 0; draw = 1; total = 1 }

  let ( + ) a b =
    let u = 1. /. float_of_int (a.total + b.total)
    and win = a.win + b.win
    and total = a.total + b.total
    and loss = a.loss + b.loss
    and draw = a.draw + b.draw in
    let q = 0.5 *. float_of_int (2 * win + draw) /. float_of_int total in
    { q; u; win; loss; draw; total }

  let ( = ) a b = a.total = b.total

  let ( $= ) a b = abs_float (a.q +. a.u -. (b.q +. b.u)) <. 1e-5

  let ( > ) a b = a.total > b.total

  let score_own a = a.q *. sqrt a.u +. a.u ** 4.
  let score_opponent a = (1. -. a.q) *. sqrt a.u +. a.u ** 4.

  let ( $< ) a b =
    if a $= b then false
    else score_opponent a >. score_opponent b

  let ( $> ) a b =
    if a $= b then false
    else score_own a >. score_own b

  let print a = Printf.printf
                "(q: %f, u: %f, win: %i, draw: %i, loss: %i, total: %i) score = %f"
                a.q a.u a.win a.draw a.loss a.total (score_own a)
end

module type S = sig
  type t
  val most_favored_move : int -> t -> debug -> Index.t
end

module Make (M : GAME) : S
  with type t = M.t = struct

  type t = M.t
  type elt = Index.t * player * Score.t * M.t

  exception ExpansionError

  (* Scoring functions for "pick". See below. *)
  let comp f a b =
    let _, _, x, _ = Tree.node_elt a
    and _, _, y, _ = Tree.node_elt b in
    f x y
  
  let _branch_gt = comp Score.( $> )
  let _branch_lt = comp Score.( $< )
  let _branch_eq = comp Score.( $= )
  let _ord = comp Score.( > )
  let _eq = comp Score.( = )

  let _print_node n =
    let a, b, c, d = Tree.node_elt n in
    Score.print c

  (* Pick according to the criterion given (as function f). When undecided,
   * randomly pick one from among the equals *)
  let pick greater eq l =
    let rec aux acc = function
      | [] -> acc
      | hd :: tl ->
          match acc with
          | [] -> aux [hd] tl
          | fst :: _ as eqs ->
              if greater hd fst then aux [hd] tl
              else if eq hd fst then aux (hd :: eqs) tl
              else aux acc tl in
    let r = Random.pick_list (aux [] l) in
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
            let leaf = Tree.Leaf (n, p', Score.init (), brd') in
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
        let branch =
          match (p, player) with
          | One, One | Two, Two -> pick _branch_gt _branch_eq branches
          | _ -> pick _branch_lt _branch_eq branches in
        let fav, branch' = playout player branch in
        let f' = Score.(fav + f) in
        let same_branch = fun a b ->
          let i, _, _, _ = Tree.node_elt a
          and j, _, _, _ = Tree.node_elt b in
          Index.(i = j) in
        let branches' = replace_branch same_branch branch branch' branches in
        fav, Tree.Node ((i, p, f', b), branches')
    | Tree.Leaf (i, p, _, board) as leaf ->
        if M.is_finished board then
          let f = (
            match M.winner_is board with
            | None -> Draw
            | Some p ->
              match (p, player) with
              | One, One | Two, Two -> Win
              | _ -> Loss)
            |> Score.of_outcome in
          f, Tree.Leaf (i, p, f, board)
        else playout player (expand_one_level leaf)

  let most_favored_move nplayouts board dbg =
    let rec aux n acc player (fav, tree) =
      if n = 0 then acc, tree
      else
        let acc' = Score.(acc + fav) in
        aux (n - 1) acc' player (playout player tree) in
    let player = M.curr_player board in
    let fav = Score.init () in
    let root = Tree.Leaf (Index.init (), player, fav, board) in
    let _, root' = aux nplayouts (Score.init ()) player (Score.init (), root) in
    match root' with
    | Leaf _ -> Index.init () (* inaccessible branch *)
    | Node (_, branches) ->
        let _ =
          match dbg with
          | Release -> ();
          | Debug ->
              let l = List.map Tree.node_elt branches in
              List.iter (fun (i, _, f, b) ->
                print_newline ();
                Printf.printf "(%i, " (Index.to_int i);
                Score.print f;
                print_string ") ";
                ) l in
        print_newline ();
        let i, _, _, _ = Tree.node_elt (pick _ord _eq branches) in
        i
end
