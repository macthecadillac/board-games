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
  val init : int -> t
  val update : t -> outcome -> t
  val parent_total : t -> int
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
             total : int;
             ptot : int }

  let init ptot = { q = 1.; u = 1.; win = 0; loss = 0; draw = 0;
                  total = 0; ptot }

  let parent_total a = a.ptot

  let ( + ) a b =
    let u = 1. /. float_of_int (a.total + b.total)
    and win = a.win + b.win
    and total = a.total + b.total
    and ptot = a.ptot + 1
    and loss = a.loss + b.loss
    and draw = a.draw + b.draw in
    let q = 0.5 *. float_of_int (2 * win + draw) /. float_of_int total in
    { q; u; win; loss; draw; total; ptot }

  let update a = function
    | Win -> a + { q = 1.0; u = 0.; win = 1; loss = 0; draw = 0; total = 1; ptot = 0 }
    | Loss -> a + { q = 0.; u = 0.; win = 0; loss = 1; draw = 0; total = 1; ptot = 0 }
    | Draw -> a + { q = 0.5; u = 0.; win = 0; loss = 0; draw = 1; total = 1; ptot = 0}

  let ( = ) a b = a.total = b.total

  let ( $= ) a b =
    abs_float (a.q -. b.q) <. 1e-5 && abs_float (a.u -. b.u) <. 1e-5

  let ( > ) a b = a.total > b.total

  let score q a =
    sqrt ((log (Float.of_int a.ptot)) /. Float.of_int (a.total)) +. q *. sqrt a.u

  let score_self a = score a.q a
  let score_other a = score (1. -. a.q) a

  let ( $< ) a b =
    if a $= b then false
    else score_other a >. score_other b

  let ( $> ) a b =
    if a $= b then false
    else score_self a >. score_self b

  let print a = Printf.printf
                "(q: %f, u: %f, win: %i, draw: %i, loss: %i, total: %i, ptot: %i)\tscore = %f"
                a.q a.u a.win a.draw a.loss a.total a.ptot (score_self a)
end

module type S = sig
  type t
  val most_favored_move : int -> t -> debug -> Index.t
end

module Make (M : GAME) : S
  with type t = M.t = struct

  type t = M.t

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
    let _, _, x, _ = Tree.node_elt n in
    Score.print x

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

  (* TODO: Replace ExpansionError with something concrete for games in which
   * players may run out of moves mid-game, such as reversi *)
  let rec expand_one_level tree =
    let rec list_branches ptot brd = function
      | [] -> []
      | n :: tl ->
          let node' =
            let p = M.curr_player brd in
            let brd' = M.move n brd in
            let p' = M.curr_player brd' in
            let leaf = Tree.Leaf (n, p', Score.init 0, brd') in
            match (p, p') with
            | One, Two | Two, One -> leaf
            | One, One | Two, Two ->
                if M.is_finished brd' then leaf
                else
                  match expand_one_level leaf with
                  | Tree.Leaf _ -> raise ExpansionError
                  | Tree.Node ((_, p', score, brd'), l) ->
                      Tree.Node ((n, p', score, brd'), l)
          in node' :: list_branches ptot brd tl in
    let i, p, s, b =
      match tree with
      | Tree.Leaf (indx, plyr, score, brd) -> indx, plyr, score, brd
      | Tree.Node _ -> raise ExpansionError in
    match M.available_moves b with
    | [] -> raise ExpansionError
    | moves -> Tree.Node ((i, p, s, b), list_branches (Score.parent_total s) b moves)

  let playout player tree =
    let rec aux player = function
      | Tree.Node ((i, p, f, b), branches) ->
          let branch =
            match (p, player) with
            | One, One | Two, Two -> pick _branch_gt _branch_eq branches
            | _ -> pick _branch_lt _branch_eq branches in
          let outcome, branch' = aux player branch in
          let f' = Score.update f outcome in
          let same_branch = fun a b ->
            let i, _, _, _ = Tree.node_elt a
            and j, _, _, _ = Tree.node_elt b in
            Index.(i = j) in
          let branches' = replace_branch same_branch branch branch' branches in
          outcome, Tree.Node ((i, p, f', b), branches')
      | Tree.Leaf (i, p, f, board) as leaf ->
          if M.is_finished board then
            let outcome = 
              match M.winner_is board with
              | None -> Draw
              | Some p ->
                match (p, player) with
                | One, One | Two, Two -> Win
                | _ -> Loss in
            outcome, Tree.Leaf (i, p, Score.update f outcome, board)
          else aux player (expand_one_level leaf) in
    let _, tree' = aux player tree in
    tree'

  let most_favored_move maxIter board dbg =
    let player = M.curr_player board in
    let initScore = Score.init 0 in
    let emptyTree = Tree.Leaf (Index.init (), player, initScore, board) in
    let tree = Iter.repeat ()
      |> Iter.take maxIter
      |> Iter.fold (fun tree _ -> playout player tree) emptyTree in
    match tree with
    | Leaf _ -> Index.init () (* inaccessible branch *)
    | Node (_, branches) ->
        let _ =
          match dbg with
          | Release -> ();
          | Debug ->
              let l = List.map Tree.node_elt branches in
              List.iter (fun (i, _, f, _) ->
                print_newline ();
                Printf.printf "%i  " (Index.to_int i + 1);
                Score.print f;
                print_string " ";
                ) l in
        print_newline ();
        let i, _, _, _ = Tree.node_elt (pick _ord _eq branches) in
        i
end
