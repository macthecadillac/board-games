(* TODO: Make the simulations more efficient by narrowing/extending game trees
 * from previous moves instead of building a new one from the ground up for
 * every move *)
open Containers
open Common

module T = struct
  (* TODO: perhaps one day rewrite as the more common tree structure (also used
   * later in the code as FavHistTree) *)
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
      num   : int;
      denom : int;
    }

    type outcome = Positive | Negative | Indecisive

    let init () = { num = 0; denom = 0 }

    let as_float s = match s.denom with
        0 -> 0.
      | _ -> Float.of_int s.num /. (Float.of_int s.denom)

    let promote s =
      { num = s.num + 2; denom = s.denom + 2 }

    let demote s = { s with denom = s.denom + 2 }

    let mote s = { num = s.num + 1; denom = s.denom + 2 }

    let (+) a b = { num = a.num + b.num;
                    denom = a.denom + b.denom }

    let (>) a b = as_float a -. (as_float b) >. 1e-10

    let (=) a b = Float.abs (as_float a -. (as_float b)) <. 1e-10
  end

  module FavHist = struct
    type t = {
      one : int;
      two : int;
    }

    let init () = { one = 0; two = 0 }

    let is_null a = a.one = 0 && a.two = 0

    let bump a = function
        None     -> { one = a.one + 1; two = a.two + 1 }
      | Some One -> { a with one = a.one + 2 }
      | Some Two -> { a with two = a.two + 2 }

    let as_favorability a player =
      let denom = a.one + a.two in
      match player with
        One -> Favorability.({ num = a.one; denom })
      | Two -> Favorability.({ num = a.two; denom })

    let print a = Printf.printf "{ one = %i; two = %i }" a.one a.two
  end

  (* TODO: a tree is efficient enough for mancala, but for other games, a graph
   * like structure would be far superior *)
  module FavHistTree = struct
    (* we don't need to keep track of which player made which move because there
     * is only one way for a given tree to be traversed. You might want to
     * change this when switching over to graph like structures. *)
    type t = Node of Index.t * FavHist.t * t list

    let init i = Node (i, FavHist.init (), [])

    let depth a =
      let rec aux d a =
        let Node (i, f, l) = a in
        match l with
            [] -> d
          | l  -> List.map (aux (d + 1)) l |> List.fold_left max 0
      in aux 0 a
    
    let rec choose_branch i n =
      let rec aux i = function
          []       -> FavHist.init (), init i
        | hd :: tl ->
            let Node (j, f, _) = hd in
            if Index.(i = j) then f, n
            else aux i tl
      in
      let Node (_, _, l) = n in
      aux i l

    let rec update_from_last_playout winner tree record =
      match record with
        []             -> tree
      | indx :: nxtMvs ->
          let rec aux = function
              []         -> let newNode = Node (indx, FavHist.(bump (init ()) winner), []) in
                            let newsubTr = update_from_last_playout winner newNode nxtMvs in
                            newsubTr :: []
            | hd :: tail ->
              let Node (jj, ff, l) = hd in
              if Index.(jj = indx) then
                let newNode = Node (jj, FavHist.bump ff winner, l) in
                let newSubTr = update_from_last_playout winner newNode nxtMvs in
                newSubTr :: tail
              else hd :: (aux tail)
          in
          let Node (j, f, l) = tree in
          Node (j, f, aux l)

    let load s =
      try
        let channel = open_in_bin s in
        let t = (Marshal.from_channel channel : t) in
        close_in channel;
        t
      with Sys_error _ -> init (Index.of_int 0)

    let save (t : t) s =
      (* for some weird reason the file permission of 644 is represented as 422
       * in ocaml *)
      let channel = open_out_gen [Open_binary; Open_creat; Open_wronly] 422 s in
      (Marshal.to_channel channel t []);
      close_out channel
  end

  type sim_mode = Manual of Index.t | Random

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

  let most_favored_move searchLimit player board histTree =
    let module F = Favorability in
    let pick_max a b =
      let _, fA = a and _, fB = b in
      F.(if fA > fB then a
         (* pick one at random if both have the same favorability *)
         else if fA = fB then let r = Random.pick_list [a; b] in Random.run r
         else b) in
    (* extract contents of a leaf with the assumption that it only receives
     * a leaf as its argument *)
    let unpack_leaves = function
        Tree.Node (i, _, l) -> i, F.init () (* Inaccessible branch in this context *)
      | Tree.Leaf (i, _, a) -> 
          let fh, _ = FavHistTree.choose_branch i histTree in
          (* FavHist.print fh; *)
          match FavHist.is_null fh with
              true  -> i, a
            | false -> i, Favorability.(FavHist.as_favorability fh player + a)
    in

    (* expand 3 levels down the game tree *)
    let tree = Tree.expand_to_leaves 3 player board (Favorability.init ())
      |> compute_favorability searchLimit player
      |> Tree.map (fun (f, _) -> f) in (* filters out only the favorabilities *)
    let moves = match tree with
        Tree.Leaf _ as l    -> [l]
      | Tree.Node (_, _, l) -> List.map (Tree.collapse F.(+) (F.init ())) l in
    let unpackedBranches = List.map unpack_leaves moves in
    (* List.iter (fun (_, x) -> Favorability.(Printf.printf "(%i, %i) " x.num *)
    (* x.denom)) unpackedBranches; *)
    (* print_newline (); *)
    let indx, _ = List.fold_left pick_max (List.hd unpackedBranches)
                                          unpackedBranches
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
    let unpackedBranches = List.map (unpack_leaves (F.init ())) moves in
    let indx, _ = List.fold_left max_indx (List.hd unpackedBranches) unpackedBranches
    in indx
end
