(* TODO: Make the simulations more efficient by narrowing/extending game trees
 * from previous moves instead of building a new one from the ground up for
 * every move *)
(* TODO: Change the training mode such that the first AI is forced to evaluate
 * every first move so the the second AI won't be blinsided when I'm playing an
 * opening move that it has never seen *)
open Containers
open Common

module T = struct
  (* TODO: perhaps one day rewrite as the more common tree structure (also used
   * later in the code as HistTree) *)
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

module MCSearch = struct
  module Favorability = struct
    type t = {num: int; denom: int}

    type outcome = Positive | Negative | Indecisive

    let init () = {num= 0; denom= 0}

    let is_unity a = a.num = a.denom

    let as_float s =
      match s.denom with
      | 0 -> 0.
      | _ -> Float.of_int s.num /. Float.of_int s.denom

    let promote s = {num= s.num + 2; denom= s.denom + 2}

    let demote s = {s with denom= s.denom + 2}

    let mote s = {num= s.num + 1; denom= s.denom + 2}

    let ( + ) a b = {num= a.num + b.num; denom= a.denom + b.denom}

    let ( > ) a b = as_float a -. as_float b >. 1e-10

    let ( = ) a b = Float.abs (as_float a -. as_float b) <. 1e-10

    let weight w a = {num= w * a.num; denom= w * a.denom}

    let print a = Printf.printf "{ num = %i; denom = %i }" a.num a.denom
  end

  module Hist = struct
    type t = {one: int; two: int; draw: int}

    let init () = {one= 0; two= 0; draw= 0}

    let is_null a = a.one = 0 && a.two = 0

    let bump a = function
      | None -> {a with draw= a.draw + 1}
      | Some One -> {a with one= a.one + 1}
      | Some Two -> {a with two= a.two + 1}

    let as_float a player =
      let one_sq = ((2 * a.one) + a.draw) * ((2 * a.one) + a.draw) in
      let two_sq = ((2 * a.two) + a.draw) * ((2 * a.two) + a.draw) in
      match player with
      | One -> float_of_int one_sq /. float_of_int (one_sq + two_sq)
      | Two -> float_of_int two_sq /. float_of_int (one_sq + two_sq)

    let as_favorablity a p =
      let num, denom =
        match p with
        | One -> ((2 * a.one) + a.draw, 2 * (a.one + a.two + a.draw))
        | Two -> ((2 * a.two) + a.draw, 2 * (a.one + a.two + a.draw))
      in
      Favorability.{num; denom}

    let print a =
      Printf.printf "{ one = %i; two = %i; draw = %i }" a.one a.two a.draw
  end

  module Depth = Abstype.MakeMInt (Abstype.I)

  (* TODO: a tree is efficient enough for mancala, but for other games, a graph
   * like structure would be far superior *)
  module HistTree = struct
    (* we don't need to keep track of which player made which move because there
     * is only one way for a given tree to be traversed. You might want to
     * change this when switching over to graph like structures. *)
    type t = Node of Index.t * Depth.t * Hist.t * t list

    let init i n = Node (i, n, Hist.init (), [])

    let depth a =
      let rec aux d a =
        let (Node (i, n, f, l)) = a in
        match l with
        | [] -> d
        | l -> List.map (aux (d + 1)) l |> List.fold_left max 0
      in
      aux 0 a

    let rec choose_branch i n =
      let rec aux i d = function
        | [] -> (d, Hist.init (), init i d)
        | hd :: tl ->
            let (Node (j, dd, f, _)) = hd in
            if Index.(i = j) then (dd, f, hd) else aux i d tl
      in
      let (Node (_, d, _, l)) = n in
      aux i d l

    let rec update_from_last_playout winner tree record =
      let module D = Depth in
      let rec aux winner tree record depth =
        match record with
        | [] -> tree
        | indx :: nxtMvs ->
            let rec aux2 = function
              | [] ->
                  let newNode =
                    Node (indx, depth, Hist.(bump (init ()) winner), [])
                  in
                  let newsubTr = aux winner newNode nxtMvs (D.inc depth) in
                  [newsubTr]
              | hd :: tail ->
                  let (Node (jj, dd, ff, l)) = hd in
                  if Index.(jj = indx) then
                    let newNode = Node (jj, dd, Hist.bump ff winner, l) in
                    let newSubTr = aux winner newNode nxtMvs (D.inc depth) in
                    newSubTr :: tail
                  else hd :: aux2 tail
            in
            let (Node (j, d, f, l)) = tree in
            Node (j, d, f, aux2 l)
      in
      aux winner tree record (Depth.init ())

    let load s =
      try
        let channel = open_in_bin s in
        let t : t = Marshal.from_channel channel in
        close_in channel ; t
      with Sys_error _ -> init (Index.init ()) (Depth.init ())

    let save (t: t) s =
      (* for some weird reason the file permission of 644 is represented as 422
       * in ocaml *)
      let channel =
        open_out_gen [Open_binary; Open_creat; Open_wronly] 422 s
      in
      Marshal.to_channel channel t [] ;
      close_out channel
  end

  type sim_mode = Manual of Index.t | Random

  let random_move availableMoves =
    let state = Random.pick_list availableMoves in
    Random.run state

  let rec random_playout mode player board =
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
        match mode with
        | Random ->
            let n = random_move availableMoves in
            let newBoard = Board.move n board in
            random_playout Random player newBoard
        | Manual n ->
          match Board.is_valid_move n board with
          | false -> Favorability.Indecisive
          | true ->
              let newBoard = Board.move n board in
              random_playout Random player newBoard

  let rec compute_favorability searchLimit player tree =
    let rec random_play sl (f0, b) =
      (* Printf.printf "%i " sl; *)
      if sl = 0 then (f0, b)
      else
        let f =
          match random_playout Random player b with
          | Favorability.Positive -> Favorability.promote f0
          | Favorability.Negative -> Favorability.demote f0
          | Favorability.Indecisive -> Favorability.mote f0
        in
        random_play (sl - 1) (f, b)
    in
    Tree.map (random_play searchLimit) tree

  let compute_score hist fav depth player =
    match Hist.is_null hist with
    | true -> Favorability.as_float fav
    | false ->
      match Favorability.is_unity fav with
      | true -> Favorability.as_float fav
      | false ->
          (* let histWeight = *)
          (*   0.6 -. 0.5 /. Depth.(inc depth |> to_int |> float_of_int) in *)
          (*   (1* 0.5 in *1) *)
          (* let mcWeight = 1. -. histWeight in *)
          (* histWeight *. Hist.as_float hist player +. mcWeight *. Favorability.as_float fav *)
          let open Favorability in
          Hist.as_favorablity hist player
          |> weight 100 |> ( + ) fav |> as_float

  let most_favored_move searchLimit player board histTree =
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
      | Tree.Node (i, _, l) ->
          (i, 0.) (* Inaccessible branch in this context *)
      | Tree.Leaf (i, _, a) ->
          let d, fh, _ = HistTree.choose_branch i histTree in
          Printf.printf " %i=" (Index.to_int i) ;
          Hist.print fh ;
          Favorability.print a ;
          (i, compute_score fh a d player)
    in
    (* expand 3 levels down the game tree *)
    let tree =
      Tree.expand_to_leaves 3 player board (Favorability.init ())
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
    print_newline () ;
    print_string "Scores: " ;
    List.iter
      (fun (i, x) -> F.(Printf.printf "%i=%f " (Index.to_int i) x))
      scores ;
    print_newline () ;
    let indx, _ = List.fold_left pick_max (List.hd scores) scores in
    Index.to_int indx |> Printf.printf "Best move: %i\n\n" ;
    indx
end

(* FIXME: Doesn't work all too well yet. Take into account of which player made
 * which move when collapsing the game tree to make it truly minimax *)
module MiniMax = struct
  module Favorability = Abstype.MakeMInt (Abstype.I)

  let scorer aiPlayer board =
    match Board.is_finished board with
    | true -> (
      match Board.winner_is board with
      | None -> Favorability.init ()
      | Some p ->
        match (p, aiPlayer) with
        | One, One | Two, Two -> Favorability.of_int 1000
        | _ -> Favorability.of_int (-1000) )
    | false ->
        let get_score hb = HalfBoard.get_tally hb |> Count.to_int in
        let relScore =
          (Board.curr_side board |> get_score)
          - (Board.other_side board |> get_score)
        in
        match (aiPlayer, Board.curr_player board) with
        | One, One | Two, Two -> Favorability.of_int relScore
        | _ -> -1 * relScore |> Favorability.of_int

  let max_indx a b =
    let _, fA = a and _, fB = b in
    let open Favorability in
    if fA > fB then a
      (* pick one at random if both have the same favorability *)
    else if fA = fB then
      let r = Random.pick_list [a; b] in
      Random.run r
    else b

  let pick_max a b =
    let open Favorability in
    if a > b then a
    else if a = b then
      let r = Random.pick_list [a; b] in
      Random.run r
    else b

  let rec compute_favorability aiPlayer tree =
    let aux (_, b) = scorer aiPlayer b in
    Tree.map aux tree

  let most_favored_move searchDepth aiPlayer board =
    let module F = Favorability in
    let unpack_leaves default = function
      | Tree.Leaf (i, _, a) -> (i, a)
      | Tree.Node (i, _, l) -> (i, default)
      (* Inaccessible branch in this context *)
    in
    let tree =
      Tree.expand_to_leaves searchDepth aiPlayer board (F.init ())
      |> compute_favorability aiPlayer
    in
    let moves =
      match tree with
      | Tree.Leaf _ as l -> [l]
      | Tree.Node (_, _, l) -> List.map (Tree.collapse pick_max (F.init ())) l
    in
    let unpackedBranches = List.map (unpack_leaves (F.init ())) moves in
    let indx, _ =
      List.fold_left max_indx (List.hd unpackedBranches) unpackedBranches
    in
    indx
end
