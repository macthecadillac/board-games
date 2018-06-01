open Containers
open Common

module Favorability = struct
  type t = {
    numerator   : int;
    denominator : int;
  }
  type outcome =
    | Positive
    | Negative
    | Indecisive
  let init () = { numerator = 0; denominator = 0 }
  let as_float s = match s.denominator with
    | 0 -> 0.
    | _ -> Float.of_int s.numerator /. (Float.of_int s.denominator)
  let promote s =
    { numerator = s.numerator + 2; denominator = s.denominator + 2 }
  let demote s = { s with denominator = s.denominator + 2 }
  let mote s = { numerator = s.numerator + 1; denominator = s.denominator + 2 }
  let (>) a b = as_float a >. (as_float b)
  let (=) a b = Float.abs (as_float a -. (as_float b)) <. 1e-8
end

type sim_mode =
  | Manual of Index.t
  | Random

let available_moves board =
  let nListFiltered, boardConfigsFiltered =
    List.init 6 (fun x -> Index.of_int x)
      |> List.map (fun x -> x, Board.remove_pieces x board)
      |> List.filter (fun x -> match x with
                      | (_, Some _) -> true
                      | (_, None)   -> false)
      |> List.split in
  List.filter_map (fun x -> x) boardConfigsFiltered
    |> List.combine nListFiltered
              
let random_move availableMoves =
  let state = Random.pick_list availableMoves in
  Random.run state

let rec random_playout mode player board =
  match Board.is_finished board with
  | true  -> (match Board.winner_is board with
      | None   -> Favorability.Indecisive
      | Some p -> (match p, player with
          | One, One | Two, Two -> Favorability.Positive
          | _                   -> Favorability.Negative))
  | false -> match mode with
      | Random ->
          let availableMoves = available_moves board in
          let n, (count, board) = random_move availableMoves in
          let newBoard = Board.dist (Index.inc n) count board in
          random_playout Random player newBoard
      | Manual n  -> match Board.remove_pieces n board with
          | None                -> Favorability.Indecisive
          | Some (count, board) ->
              let newBoard = Board.dist (Index.inc n) count board in
              random_playout Random player newBoard

let compute_favorability searchLimit player board =
  let indcs, _ = board |> available_moves |> List.split in
  let nIndcs = List.length indcs in
  let rec aux sl favorability =
    if sl = 0 then List.combine indcs favorability
    else 
      let fav =
        indcs
        |> List.map (fun n -> random_playout (Manual n) player board)
        |> List.map2 (fun f0 f -> match f with
                      | Favorability.Positive   -> Favorability.promote f0
                      | Favorability.Negative   -> Favorability.demote f0
                      | Favorability.Indecisive -> Favorability.mote f0)
                     favorability
    in aux (sl - 1) fav
  in aux searchLimit (List.init nIndcs (fun x -> Favorability.init ()))

let most_favored_move searchLimit player board =
  let pick_max a b =
    let _, fA = a and _, fB = b in
    Favorability.(
      if fA > fB then a
      (* pick one at random if both have the same favorability *)
      else if fA = fB then let r = Random.pick_list [a; b] in Random.run r
      else b
    ) in
  let indx, _ =
    let entries = compute_favorability searchLimit player board in
    let firstEntry = List.hd entries in
    List.fold_left pick_max firstEntry entries
  in indx
