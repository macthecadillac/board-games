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
  let init () = { numerator = 0; denominator = 1 }
  let as_float s = Float.of_int s.numerator /. (Float.of_int s.denominator)
  let promote s =
    { numerator = s.numerator + 1; denominator = s.denominator + 1 }
  let demote s = { s with denominator = s.denominator + 1 }
  let (>) a b = as_float a >. (as_float b)
end

type sim_mode =
  | Manual of Index.t
  | Random

let available_moves board =
  let nList = List.init 6 (fun x -> Index.of_int x) in
  let boardConfigs =
    List.map (fun x -> Mechanics.remove_pieces x board) nList in
  let nListFiltered, boardConfigsFiltered =
    List.combine nList boardConfigs
      |> List.filter (fun x -> match x with
                      | (_, Some _) -> true
                      | (_, None)   -> false)
      |> List.split in
  List.filter_map (fun x -> x) boardConfigsFiltered
    |> List.combine nListFiltered
              
let random_move availableMoves =
  let state = Random.pick_list availableMoves in
  Random.run state

let rec monte_carlo_simulation mode player board =
  match Mechanics.is_finished board with
  | true  -> (match Mechanics.winner_is board with
      | None   -> Favorability.Negative
      | Some p -> (match p, player with
          | One, One | Two, Two -> Favorability.Positive
          | _                   -> Favorability.Negative))
  | false -> match mode with
      | Random   ->
          let availableMoves = available_moves board in
          let n, (count, board) = random_move availableMoves in
          let newBoard = Mechanics.play n count board in
          monte_carlo_simulation Random One newBoard
      | Manual n -> match Mechanics.remove_pieces n board with
          | None                -> Favorability.Indecisive
          | Some (count, board) ->
              let newBoard = Mechanics.play n count board in
              monte_carlo_simulation Random One newBoard

let compute_favorability searchLimit player board =
  let rec aux sl favorability =
    if sl = 0 then favorability
    else let fav =
      List.init 6 (fun x -> Index.of_int x)
        |> List.map (fun n -> monte_carlo_simulation (Manual n) player board)
        |> List.map2 (fun f0 f -> match f with
                      | Favorability.Positive   -> Favorability.promote f0
                      | Favorability.Negative   -> Favorability.demote f0
                      | Favorability.Indecisive -> f0)
                     favorability
    in aux (sl - 1) fav
  in aux searchLimit (List.init 6 (fun x -> Favorability.init ()))

let most_favored_move searchLimit player board =
  let indx, _ = compute_favorability searchLimit player board
    |> List.combine (List.init 6 (fun x -> Index.of_int x))
    |> List.fold_left (fun a b ->
                         let _, fA = a
                         and _, fB = b in
                         Favorability.(if fA > fB then a else b))
                      (Index.of_int 0, Favorability.init ())
  in indx
