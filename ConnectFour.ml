open Common
open Containers
open CCFun

type t = { currPlayer : player;
           currSide : int;
           otherSide : int }

(* bit-board representation of the winning configurations. The configurations
 * from smallest to largest represent configurations from lower left corner to
 * the right and up *)
let winConfigs = [| 15; 30; 60; 120; 1920; 3840; 7680; 15360; 245760; 491520;
                    983040; 1966080; 2113665; 2130440; 4227330; 4260880;
                    8454660; 8521760; 16843009; 16909320; 17043520; 31457280;
                    33686018; 33818640; 62914560; 67372036; 67637280; 125829120;
                    134744072; 135274560; 251658240; 270549120; 272696320;
                    541098240; 545392640; 1082196480; 1090785280; 2155905152;
                    2164392960; 2181570560; 4026531840; 4311810304; 4328785920;
                    8053063680; 8623620608; 8657571840; 16106127360;
                    17247241216; 17315143680; 32212254720; 34630287360;
                    34905128960; 69260574720; 69810257920; 138521149440;
                    139620515840; 275955859456; 277042298880; 279241031680;
                    515396075520; 551911718912; 554084597760; 1030792151040;
                    1103823437824; 1108169195520; 2061584302080; 2207646875648;
                    2216338391040; 4123168604160 |]

(* Encodings of the top-most available slot. If these are vacant then the column
 * is an available choice for the next move *)
let topRow = [ 34359738368; 68719476736; 137438953472; 274877906944;
               549755813888; 1099511627776; 2199023255552 ]

let pow2 = [| 1; 2; 4; 8; 16; 32; 64; 128; 256; 512; 1024; 2048; 4096; 8192;
              16384; 32768; 65536; 131072; 262144; 524288; 1048576; 2097152;
              4194304; 8388608; 16777216; 33554432; 67108864; 134217728;
              268435456; 536870912; 1073741824; 2147483648; 4294967296;
              8589934592; 17179869184; 34359738368; 68719476736; 137438953472;
              274877906944; 549755813888; 1099511627776; 2199023255552 |]

let init () = { currPlayer = One; currSide = 0; otherSide = 0 }

let available_moves a =
  List.foldi
  (fun l i elt ->
    if a.currSide lor elt = a.currSide then Index.of_int i :: l
    else l)
  [] topRow

let curr_player a = a.currPlayer

let check_winner bboard =
  Array.fold_while
  (fun s c ->
    if c lor bboard = bboard then true, `Stop
    else if c > bboard then false, `Stop
    else false, `Continue)
  false winConfigs

let is_finished a =
  check_winner a.currSide ||
  check_winner a.otherSide ||
  a.currSide + a.otherSide = 4398046511103

let is_valid_move n board = List.mem ~eq:Index.( = ) n (available_moves board)

let move indx board =
  let currPlayer = switch_player board.currPlayer in
  let currSide =
    List.init 6 (fun x -> pow2.(Index.to_int indx + 7 * x))
    |> List.fold_while
       (fun side i ->
         if i lor side = side then side, `Continue
         else side + i, `Stop)
       board.currSide in
  { board with currPlayer; currSide }

let winner_is a =
  if check_winner a.currSide then Some a.currPlayer
  else if check_winner a.otherSide then Some (switch_player a.currPlayer)
  else None

let print board =
  let int_to_padded_str = Int.to_string_binary %> String.pad ~c:'0' 42 in
  let currPlayerBString = int_to_padded_str board.currSide
  and otherPlayerBString = int_to_padded_str board.otherSide in
  let rawRepr = String.map2
                (fun a b ->
                  if Char.equal a '1' then 'o'
                  else if Char.equal b '1' then 'x'
                  else ' ')
                currPlayerBString otherPlayerBString in
  let rec _split_str s =
    let s1, s2 = String.take_drop 7 s in
    if String.equal s2 "" then []
    else s1 :: _split_str s2 in
  String.rev rawRepr
  |> _split_str
  |> List.rev
  |> List.iter print_endline
