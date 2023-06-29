open Shared

let file = if true then "input02.data" else "example02.data"

type shape = Rock | Paper | Scisor

(** Parse a single character into a [share]. *)
let parse_shape = function
  | 'A' | 'X' -> Rock
  | 'B' | 'Y' -> Paper
  | 'C' | 'Z' -> Scisor
  | _ -> failwith "illegal input"

let string_of_shape = function
  | Rock -> "Rock"
  | Paper -> "Paper"
  | Scisor -> "Scisor"

let value_of_shape = function Rock -> 1 | Paper -> 2 | Scisor -> 3

let winner_value = function
  | Rock, Paper | Paper, Scisor | Scisor, Rock -> 6
  | Rock, Rock | Scisor, Scisor | Paper, Paper -> 3
  | _ -> 0

let win_against = function Rock -> Paper | Scisor -> Rock | Paper -> Scisor
let loose_against = function Rock -> Scisor | Scisor -> Paper | Paper -> Rock

let print_round round =
  print_endline
    ((round |> fst |> string_of_shape) ^ " " ^ (round |> snd |> string_of_shape))

(** Parses a string into a round. *)
let parse_round1 line =
  if String.length line != 3 then failwith "illegal round input"
  else (parse_shape line.[0], parse_shape line.[2])

let match_shape_on_strategy = function
  | 'X' -> loose_against (* lose *)
  | 'Y' -> fun x -> x (* draw *)
  | 'Z' -> win_against (* win *)
  | _ -> failwith "illegal input"

let parse_round2 line =
  if String.length line != 3 then failwith "illegal round input"
  else
    let enemy = parse_shape line.[0] in
    let answer = match_shape_on_strategy line.[2] enemy in
    (enemy, answer)

(** Cpmpute the value of a [round]. *)
let eval_round round = winner_value round + (value_of_shape $ snd round)

let solve1 lines =
  lines |> List.map (parse_round1 >> eval_round) |> List.fold_left ( + ) 0

let solve2 lines =
  lines |> List.map (parse_round2 >> eval_round) |> List.fold_left ( + ) 0

let _ =
  let data = read_file_to_string_list file in
  print_endline "Part 1";
  data |> solve1 |> Printf.printf "%d\n";
  print_endline "Part 2";
  data |> solve2 |> Printf.printf "%d\n"
