open Shared
module CharSet = Set.Make (Char)

let file = if true then "input03.data" else "example03.data"

(** Convert a charcter into the corresponding priority.
    A char outside [A-Za-Z] will yield unsound results. *)
let char_to_prio c =
  if c <= 'z' && c >= 'a' then int_of_char c - int_of_char 'a' + 1
  else int_of_char c - int_of_char 'A' + 27

(** Split a string into a tuple of same-size halves.
    Assumes the string has an even length. *)
let split_compartments line =
  let length = String.length line / 2 in
  (String.sub line 0 length, String.sub line length length)

(** Search through a tuple of string to find the first common char of both. *)
let find_common_element comparments =
  let first, second = comparments in
  let rec search index =
    if index > String.length first then failwith "list exceeded without result"
    else if String.contains second first.[index] then first.[index]
    else search (index + 1)
  in
  search 0

let group_in_tripples list =
  let rec inner remaining acc =
    match remaining with
    | [] -> acc
    | a :: b :: c :: rest -> inner rest ((a, b, c) :: acc)
    | _ -> failwith "invalid list"
  in
  inner list []

(* Following function and operator were taken from
   https://cs3110.github.io/textbook/chapters/data/lists.html#tail-recursion *)

(** [from i j l] is the list containing the integers from [i] to [j],
    inclusive, followed by the list [l].
    Example:  [from 1 3 [0] = [1; 2; 3; 0]] *)
let rec from i j l = if i > j then l else from i (j - 1) (j :: l)

(** [i -- j] is the list containing the integers from [i] to [j], inclusive. *)
let ( -- ) i j = from i j []

(* Taken from https://batsov.com/articles/2022/10/24/ocaml-tips-converting-a-string-to-a-list-of-characters/ *)
let explode_string s = List.init (String.length s) (String.get s)

let all_chars =
  (int_of_char 'A' -- int_of_char 'Z') @ (int_of_char 'a' -- int_of_char 'z')
  |> List.map char_of_int |> CharSet.of_list

let find_common_in_group group =
  let first, second, third = group in
  let first_set = first |> explode_string |> CharSet.of_list in
  let second_set = second |> explode_string |> CharSet.of_list in
  let third_set = third |> explode_string |> CharSet.of_list in
  let common_chars =
    all_chars |> CharSet.inter first_set |> CharSet.inter second_set
    |> CharSet.inter third_set
  in
  if CharSet.cardinal common_chars == 1 then CharSet.choose common_chars
  else failwith "Set has the wrong site"

let solve1 lines =
  lines
  |> List.map (split_compartments >> find_common_element >> char_to_prio)
  |> List.fold_left ( + ) 0

let solve2 lines =
  lines |> group_in_tripples
  |> List.map (find_common_in_group >> char_to_prio)
  |> List.fold_left ( + ) 0

let _ =
  let data = read_file_to_string_list file in
  print_endline "== Day 03 ==";
  print_endline "Solution 1";
  data |> solve1 |> Printf.printf "%d\n";
  print_endline "Solution 2";
  data |> solve2 |> Printf.printf "%d\n"
