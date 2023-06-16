
let file = if true then "input01.data" else "example01.data"

(** Read all content of a given [filename]. *)
let read_file filename =
  In_channel.(with_open_text filename input_all) |> (Str.split (Str.regexp "\n"))

  
(** Group the chunks and sum them up. *)
let group_chunks (lines: string list) =
  let rec inner lines current acc =
    match lines with
    | [] -> acc
    | head :: tail when head = "" -> inner tail 0 (acc @ [current])
    | head :: tail -> inner tail (int_of_string head + current) acc
  in

  inner lines 0 []

(** Computes the maximum value of [list]. An empty list gives a maximum of 0. *)
let max list =
  let rec inner acc = function
    | [] -> acc
    | head :: tail -> inner (if acc > head then acc else head) tail in
  inner 0 list

let _ =
  print_endline "== Day 01 ==";
  let data = read_file file in
    data |> group_chunks |> max |> print_int;
  print_string "\n";