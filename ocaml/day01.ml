let file = if true then "input01.data" else "example01.data"

(** Group the chunks and sum them up. *)
let group_chunks (lines : string list) =
  let rec inner lines current acc =
    match lines with
    | [] -> acc
    | head :: tail when head = "" -> inner tail 0 (acc @ [ current ])
    | head :: tail -> inner tail (int_of_string head + current) acc
  in

  inner lines 0 []

(** Computes the maximum value of [list]. An empty list gives a maximum of 0. *)
let max list =
  let rec inner acc = function
    | [] -> acc
    | head :: tail -> inner (if acc > head then acc else head) tail
  in
  inner 0 list

let solution1 data = data |> group_chunks |> max

let solution2 data =
  let sorted_data = data |> group_chunks |> List.sort Int.compare |> List.rev in
  match sorted_data with
  | a :: b :: c :: _ -> a + b + c
  | _ -> failwith "The list is not long enough"

let solution2_alt filename =
  Shared.(
    filename |> read_file_to_string
    |> Str.split (Str.regexp "\n\n")
    |> List.map
         (Str.split $ Str.regexp "\n" >> List.map int_of_string
        >> List.fold_left ( + ) 0)
    |> List.sort Int.compare |> List.rev
    |> function
    | a :: b :: c :: _ -> a + b + c
    | _ -> failwith "list to short")

let _ =
  let data = Shared.read_file_to_string_list file in
  print_endline "== Day 01 ==";
  print_endline "Solution 1";
  data |> solution1 |> print_int;
  print_string "\n";
  print_endline "Solution 2";
  data |> solution2 |> print_int;
  print_string "\n";
  print_string "\n";
  file |> solution2_alt |> Printf.printf "%d\n"
