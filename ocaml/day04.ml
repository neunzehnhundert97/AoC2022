open Shared
module IntSet = Set.Make (Int)

let solve1 lines = List.length lines

let _ =
  let example_data = read_file_to_string_list "example04.data" in
  let data = read_file_to_string_list "input04.data" in
  print_endline "== Day 04 ==";
  print_endline "Example 1\n";
  if solve1 example_data = 2 then print_endline "Solved correctly"
  else failwith "Example failed";
  data |> solve1 |> Printf.printf "Solution 1: %d\n"
