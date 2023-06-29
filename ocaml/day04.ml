open Shared
module IntSet = Set.Make (Int)

let do_with_two func = function
  | [ a; b ] -> (func a, func b)
  | _ -> failwith "not two"

let ranges_to_sets (start, ending) =
  List.init (ending - start + 1) (( + ) start) |> IntSet.of_list

let parse_partial_line_to_set pline =
  pline
  |> Str.split (Str.regexp "-")
  |> do_with_two int_of_string |> ranges_to_sets

let does_fully_contain (set_1, set_2) =
  IntSet.subset set_1 set_2 || IntSet.subset set_2 set_1

let does_partially_contain (set_1, set_2) =
  not (IntSet.inter set_1 set_2 |> IntSet.is_empty)

let parse_line_to_sets line =
  line |> Str.split (Str.regexp ",") |> do_with_two parse_partial_line_to_set

let bool_to_int b = if b then 1 else 0
let sum = List.fold_left ( + ) 0

let process_line_to_result func line =
  line |> parse_line_to_sets |> func |> bool_to_int

let solve_with_predicate pred lines =
  lines |> List.map (process_line_to_result pred) |> sum

let solve1 lines = lines |> solve_with_predicate does_fully_contain
let solve2 lines = lines |> solve_with_predicate does_partially_contain

let _ =
  let example_data = read_file_to_string_list "example04.data" in
  let data = read_file_to_string_list "input04.data" in
  print_endline "== Day 04 ==";
  print_endline "Part 1";
  if solve1 example_data = 2 then print_endline "Solved example correctly"
  else failwith "Example failed";
  data |> solve1 |> Printf.printf "Solution 1: %d\n";

  print_endline "Part 2";
  if solve2 example_data = 4 then print_endline "Solved example correctly"
  else failwith "Example failed";
  data |> solve2 |> Printf.printf "Solution 2: %d\n"
