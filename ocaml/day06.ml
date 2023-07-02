module CharSet = Set.Make (Char)

let day = 6

let print_char_set char_set =
  print_char '{';
  CharSet.iter (Printf.printf "'%c',") char_set;
  print_endline "}"

let test_and_do solve print test_value =
  let example_file = Printf.sprintf "example%02d.data" day in
  let data_file = Printf.sprintf "input%02d.data" day in
  let example_data = Shared.read_file_to_string example_file in
  let data = Shared.read_file_to_string data_file in
  if solve example_data = test_value then Printf.printf "Test succeeded\n"
  else failwith "Test failed";
  data |> solve |> print

let find_begin_of_thing amount_same input =
  let is_alpha c = c >= 'a' && c <= 'z' in

  let windowed list =
    let rec inner acc list =
      let firsts = Shared.take amount_same list in
      if List.length firsts = amount_same then
        inner (CharSet.of_list firsts :: acc) (List.tl list)
      else acc
    in

    List.rev (inner [] list)
  in

  let find_first_good_index list =
    let rec inner index = function
      | [] -> failwith "List exceeded without finding"
      | x :: xs ->
          if CharSet.cardinal x = amount_same then index
          else inner (index + 1) xs
    in
    amount_same + inner 0 list
  in

  input |> String.to_seq |> List.of_seq |> List.filter is_alpha |> windowed
  |> find_first_good_index

let solve1 = find_begin_of_thing 4
let solve2 = find_begin_of_thing 14

let _ =
  print_endline "Day 6";
  test_and_do solve1 (Printf.printf "Solution 1: %d\n") 11;
  test_and_do solve2 (Printf.printf "Solution 2: %d\n") 26
