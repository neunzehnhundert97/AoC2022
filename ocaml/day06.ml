module CharSet = Set.Make (Char)

let day = 6

let print_char_set char_set =
  print_char '{';
  CharSet.iter (Printf.printf "'%c',") char_set;
  print_endline "}"

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
  Shared.do_day 6 solve1 solve2 Shared.read_file_to_string string_of_int 11 26
