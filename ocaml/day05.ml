open Shared

let empty_line_regex = Str.regexp "\n\n"
let newline_regex = Str.regexp "\n"

type instruction = { amount : int; source : int; destination : int }

let print_instruction { amount; source; destination } =
  Printf.printf "%d crates should be moved from stack %d to %d\n" amount source
    destination

let print_item item = Printf.printf "[%c]" item

let print_stack index stack =
  Printf.printf "%d: " index;
  List.iter print_item stack;
  print_string "\n"

let print_stacks = List.iteri print_stack
let first_pair_option = function a :: b :: _ -> Some (a, b) | _ -> None
let bimap_tuple func1 func2 (a, b) = (func1 a, func2 b)

let parse_initial_stack input =
  print_endline "Intial stack";
  print_string input;
  print_endline input;
  [ [ 'a'; 'c' ]; [ 'd'; 'e' ] ]

let instruction_regex =
  let number_group = Re.group (Re.rep1 Re.digit) in
  Re.(
    compile
      (seq
         [
           str "move ";
           number_group;
           str " from ";
           number_group;
           str " to ";
           number_group;
         ]))

let string_to_instruction line =
  let group = Re.exec instruction_regex line in
  let get_group = Re.Group.get_opt group in
  match (get_group 1, get_group 2, get_group 3) with
  | Some a, Some b, Some c ->
      Some
        {
          amount = int_of_string a;
          source = int_of_string b;
          destination = int_of_string c;
        }
  | _ -> None

let parse_instructions input =
  print_endline "Orders";
  input |> Str.split newline_regex
  |> List.map string_to_instruction
  |> List.map Option.to_list |> List.flatten

let parse_input input =
  input |> Str.split empty_line_regex |> first_pair_option
  |> Option.map (bimap_tuple parse_initial_stack parse_instructions)

let process_orders (stacks, orders) =
  List.iter print_instruction orders;
  print_stacks stacks

let solve1 input =
  input |> parse_input |> Option.map process_orders |> fun _ -> ""

let _ =
  print_endline "Day 5";
  let example_data = read_file_to_string "example05.data" in
  if example_data |> solve1 |> String.equal "CMZ" then
    print_endline "Part 1 verified"
  else failwith "Part 1 failed";

  let real_data = read_file_to_string "input05.data" in
  real_data |> solve1 |> Printf.printf "Solution 1: %s\n"
