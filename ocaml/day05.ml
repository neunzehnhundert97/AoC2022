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
let drop = function _ :: rest -> rest | _ -> []

let parse_stack_line line =
  let filter_func (index, _) = (index - 1) mod 4 = 0 in

  line |> String.to_seqi |> Seq.filter filter_func |> Seq.map snd |> List.of_seq

let rotate_matrix (matrix : 'a list list) =
  print_stacks matrix;
  print_endline "-------";
  let rec outer matrix acc =
    match matrix with [] -> acc | a :: rest -> outer rest (inner a acc [])
  and inner list acc acc_done =
    match (list, acc) with
    | [], _ -> acc_done
    | a :: rest, b :: acc_rest ->
        (* Printf.printf ";'%c' to %s" a (String.of_seq (List.to_seq b)); *)
        inner rest acc_rest (acc_done @ [ b @ [ a ] ])
    | a :: rest, [] ->
        (* Printf.printf ";\"%c\" " a; *)
        inner rest [] (acc_done @ [ [ a ] ])
  in
  outer matrix []

let parse_initial_stack input =
  print_endline "Intial stack";
  input |> Str.split newline_regex |> List.rev |> drop
  |> List.map parse_stack_line |> rotate_matrix

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
  input |> Str.split newline_regex |> List.filter_map string_to_instruction

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
