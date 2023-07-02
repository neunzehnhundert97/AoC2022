open Shared

let empty_line_regex = Str.regexp "\n\n"
let newline_regex = Str.regexp "\n"

type instruction = { amount : int; source : int; destination : int }

let print_instruction { amount; source; destination } =
  Printf.printf "%d crates should be moved from stack %d to %d\n" amount source
    destination

let print_stacks =
  let print_item item = Printf.printf "[%c]" item in
  let print_stack index stack =
    Printf.printf "%d: " (index + 1);
    List.iter print_item stack;
    print_string "\n"
  in
  List.iteri print_stack

let first_pair_option = function a :: b :: _ -> Some (a, b) | _ -> None
let bimap_tuple func1 func2 (a, b) = (func1 a, func2 b)

let parse_stack_line line =
  let filter_func (index, _) = (index - 1) mod 4 = 0 in

  line |> String.to_seqi |> Seq.filter filter_func |> Seq.map snd |> List.of_seq

let parse_initial_stack input =
  let filter_empty =
    let char_is_space char = char != ' ' in
    let filter_inner = List.filter char_is_space in
    List.map filter_inner
  in
  let rotate_matrix (matrix : 'a list list) =
    print_stacks matrix;
    let rec outer matrix acc =
      match matrix with [] -> acc | a :: rest -> outer rest (inner a acc [])
    and inner list acc acc_done =
      match (list, acc) with
      | [], _ -> acc_done
      | a :: rest, b :: acc_rest ->
          Printf.printf "%c :: %s\n" a (String.of_seq (List.to_seq b));
          inner rest acc_rest (acc_done @ [ a :: b ])
      | a :: rest, [] -> inner rest [] (acc_done @ [ [ a ] ])
    in
    outer matrix []
  in
  input |> Str.split newline_regex |> List.rev |> drop1
  |> List.map parse_stack_line |> rotate_matrix |> filter_empty

let string_to_instruction (line : string) : instruction option =
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
  in
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
  input |> Str.split newline_regex |> List.filter_map string_to_instruction

let parse_input input =
  input |> Str.split empty_line_regex |> first_pair_option
  |> Option.map (bimap_tuple parse_initial_stack parse_instructions)

let process_order first_part (stacks : char list list) order =
  let source_stack = List.nth stacks (order.source - 1) in
  let dest_stack = List.nth stacks (order.destination - 1) in
  let crates, reduced = take_and_remainder order.amount source_stack in
  let increased =
    (if first_part then List.rev else fun x -> x) crates @ dest_stack
  in

  let update index lst =
    match index with
    | n when n = order.source - 1 -> reduced
    | n when n = order.destination - 1 -> increased
    | _ -> lst
  in

  print_stacks stacks;

  print_instruction order;

  let new_stacks = List.mapi update stacks in
  print_endline "---------------------";
  print_stacks new_stacks;
  print_endline "=====================";

  new_stacks

let process_orders first_part (stacks, orders) =
  List.fold_left (process_order first_part) stacks orders

let top_items stacks = List.map (fun l -> List.hd l) stacks

let solve first_part input =
  input |> parse_input
  |> Option.map
       (process_orders first_part >> top_items >> List.to_seq >> String.of_seq)
  |> Option.value ~default:""

let solve1 = solve true
let solve2 = solve false

let _ =
  print_endline "Day 5";
  let example_data = read_file_to_string "example05.data" in
  if example_data |> solve1 |> String.equal "CMZ" then
    print_endline "Part 1 verified"
  else failwith "Part 1 failed";

  let real_data = read_file_to_string "input05.data" in
  real_data |> solve1 |> Printf.printf "Solution 1: %s\n";

  if example_data |> solve2 |> String.equal "MCD" then
    print_endline "Part 2 verified"
  else failwith "Part 2 failed";

  real_data |> solve2 |> Printf.printf "Solution 2: %s\n"
