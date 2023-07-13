open Shared
module IntMap = Map.Make (Int)

(** Like [nth] but with swapped arguments. *)
let nth index list = List.nth list index

type monkey = {
  index : int;
  start_items : int list;
  operation : int -> int;
  test : int -> bool;
  true_dest : int;
  false_dest : int;
}

(** Return if [x] is restless divisble by [y]. *)
let is_divisible y x = x mod y = 0

let parse_monkey input =
  let parse_index line =
    line
    |> Str.split (Str.regexp "Monkey ")
    |> List.hd |> String.split_on_char ':' |> List.hd |> int_of_string
  in

  let parse_items line =
    line |> String.split_on_char ':' |> nth 1 |> String.split_on_char ','
    |> List.map (String.trim >> int_of_string)
  in

  let parse_operation line =
    line
    |> Str.split (Str.regexp ": new = ")
    |> nth 1 |> String.split_on_char ' '
    |> function
    | [ "old"; "*"; "old" ] -> fun x -> x * x
    | [ "old"; "*"; num ] -> int_of_string num |> ( * )
    | [ "old"; "+"; num ] -> int_of_string num |> ( + )
    | _ -> failwith "unknown operation"
  in

  let parse_test line =
    line
    |> Str.split (Str.regexp "divisible by ")
    |> nth 1 |> int_of_string |> is_divisible
  in

  let parse_dest line =
    line |> Str.split (Str.regexp "throw to monkey ") |> nth 1 |> int_of_string
  in

  (* Printf.printf "'%s'" input; *)
  match String.split_on_char '\n' input with
  | index :: starting :: operation :: test :: if_true :: if_false :: _ ->
      {
        index = parse_index index;
        start_items = parse_items starting;
        operation = parse_operation operation;
        test = parse_test test;
        true_dest = parse_dest if_true;
        false_dest = parse_dest if_false;
      }
  | _ -> failwith "invalid monkey"

let simulate_monkeys rounds monkeys =
  let simulate_monkey monkey items =
    (* Turn item into a tuple of (next monkey, new value) *)
    let handle_item item =
      (* Printf.printf "Handle %d\n" item; *)
      let new_value = item |> monkey.operation |> fun x -> x / 3 in
      let dest =
        if monkey.test new_value then monkey.true_dest else monkey.false_dest
      in
      (* Printf.printf "%d goes to %d\n" new_value dest; *)
      (dest, new_value)
    in

    (* Printf.printf "Handle monkey %d\n" monkey.index; *)
    let monkey_item, other_items =
      items |> List.partition (fst >> ( = ) monkey.index)
    in
    let handled_items = List.map (snd >> handle_item) monkey_item in
    (List.length handled_items, other_items @ handled_items)
  in

  let insert_or_add value index =
    IntMap.update index (function
      | Some x -> Some (x + value)
      | None -> Some value)
  in

  let rec simulate_round items inspections = function
    | [] -> (items, inspections)
    | monkey :: rest ->
        let inspected, new_items = simulate_monkey monkey items in
        simulate_round new_items
          (insert_or_add inspected monkey.index inspections)
          rest
  in

  let rec simulate_rounds index items inspections =
    Printf.printf "Round %d\n" index;
    items |> List.map Shared.Index.to_string |> print_string_list;
    if index = 0 then inspections
    else
      let new_items, added_inspections =
        simulate_round items inspections monkeys
      in
      simulate_rounds (index - 1) new_items added_inspections
  in

  let initial_items =
    monkeys
    |> List.map (fun m -> List.map (fun x -> (m.index, x)) m.start_items)
    |> List.flatten
  in

  let inspections_by_monkey =
    simulate_rounds rounds initial_items IntMap.empty
  in

  inspections_by_monkey |> IntMap.to_seq |> List.of_seq |> List.map snd
  |> List.sort Int.compare |> List.rev
  |> function
  | a :: b :: _ -> a * b
  | _ -> failwith "List to short"

let solve1 input =
  input
  |> Str.split (Str.regexp "\n\n")
  |> List.map parse_monkey |> simulate_monkeys 20

let solve2 input =
  input
  |> Str.split (Str.regexp "\n\n")
  |> List.map parse_monkey |> simulate_monkeys 10
(* kleinstes gemeinsames Vielfaches *)

let _ =
  Shared.do_day 11 solve1 solve2 read_file_to_string string_of_int 10605
    2713310158
