open Shared

type instruction = Noop | Add of int | PseudoAdd of int

let parse_instruction line =
  match String.split_on_char ' ' line with
  | [ "noop" ] -> Noop
  | [ "addx"; number ] -> Add (int_of_string number)
  | _ -> failwith "Invalid input"

let process_instructions instructions =
  let rec operate register cycle acc ins =
    Printf.printf "Cycle: %d X: %d Acc: %d\n" cycle register acc;
    let is_trigger c = (c - 20) mod 40 in
    match ins with
    | [] -> acc
    | Noop :: rest ->
        let acc_new =
          if is_trigger cycle = 0 then acc + (cycle * register) else acc
        in

        operate register (cycle + 1) acc_new rest
    | Add x :: rest ->
        let acc_new =
          if is_trigger cycle = 0 then acc + (cycle * register)
          else if is_trigger (cycle + 1) = 0 then acc + ((cycle + 1) * register)
          else acc
        in

        operate (register + x) (cycle + 2) acc_new rest
    | _ -> failwith "Not implementd for solution 1"
  in

  operate 1 1 0 instructions

let process__draw_instructions instructions =
  let rec operate register cycle ins pic =
    let rendered_pixel =
      if Int.abs (((cycle - 1) mod 40) - register) <= 1 then "#" else "."
    in
    let new_pic =
      if cycle mod 40 = 0 then pic ^ rendered_pixel ^ "\n"
      else pic ^ rendered_pixel
    in
    (* Printf.printf "Cycle: %d X: %d Pixel: %s\n" cycle register rendered_pixel; *)
    match ins with
    | [] -> new_pic
    | Noop :: rest -> operate register (cycle + 1) rest new_pic
    | Add x :: rest ->
        operate register (cycle + 1) (PseudoAdd x :: rest) new_pic
    | PseudoAdd x :: rest -> operate (register + x) (cycle + 1) rest new_pic
  in

  operate 1 1 instructions ""

let solve1 lines = lines |> List.map parse_instruction |> process_instructions

let solve2 lines =
  lines |> List.map parse_instruction |> process__draw_instructions

let _ =
  let _ =
    match
      test_and_do 10 solve1 read_file_to_string_list string_of_int 13140
    with
    | Either.Right value -> Printf.printf "Solution 1: %s\n" value
    | Either.Left s ->
        print_endline s;
        exit 1
  in
  read_file_to_string_list "example10.data" |> solve2 |> print_endline;
  read_file_to_string_list "input10.data" |> solve2 |> print_endline
