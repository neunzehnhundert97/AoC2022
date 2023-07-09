(** Read the file of the given [filename] and return its contents as string. *)
let read_file_to_string filename =
  In_channel.(with_open_text filename input_all)

(** Read all content of a given [filename] and return a list of lines,
    excluding the newline characters. *)
let read_file_to_string_list filename =
  read_file_to_string filename |> Str.split (Str.regexp "\n")

(** Drop the first argument of a [list] and return the remainder.
    For an empty list, an empty list is returned. *)
let drop1 = function _ :: rest -> rest | _ -> []

(** Take the first [n] elemts of [list].
    For an empty list, an empty list is returned. 
    For a negative number, an empty list is returned. *)
let take_and_remainder n (list : 'a list) =
  let rec inner (list : 'a list) n acc =
    match (n, list) with
    | 0, list -> (List.rev acc, list)
    | _, [] -> (List.rev acc, [])
    | n, x :: xs -> inner xs (n - 1) (x :: acc)
  in
  if n > 0 then inner list n [] else ([], list)

let take n list = fst (take_and_remainder n list)
let sum = List.fold_left ( + ) 0
let string_concat = List.fold_left ( ^ ) ""

(** Apply the side effect [func] to each element of [list] and return it
    unchanged. *)
let tap func =
  List.map (fun x ->
      func x;
      x)

let inspect func x =
  func x;
  x

let test_and_do day solve read to_string test_value =
  let example_file = Printf.sprintf "example%02d.data" day in
  let data_file = Printf.sprintf "input%02d.data" day in
  let example_data = read example_file in
  let data = read data_file in
  let example_result = solve example_data in
  if example_result != test_value then
    Either.left
      (Printf.sprintf "Test failed, expected %s, but got %s"
         (to_string test_value) (to_string example_result))
  else (
    Printf.printf "Test succeeded\n";
    data |> solve |> to_string |> Either.right)

(** Main function to run a day of the advent. Given the day. *)
let do_day day solve1 solve2 read to_string test1 test2 =
  Printf.printf "Day %d\n" day;
  let _ =
    match test_and_do day solve1 read to_string test1 with
    | Either.Right value -> Printf.printf "Solution 1: %s\n" value
    | Either.Left s ->
        print_endline s;
        exit 1
  in
  match test_and_do day solve2 read to_string test2 with
  | Either.Right value -> Printf.printf "Solution 2: %s\n" value
  | Either.Left s ->
      print_endline s;
      exit 1

(** Print a list of strings.*)
let print_string_list list = List.iter print_string list

(** Function application with lower priority to get around parens. *)
let ( $ ) f x = f x

(** And then operator.*)
let ( >> ) f g x = g (f x)

(** Returns the identity of [x], which is x. *)
let identity x = x

let const x _ = x

let rec last = function
  | [ x ] -> x
  | _ :: xs -> last xs
  | _ -> failwith "Empty list"
