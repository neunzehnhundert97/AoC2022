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
let print_string_list list =
  print_string " [ ";
  List.iter (fun x -> Printf.printf "'%s', " x) list;
  print_endline " ]"

(** Function application with lower priority to get around parens. *)
let ( $ ) f x = f x

(** And then operator.*)
let ( >> ) f g x = g (f x)

(** Returns the identity of [x], which is x. *)
let identity x = x

let const x _ = x

(** Return the last element of an [list].
    When the list is empty, raise an exeption. *)
let rec last = function
  | [ x ] -> x
  | _ :: xs -> last xs
  | _ -> failwith "Empty list"

(** For a [func] with two argument, swap the position of the arguments. *)
let swap2 func x y = func y x

let ( -..> ) = swap2

(** Like [fold_left], but taking the first element as initial value.
    Raises an exception if the list is empty. *)
let reduce func list = List.fold_left func (List.hd list) (List.tl list)

let tuple f (x, y) = f x y

let list_max list =
  let rec inner acc = function x :: xs -> inner (max x acc) xs | [] -> acc in
  inner (List.hd list) (List.tl list)

(** Module for an index, which is a two dimensional point. *)
module Index = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    let x_comp = Int.compare x1 x2 in
    if x_comp != 0 then x_comp else Int.compare y1 y2

  let init a b = List.init a (fun x -> List.init b (fun y -> (x, y)))
  let flat_init a b = init a b |> List.flatten
  let to_string (x, y) = Printf.sprintf "(%d, %d) " x y
  let print index = index |> to_string |> print_string
  let neighbours (x, y) = [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]

  let diag_neighbours (x, y) =
    [ (x + 1, y + 1); (x - 1, y - 1); (x - 1, y + 1); (x + 1, y - 1) ]

  let points_in_range a b =
    match (a, b) with
    | (x1, y1), (x2, y2) when x1 = x2 ->
        let small_y, big_y = (min y1 y2, max y1 y2) in
        List.init (big_y - small_y + 1) (fun y -> (x1, y + small_y))
    | (x1, y1), (x2, y2) when y1 = y2 ->
        let small_x, big_x = (min x1 x2, max x1 x2) in
        List.init (big_x - small_x + 1) (fun x -> (x + small_x, y1))
    | _ -> failwith "Points must be on a straight line"

  let manhattan_distance (x1, y1) (x2, y2) =
    Int.abs (x1 - x2) + Int.abs (y1 - y2)
end

module IndexSet = Set.Make (Index)
