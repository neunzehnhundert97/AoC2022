open Shared

module Index = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    let x_comp = Int.compare x1 x2 in
    if x_comp != 0 then x_comp else Int.compare y1 y2

  let init a b = List.init a (fun x -> List.init b (fun y -> (x, y)))
  let flat_init a b = init a b |> List.flatten
  let to_string (x, y) = Printf.sprintf "(%d, %d) " x y
  let print index = index |> to_string |> print_string
end

module IndexSet = Set.Make (Index)

let print_index_set set =
  IndexSet.iter Index.print set;
  print_endline ";"

let render length head tail =
  let grid = Index.init length length in
  let string =
    List.map
      (fun x ->
        string_concat
          (List.map
             (function
               | pos when pos = head -> "H"
               | pos when pos = tail -> "T"
               | _ -> "0")
             x))
      grid
  in
  List.iter print_endline (List.rev string)

type diretion = Up | Down | Left | Right

(** Parse a charcter into a [direction].
    Raise an exception if it cannot be matched *)
let direction_of_char = function
  | 'R' -> Right
  | 'L' -> Left
  | 'U' -> Up
  | 'D' -> Down
  | c -> failwith (Printf.sprintf "Illegal input '%c'" c)

(** Determine if [pos1] and [pos2] or touching,
    meaning being directly adjecent to each other. *)
let touching pos1 pos2 =
  match (pos1, pos2) with
  | (x1, y1), (x2, y2) -> Int.abs (y1 - y2) <= 1 && Int.abs (x1 - x2) <= 1

(** Given [head] and [tail], move the latter so it touches the head again. *)
let move_to_touch head tail =
  match (head, tail) with
  | (xh, yh), (xt, yt) when xh = xt ->
      if yh > yt then (xt, yt + 1) else (xt, yt - 1)
  | (xh, yh), (xt, yt) when yh = yt ->
      if xh > xt then (xt + 1, yt) else (xt - 1, yt)
  | (x1, yh), (xt, yt) ->
      ((if xt > x1 then xt - 1 else xt + 1), if yt > yh then yt - 1 else yt + 1)

let apply_direction (x, y) = function
  | Right -> (x + 1, y)
  | Left -> (x - 1, y)
  | Up -> (x, y + 1)
  | Down -> (x, y - 1)

(** Parse the input into tuple of direction and number of steps. *)
let parse_input =
  let parse_line line =
    match String.split_on_char ' ' line with
    | [ direction; step ] ->
        (direction_of_char direction.[0], int_of_string step)
    | _ -> failwith "Illegal input"
  in
  List.map parse_line

(** Simulate the movement of the rope given a list of [commands] as
    tuples of (direction, steps). *)
let simulate_rope commands =
  (* Move the head and make the fail follow. *)
  let rec do_step head tail history = function
    | _, 0 -> (head, tail, history)
    | direction, step ->
        let moved_head = apply_direction head direction in
        let moved_tail =
          if touching moved_head tail then tail
          else move_to_touch moved_head tail
        in
        (* Printf.printf "H: %s T: %s\n"
           (Index.to_string moved_head)
           (Index.to_string moved_tail); *)
        (* render 500 moved_head moved_tail; *)
        if not (touching moved_head moved_tail) then failwith "!";
        do_step moved_head moved_tail (moved_tail :: history)
          (direction, step - 1)
  in

  (* Process each command at at time.
      [head] is a tuple of the head's current position.
      [tail] is a tuple of the head's current position.
      [tail] is a list of tuples of the head's past positions.
      The [history] is returned at the end. *)
  let rec simulate head tail history = function
    | [] -> List.flatten history
    | command :: rest ->
        let moved_head, moved_tail, hist = do_step head tail [] command in
        simulate moved_head moved_tail (hist :: history) rest
  in

  (* Printf.printf "H: %s T: %s\n"
     (Index.to_string (0, 0))
     (Index.to_string (0, 0)); *)
  commands
  |> simulate (0, 0) (0, 0) [ [ (0, 0) ] ]
  (* |> tap Index.print *)
  |> IndexSet.of_list
  |> inspect print_index_set |> IndexSet.cardinal

let solve1 lines = lines |> parse_input |> simulate_rope
let solve2 lines = List.length lines
let _ = do_day 09 solve1 solve2 read_file_to_string_list string_of_int 13 0

(* let _ =
   let grid = Index.flat_init 5 5 in
   let head = (2, 2) in

   grid
   |> List.map (fun tail ->
          (tail, if touching tail head then tail else move_to_touch head tail))
   |> List.iter (fun (old_t, new_t) ->
          print_endline "-------------";
          render 5 head old_t;
          print_endline "\nBecomes\n";
          render 5 head new_t);

   print_endline "" *)
