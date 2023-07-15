open Shared
module IndexMap = Map.Make (Index)

(** Transform the [input] string into a 2-dimesional array of chars. *)
let parse_input input =
  input
  |> List.map (String.to_seq >> Seq.filter (( != ) '\n') >> Array.of_seq)
  |> Array.of_list

let transform_to_height = function
  | 'S' -> 0
  | 'E' -> 25
  | c -> int_of_char c - int_of_char 'a'

(** Return the dimesions of a 2-dimensional array, assuming it is not empty. *)
let array_dims array = (Array.length array, Array.length array.(0))

(** Searches for the start and stop marker and returns two index tuples of
    ([start], [stop]), or raises an exception of they cannot be found. *)
let find_start_and_stop (array : char array array) =
  let find (x, y) =
    match array.(x).(y) with
    | 'S' -> Some ('S', x, y)
    | 'E' -> Some ('E', x, y)
    | _ -> None
  in

  let x, y = array_dims array in

  Shared.Index.flat_init x y |> List.filter_map find |> function
  | ('S', x1, y1) :: ('E', x2, y2) :: _ -> ((x1, y1), (x2, y2))
  | ('E', x1, y1) :: ('S', x2, y2) :: _ -> ((x2, y2), (x1, y1))
  | l -> failwith (Printf.sprintf "Illegal result %d" (List.length l))

let dijkstra array =
  let x_dim, y_dim = array_dims array in
  let start, stop = find_start_and_stop array in
  Printf.printf "Start: %s Stop: %s in %dx%d\n" (Index.to_string start)
    (Index.to_string stop) x_dim y_dim;

  let valid_neighbours (x, y) =
    let height = array.(x).(y) |> transform_to_height in
    (x, y) |> Index.neighbours
    |> List.filter (fun (x, y) ->
           x >= 0 && y >= 0 && x < x_dim && y < y_dim
           && transform_to_height array.(x).(y) - 1 <= height)
  in

  let rec traverse result_map = function
    | (visited, index) :: rest -> (
        (* Printf.printf "Position %s Visit: %d Value:%d \n"
           (Index.to_string index) visited
           (IndexMap.find_opt index result_map |> Option.value ~default:0); *)
        (* if List.length rest mod 4 = 0 then
           Printf.printf "Coverage %f\n"
             (float_of_int (IndexMap.cardinal result_map)
             /. (float_of_int x_dim *. float_of_int y_dim)
             *. 100.); *)
        match IndexMap.find_opt index result_map with
        | Some value when value <= visited -> traverse result_map rest
        | _ ->
            let adjecent =
              index |> valid_neighbours |> List.map (fun x -> (visited + 1, x))
            in
            traverse (IndexMap.add index visited result_map) (adjecent @ rest))
    | [] -> result_map
  in

  let result = traverse IndexMap.empty [ (0, start) ] in

  IndexMap.iter
    (fun x y -> Printf.printf "%s -> %d\n" (Index.to_string x) y)
    result;

  IndexMap.find stop result

let solve1 input = input |> parse_input |> dijkstra
let solve2 input = List.length input
let _ = do_day 12 solve1 solve2 read_file_to_string_list string_of_int 31 0
