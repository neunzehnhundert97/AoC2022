open Shared

let parse line =
  line
  |> Str.split (Str.regexp "[xy]=\\|[:,]")
  |> List.filter_map int_of_string_opt
  |> function
  | [ a; b; c; d ] -> ((a, b), (c, d))
  | _ -> failwith "Illegal input"

(* let _ = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15" |> parse *)

(** Given a list of index pairs, get the minimum and maximum of both
    coordinates. *)
let dimensions pair_list =
  let rec inner min_x min_y max_x max_y = function
    | ((x1, y1), (x2, y2)) :: rest ->
        inner
          (min_x |> min x1 |> min x2)
          (min_y |> min y1 |> min y2)
          (max_x |> max x1 |> max x2)
          (max_y |> max y1 |> max y2)
          rest
    | [] -> ((min_x, min_y), (max_x, max_y))
  in

  inner 0 0 0 0 pair_list

let count_covered_in_line line_num count_beacon max_value pair_list =
  let scanner_and_distance, beacons =
    List.map (fun (x, y) -> ((x, Index.manhattan_distance x y), y)) pair_list
    |> List.split
  in

  let max_distance = scanner_and_distance |> List.map snd |> list_max in

  let is_beacon point = List.exists (( = ) point) beacons in

  let scan_line =
    (match max_value with
    | None ->
        let (x_min, _), (x_max, _) = dimensions pair_list in
        List.init
          (x_max - x_min + 1 + (2 * max_distance))
          (fun x -> (x + x_min - max_distance, line_num))
    | Some max_value -> List.init (max_value + 1) (fun x -> (x, line_num)))
    |> if count_beacon then List.filter (is_beacon >> not) else identity
  in

  (* Only use beacons which can reach the current line. *)
  let filtered_sensors =
    List.filter
      (fun ((_, y), distance) -> Int.abs (y - line_num) <= distance)
      scanner_and_distance
  in

  let in_range point =
    List.exists
      (fun (sensor, dis) -> Index.manhattan_distance point sensor <= dis)
      scanner_and_distance
  in

  scanner_and_distance |> List.map snd |> List.iter (Printf.printf "%d\n");

  scan_line |> List.filter in_range

let successor (x, y) dim_max = if y = dim_max then (x + 1, 0) else (x, y + 1)

(* Way to slow. *)
(* let scan_all_lines max_value pair_list =
   let rec find_line line_index =
     Printf.printf "%d\n" line_index;
     print_newline ();
     if line_index >= max_value then failwith "Nothing was found"
     else
       let line_scan =
         count_covered_in_line line_index false (Some max_value) pair_list
       in
       if List.length line_scan != max_value + 1 then
         let rec find_missing pred = function
           | succ :: rest ->
               if successor pred max_value = succ then find_missing succ rest
               else successor pred max_value
           | [] -> failwith "Sequence exceeded"
         in
         let x, y = find_missing (0, -1) line_scan in
         (x * 4000000) + y
       else find_line (line_index + 1)
   in

   find_line 0 *)

(* Only works for the small sample due to RAM restrictions. *)
(* let remaining_in_square dim_max pair_list =
   let scanner_and_distance =
     List.map (fun (x, y) -> (x, Index.manhattan_distance x y)) pair_list
   in

   let rec points_in_distance points = function
     | (_, _, -1) :: rest -> points_in_distance points rest
     | ((x, y), _, _) :: rest when x < 0 || y < 0 || x > dim_max || y > dim_max
       ->
         points_in_distance points rest
     | ((x, y), parent, distance) :: rest ->
         let new_points =
           [ (x + 1, y); (x, y + 1); (x - 1, y); (x, y - 1) ]
           |> List.filter (( != ) parent)
           |> List.map (fun p -> (p, (x, y), distance - 1))
         in
         points_in_distance (IndexSet.add (x, y) points) (new_points @ rest)
     | [] -> points
   in

   let blocked =
     scanner_and_distance
     |> List.map (fun ((x, y), distance) ->
            points_in_distance IndexSet.empty
              [ ((x, y), (x + 2, y + 2), distance) ])
     |> reduce IndexSet.union
   in

   let rec find_missing pred sequence =
     match sequence () with
     | Seq.Cons (succ, rest) ->
         if successor pred = succ then find_missing succ rest else successor pred
     | Seq.Nil -> failwith "Sequence exceeded"
   in

   let found_x, found_y = blocked |> IndexSet.to_seq |> find_missing (0, -1) in
   Index.print (found_x, found_y);
   (4000000 * found_x) + found_y *)

let rim_of_range (x, y) range =
  let rec inner acc point =
    Index.diag_neighbours point
    |> List.find_opt (fun p ->
           Index.manhattan_distance p (x, y) = range && not (IndexSet.mem p acc))
    |> function
    | None -> IndexSet.add point acc
    | Some next -> inner (IndexSet.add point acc) next
  in

  let first_point = (x + range, y) in

  inner (IndexSet.of_list [ first_point ]) first_point

(* let _ = rim_of_range (0, 0) 4 |> IndexSet.iter Index.print *)

let solve_by_rim max_value pair_list =
  let scanner_and_distance =
    List.map (fun (x, y) -> (x, Index.manhattan_distance x y)) pair_list
  in

  let one_over_rims =
    scanner_and_distance
    |> List.map (fun (sensor, distance) ->
           rim_of_range sensor (distance + 1)
           |> IndexSet.filter (fun (x, y) ->
                  x > 0 && y > 0 && x <= max_value && y <= max_value))
    |> reduce IndexSet.union |> IndexSet.to_seq
  in

  let x, y =
    Seq.find
      (fun point ->
        List.for_all
          (fun (sensor, distance) ->
            Index.manhattan_distance point sensor > distance)
          scanner_and_distance)
      one_over_rims
    |> Option.get
  in
  (x * 4000000) + y

let solve1 index lines =
  lines |> List.map parse
  |> count_covered_in_line index true None
  |> List.length

let solve2 dim lines = lines |> List.map parse |> solve_by_rim dim

(* let solve2 line = List.length line *)
let _ =
  let example_data = read_file_to_string_list "example15.data" in
  let data = read_file_to_string_list "input15.data" in
  (* example_data |> solve1 10 |> Printf.printf "Test result: %d\n";
     data |> solve1 2000000 |> Printf.printf "Real result: %d\n"; *)
  example_data |> solve2 20 |> Printf.printf "Test result: %d\n";
  data |> solve2 4000000 |> Printf.printf "Real result: %d\n"
