open Shared
module IndexSet = Set.Make (Index)

let parse line =
  line
  |> Str.split (Str.regexp " -> ")
  |> List.map
       (String.split_on_char ',' >> function
        | [ x; y ] -> (int_of_string x, int_of_string y)
        | _ -> failwith "Illegal input")

let line_to_indexes line =
  let rec inner points = function
    | p1 :: p2 :: rest ->
        inner (Index.points_in_range p1 p2 @ points) (p2 :: rest)
    | [ _ ] -> points
    | _ -> failwith "Illegal input"
  in
  line |> inner [] |> IndexSet.of_list

let simulate_sand floored rocks =
  let max =
    rocks |> IndexSet.to_seq |> Seq.map snd |> List.of_seq
    |> List.sort Int.compare |> List.rev |> List.hd
  in

  let floor_level = max + 2 in

  let contains point set =
    if floored && snd point = floor_level then true else IndexSet.mem point set
  in

  let rec make_sand_fall blocked (x, y) =
    if (not floored) && y > max then None
    else
      let below = (x, y + 1) in
      let left = (x - 1, y + 1) in
      let right = (x + 1, y + 1) in
      if contains below blocked then
        if contains left blocked then
          if contains right blocked then Some (x, y)
          else make_sand_fall blocked right
        else make_sand_fall blocked left
      else make_sand_fall blocked below
  in

  let start = (500, 0) in

  let rec simulate sand_set =
    match make_sand_fall sand_set start with
    | Some pos ->
        let new_set = IndexSet.add pos sand_set in
        if IndexSet.mem start new_set then new_set else simulate new_set
    | None -> sand_set
  in

  let num_rocks = IndexSet.cardinal rocks in
  IndexSet.cardinal (simulate rocks) - num_rocks

let solve1 lines =
  lines
  |> List.map (parse >> line_to_indexes)
  |> reduce IndexSet.union |> simulate_sand false

let solve2 lines =
  lines
  |> List.map (parse >> line_to_indexes)
  |> reduce IndexSet.union |> simulate_sand true

let _ = do_day 14 solve1 solve2 read_file_to_string_list string_of_int 24 93
