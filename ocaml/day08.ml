open Shared

module Index = struct
  type t = int * int

  let compare t1 t2 =
    match (t1, t2) with
    | (x1, y1), (x2, y2) when y1 = y2 && x1 = x2 -> 0
    | (x1, _), (x2, _) when x1 != x2 -> Int.compare x1 x2
    | (_, y1), (_, y2) -> Int.compare y1 y2

  let init a b = List.init a (fun x -> List.init b (fun y -> (x, y)))
  let flat_init a b = init a b |> List.flatten
  let to_string (x, y) = Printf.sprintf "(%d, %d) " x y
  let print index = index |> to_string |> print_string
end

module IndexSet = Set.Make (Index)
module IndexMap = Map.Make (Index)

let print_index_set set =
  IndexSet.iter Index.print set;
  print_endline ";"

let mapmap func = List.map (List.map func)

let print_row r =
  List.iter (fun ((x, y), e) -> Printf.printf "[%d, %d]=%d " x y e) r;
  print_endline ""

let print_grid = List.iter print_row

let create_grid lines =
  let char_to_int char = Char.code char - Char.code '0' in

  let index_grid grid =
    List.mapi (fun x elem -> List.mapi (fun y e -> ((x, y), e)) elem) grid
  in
  let by_rows =
    lines
    |> List.map (String.to_seq >> List.of_seq)
    |> mapmap char_to_int |> index_grid
  in

  let rec transpose res grid =
    let heads, rests =
      List.split
        (List.map (function x :: xs -> (Some x, xs) | _ -> (None, [])) grid)
    in
    if List.hd heads = None then List.rev res
    else transpose (List.filter_map (fun x -> x) heads :: res) rests
  in

  let by_columns = transpose [] by_rows in

  (* print_grid by_rows;
     print_endline "------";
     print_grid by_columns; *)
  (by_rows, by_columns)

let scan_grid_for_visibility (rows, columns) =
  let scan =
    let rec inner res max = function
      | [] -> IndexSet.of_list res
      | ((x, y), e) :: rest when e > max -> inner ((x, y) :: res) e rest
      | _ :: rest -> inner res max rest
    in
    inner [] (-1)
  in

  let scan_both_sides =
    List.map (fun l -> [ scan l; l |> List.rev |> scan ]) >> List.flatten
  in

  let union_all = List.fold_left IndexSet.union IndexSet.empty in

  let row_result = rows |> scan_both_sides |> union_all in
  let column_result = columns |> scan_both_sides |> union_all in

  let combined_result = IndexSet.union row_result column_result in

  IndexSet.cardinal combined_result

let scan_grid_for_scenery (rows, columns) =
  let scan_list list index =
    let section =
      list |> List.filter (List.exists (fst >> ( = ) index)) |> List.hd
    in

    let look_from_position index list =
      let rec find_pos index position counter list =
        match list with
        | (pos, height) :: rest when index = pos ->
            count_trees index height counter rest
        | _ :: rest -> find_pos index position counter rest
        | [] -> failwith "Index not found"
      and count_trees index position counter list =
        match list with
        | (_, h) :: rest when h < position ->
            count_trees index position (counter + 1) rest
        | _ :: _ -> counter + 1
        | [] -> counter
      in
      find_pos index 0 0 list
    in

    let res1 = look_from_position index section in
    let res2 = look_from_position index (List.rev section) in
    (* Printf.printf "For %s:\n" (Index.to_string (x, y));
       print_row section;
       Printf.printf "%d visible trees\n\n" res1;
       print_row (List.rev section);
       Printf.printf "%d visible trees\n\n" res2; *)
    res1 * res2
  in

  let scan_all index = scan_list rows index * scan_list columns index in

  let all_indexes = Index.flat_init (List.length rows) (List.length columns) in
  all_indexes |> List.map scan_all |> List.sort Int.compare |> List.rev
  |> List.hd

let solve1 input = input |> create_grid |> scan_grid_for_visibility
let solve2 input = input |> create_grid |> scan_grid_for_scenery
let _ = do_day 8 solve1 solve2 read_file_to_string_list string_of_int 21 8
