type command =
  | ChangeDir of string
  | ListDir
  | FileSize of int * string
  | DirEntry of string

type tree =
  | Directory of string list * tree list
  | File of string * int
  | Empty

let size_of_tree tree =
  let rec inner = function
    | File (_, size) -> (size, [ (None, size) ])
    | Empty -> (0, [ (None, 0) ])
    | Directory (name, contents) ->
        let sizes, info = contents |> List.map inner |> List.split in
        let size = Shared.sum sizes in
        (size, (Some (List.hd name), size) :: List.flatten info)
  in

  let _, info = inner tree in
  info
  |> List.filter_map (function
       | Some name, size -> Some (name, size)
       | None, _ -> None)

let tree_to_string =
  let indent x = List.fold_left ( ^ ) "" (List.init x (fun _ -> "\t")) in
  let rec inner depth = function
    | Empty -> Printf.sprintf "%s(Nothing)\n" (indent depth)
    | File (name, size) ->
        Printf.sprintf "%s%s (%d bytes)\n" (indent depth) name size
    | Directory (name, content) ->
        let display_name = name |> List.rev |> Shared.string_concat in
        let body =
          content |> List.map (inner (depth + 1)) |> Shared.string_concat
        in
        Printf.sprintf "%s%s\n%s\n" (indent depth) display_name body
  in
  inner 0

let command_to_string = function
  | ChangeDir dir -> Printf.sprintf "Change to '%s'" dir
  | ListDir -> "List current dir"
  | DirEntry dir -> Printf.sprintf "Directory '%s'" dir
  | FileSize (size, filename) ->
      Printf.sprintf "File '%s' has size '%d'" filename size

let print_command = Shared.(command_to_string >> print_string)

let parse_command = function
  | s when String.starts_with ~prefix:"$ cd" s ->
      ChangeDir (String.sub s 5 (String.length s - 5))
  | s when String.starts_with ~prefix:"$ ls" s -> ListDir
  | s when String.starts_with ~prefix:"dir " s ->
      DirEntry (String.sub s 4 (String.length s - 4))
  | s -> (
      s |> Str.split (Str.regexp " ") |> function
      | a :: b :: _ -> FileSize (int_of_string a, b)
      | _ -> failwith "Syntax error")

let add_path paths segment =
  match (paths, segment) with
  | [], seg -> [ seg ]
  | _ :: [ "/" ], ".." -> [ "/" ]
  | _ :: "/" :: rest, ".." -> rest
  | [ "/" ], seg -> seg :: [ "/" ]
  | ls, seg -> seg :: "/" :: ls

let process_commands commands =
  let rec pre_group dir contents res commands =
    let append = add_path dir in
    match commands with
    | [] -> List.rev ((dir, contents) :: res)
    | ListDir :: rest -> pre_group dir contents res rest
    | ChangeDir name :: rest when List.length contents != 0 ->
        pre_group (append name) [] ((dir, contents) :: res) rest
    | ChangeDir name :: rest -> pre_group (append name) contents res rest
    | FileSize (size, name) :: rest ->
        pre_group dir (FileSize (size, name) :: contents) res rest
    | DirEntry name :: rest ->
        pre_group dir (DirEntry name :: contents) res rest
  in

  let cmd_to_entry = function
    | FileSize (size, name) -> File (name, size)
    | DirEntry name -> Directory ([ name ], [])
    | _ -> failwith "Illegal input"
  in

  let make_shallow_tree (dir, contents) =
    Directory (dir, List.map cmd_to_entry contents)
  in

  let fold_tree res trees =
    let find_dir name =
      trees
      |> List.find_map (function
           | Directory (n, c) when n = name -> Some c
           (* | Directory (n, _) when n != name ->
               Printf.printf "%s != %s\n"
                 (Shared.string_concat name)
                 (Shared.string_concat n);
               None *)
           | _ -> None)
      |> Option.get
    in

    let rec replace_stubs path = function
      | Directory (name, []) ->
          let full_dir = add_path path (List.hd name) in
          (Directory (name, find_dir full_dir), true)
      | Directory (name, content) ->
          let replaced, changeds =
            List.split
              (List.map (replace_stubs (add_path path (List.hd name))) content)
          in
          (Directory (name, replaced), List.exists (fun x -> x) changeds)
      | a -> (a, false)
    in

    (* trees |> List.map tree_to_string |> List.iter print_endline; *)
    let rec replace_all tree =
      let new_tree, changed = replace_stubs [] tree in
      if changed then replace_all new_tree else new_tree
    in

    replace_all res
  in

  let pre_tree = commands |> pre_group [] [] [] |> List.map make_shallow_tree in

  fold_tree (List.hd pre_tree) (List.tl pre_tree)

let print_tuple (x, y) = Printf.printf "%s: %d\n" x y

let solve1 input =
  let tree = input |> List.map parse_command |> process_commands in
  let dir_sizes =
    tree |> size_of_tree |> List.sort (fun (_, s1) (_, s2) -> Int.compare s1 s2)
  in

  (* tree |> tree_to_string |> print_endline; *)
  dir_sizes
  |> List.filter (fun (_, s) -> s <= 100000)
  (* |> Shared.tap print_tuple *)
  |> List.map snd
  |> Shared.sum

let solve2 input = List.length input

(* let _ =
   let i = File ("i", 584) in
   let f = File ("f", 29116) in
   let g = File ("g", 2557) in
   let h = File ("h", 62596) in
   let e = Directory ([ "e" ], [ i ]) in
   let a = Directory ([ "a" ], [ e; f; g; h ]) in

   a |> tree_to_string |> print_endline;
   a |> size_of_tree |> List.iter print_tuple *)

let _ =
  Shared.do_day 7 solve1 solve2 Shared.read_file_to_string_list string_of_int
    95437 1
