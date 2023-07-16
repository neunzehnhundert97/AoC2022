open Shared

module Package = struct
  type t = Lst of t list | Number of int

  let is_digit c = c >= '0' && c <= '9'

  let parse line =
    let parse_int seq =
      seq |> Seq.take_while is_digit |> String.of_seq |> int_of_string
      |> fun x -> (Number x, Seq.drop_while is_digit seq)
    in
    let rec parse_list acc seq =
      match seq () with
      | Seq.Cons (']', rest) ->
          (Lst (if acc = [] then [] else List.rev acc), rest)
      | Seq.Cons ('[', rst) ->
          let num, rest = parse_list [] rst in
          parse_list (num :: acc) rest
      | Seq.Cons (',', rst) -> parse_list acc rst
      | Seq.Cons (a, rst) when is_digit a ->
          let num, rest = parse_int (Seq.cons a rst) in
          parse_list (num :: acc) rest
      | Seq.Cons (a, _) -> a |> Printf.sprintf "Illegal syntax: %c" |> failwith
      | Seq.Nil -> failwith "Input exceeded prematurely"
    in

    (* Start parsing with a list.
       Drop the first char as this is the opening brace that triggers the call. *)
    parse_list [] (line |> String.to_seq |> Seq.drop 1) |> fst

  (** Return [a] ^ [s] ^ [b]. *)
  let concat_with s a b = a ^ s ^ b

  let rec to_string = function
    | Number num -> string_of_int num
    | Lst list ->
        "[" ^ reduce (concat_with ", ") (List.map to_string list) ^ "]"

  let rec compare a b =
    match (a, b) with
    | Number a, Number b -> Int.compare a b
    | Number a, Lst b -> compare (Lst [ Number a ]) (Lst b)
    | Lst a, Number b -> compare (Lst a) (Lst [ Number b ])
    | Lst (a :: a_rst), Lst (b :: b_rst) ->
        let cmp = compare a b in
        if cmp != 0 then cmp else compare (Lst a_rst) (Lst b_rst)
    | Lst [], Lst [] -> 0
    | Lst [], Lst _ -> -1
    | Lst _, Lst [] -> 1
end

let process_indexed_item string =
  string |> String.split_on_char '\n' |> function
  | a :: b :: _ -> Package.(compare (parse a) (parse b))
  | _ -> failwith "Illegal input"

let solve1 input =
  input
  |> Str.split (Str.regexp "\n\n")
  |> List.map process_indexed_item
  |> List.mapi (fun x y -> (x + 1, y))
  |> List.filter_map (fun x -> if snd x = -1 then Some (fst x) else None)
  |> sum

let _ = do_day 13 solve1 solve1 read_file_to_string string_of_int 13 0

(* let _ = Package.("[1,2,3,[],[[1],2]]" |> parse |> to_string) |> print_endline *)
