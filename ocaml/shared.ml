(** Read all content of a given [filename] and return a list of lines. *)
let read_file_to_string_list filename =
  In_channel.(with_open_text filename input_all) |> Str.split (Str.regexp "\n")

let read_file_to_string filename =
  In_channel.(with_open_text filename input_all)

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

(** Print a list of strings.*)
let print_string_list list = List.iter print_string list

(** Function application with lower priority to get around parens. *)
let ( $ ) f x = f x

(** And then operator.*)
let ( >> ) f g x = g (f x)
