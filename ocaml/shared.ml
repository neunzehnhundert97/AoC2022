(** Read all content of a given [filename]. *)
let read_file filename =
  In_channel.(with_open_text filename input_all) |> Str.split (Str.regexp "\n")

let print_string_list list = List.iter print_string list
let ( $ ) f x = f x
let ( >> ) f g x = g (f x)
