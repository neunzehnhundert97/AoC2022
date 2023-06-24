(** Read all content of a given [filename] and return a list of lines. *)
let read_file filename =
  In_channel.(with_open_text filename input_all) |> Str.split (Str.regexp "\n")

(** Print a list of strings.*)
let print_string_list list = List.iter print_string list
(** Function application with lower priority to get around parens. *)
let ( $ ) f x = f x
(** And then operator.*)
let ( >> ) f g x = g (f x)
