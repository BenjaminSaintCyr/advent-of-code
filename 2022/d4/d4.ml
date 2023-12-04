(* utils *)
let compose f g x = f (g x)

let pipe f g x = g (f x)

let ( << ) = compose

let ( >> ) = pipe

let read_file filename parser =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      let result = input_line chan |> parser in
      lines := result :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines (* put back in order*)

exception BadInput

(* solution *)
let filename = "input"

let parse_input =
  let list_to_pair = function min :: max :: [] -> (min, max) | _ -> raise BadInput in
  let parse_range = String.split_on_char '-' >> List.map int_of_string >> list_to_pair in 
  String.split_on_char ',' >> List.map parse_range >> list_to_pair

let is_fully_in = function
  | ((min1, max1), (min2, max2)) ->
    let one_in_two = min2 <= min1 && max1 <= max2 in
    let two_in_one = min1 <= min2 && max2 <= max1 in
    if one_in_two || two_in_one then 1
    else 0
  | _ -> raise BadInput

let () =
  read_file filename (parse_input >> is_fully_in)
  |> List.fold_left (+) 0
  |> print_int
