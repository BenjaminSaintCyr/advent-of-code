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

(* solution *)
let filename = "input"

let parse_input =
  let list_to_pair = function min :: max :: [] -> (min, max) in
  let parse_range = String.split_on_char '-' >> List.map int_of_string >> list_to_pair in 
  String.split_on_char ',' >> List.map parse_range >> list_to_pair

let is_fully_in range1 range2 =
  let is_in_range n (min, max) = n >= min && n <= max in
  let is_overlap (min, max) range =  is_in_range min range || max range in
  if is_overlap range1 range2 || is_overlap range2 range1 then 1 else 0

let () =
  read_file filename (parse_input >> is_fully_in)
  |> List.fold_left (+) 0
  |> print_int
