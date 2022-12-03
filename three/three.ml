(* for each bag *)
let filename = "input"

(* utils *)
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

let (>>) g f x =
  g (f x)

(* find common char *)
let split_in_half a =
  let length = String.length a in
  let half = length / 2 in (* assume it's pair  *)
  let cutter = String.sub a in
  (cutter 0 half, cutter half half)

let find_common_char a b =
  let string_to_list = List.of_seq >> String.to_seq in
  let b' = string_to_list b in
  let find_char_in_b c = List.find_opt ((=) c) b' in
  string_to_list a
  |> List.map find_char_in_b
  |> List.filter Option.is_some
  |> List.hd
  |> Option.get

(* map char to prioty  *)
let char_to_priority c =
  let code = Char.code c in
  let is_capital = code < Char.code 'a' in
  if is_capital
  then code - Char.code 'A' + 27
  else code - Char.code 'a' + 1

(* solve *)
let () =
  let rucksack_priority x =
    let a, b = split_in_half x in
    find_common_char a b
    |> char_to_priority in
  read_file filename rucksack_priority
  |> List.fold_left (+) 0
  |> print_int
