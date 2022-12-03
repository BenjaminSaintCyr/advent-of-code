let filename = "input"

(* utils *)
let (>>) g f x =
  g (f x)

(* find common char *)
let find_common a b c =
  let string_to_list = List.of_seq >> String.to_seq in
  let b' = string_to_list b in
  let c' = string_to_list c in
  let char_exist_in l ch = List.exists ((=) ch) l in
  string_to_list a
  |> List.filter (char_exist_in b')
  |> List.filter (char_exist_in c')
  |> List.hd

(* map char to prioty  *)
let char_to_priority c =
  let code = Char.code c in
  let is_capital = code < Char.code 'a' in
  if is_capital
  then code - Char.code 'A' + 27
  else code - Char.code 'a' + 1

(* for each bag *)
let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      let r1 = input_line chan in
      let r2 = input_line chan in
      let r3 = input_line chan in
      let result = find_common r1 r2 r3 |> char_to_priority in
      lines := result :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines (* put back in order*)

(* solve *)
let () =
  read_file filename
  |> List.fold_left (+) 0
  |> print_int
