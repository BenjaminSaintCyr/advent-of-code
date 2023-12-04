let pipe f g x = g (f x)
let ( >> ) = pipe

type move =
  | Up of int
  | Down of int
  | Right of int
  | Left of int

type instructions = move list

type pos = int * int

type rel_pos =
  | Oblique
  | Diag
  | Perp

type state = pos * pos * int array

let read_file filename lexer =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      let result = input_line chan |> lexer in
      lines := result :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines (* put back in order*)


let lexer line =
  String.split_on_char ' ' line
  |> function
  | "U" :: move :: [] -> (Up (int_of_string move))
  | "D" :: move :: [] -> (Down (int_of_string move))
  | "R" :: move :: [] -> (Right (int_of_string move))
  | "L" :: move :: [] -> (Left (int_of_string move))

let get_nb_moves = function
  | Up move -> move
  | Down move -> move
  | Right move -> move
  | Left move -> move

let apply_move (x, y) = function
  | Up _ -> x + 1, y
  | Down _ -> x - 1, y
  | Right _ -> x, y + 1
  | Left _ -> x, y - 1

let pull (x, y) (i, j) = (* TODO fix *)
  let v, h = x - i, y - j in
  if Int.abs h <= 1 && Int.abs v <= 1 then (i, j) (* still *)
  else if v == 0 || h == 0 then i + v, j + h (* straight*)
  else i + (v / (Int.abs v)), j + (h / Int.abs v) (* diag move *)

type rope = pos * pos

let rec apply_cmd state rope cmd = function
  | 0 -> rope
  | i ->
    let hd, tl = rope in
    let hd = apply_move hd cmd in
    let x, y = pull hd tl in
    state.(x).(y) <- 1;
    apply_cmd state (hd, (x, y)) cmd (i - 1)

let rec simulate rope state = function
  | [] -> state
  | instruction :: instructions ->
    let rope = apply_cmd state rope instruction (get_nb_moves instruction) in
    simulate rope state instructions

let apply instructions =
  simulate ((0,0), (0,0)) (Array.make_matrix 20 20 0) instructions

let sum2d =
  let sum = Array.fold_left (+) 0 in
  Array.map sum >> sum

let () =
  read_file "test" lexer
  |> apply
  |> sum2d
