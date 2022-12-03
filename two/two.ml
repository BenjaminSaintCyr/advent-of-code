let filename = "input"

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

type move = Rock | Paper | Scissors

let char_to_move = function
  | "A" | "X" -> Rock
  | "B" | "Y" -> Paper
  | "C" | "Z" -> Scissors

let trick_score = function
  | _, Rock -> 1
  | _, Paper -> 2
  | _, Scissors -> 3

let round_score = function
    | Scissors, Rock | Rock, Paper | Paper, Scissors -> 6
    | Rock, Rock | Paper, Paper | Scissors, Scissors -> 3
    | Rock, Scissors | Paper, Rock | Scissors, Paper -> 0

let parseRound line =
  let a :: b :: _ = String.split_on_char ' ' line |> List.map char_to_move in  (* Only possible case*)
  trick_score (a, b) + round_score (a, b)


let () =
  read_file filename parseRound
  |> List.fold_left (+) 0
  |> print_int
