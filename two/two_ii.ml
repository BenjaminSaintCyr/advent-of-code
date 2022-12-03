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

type outcome = Win  | Draw | Lose
type move = Rock | Paper | Scissors

let game_outcome = function
  | Scissors, Rock | Rock, Paper | Paper, Scissors -> Win
  | Rock, Rock | Paper, Paper | Scissors, Scissors -> Draw
  | Rock, Scissors | Paper, Rock | Scissors, Paper -> Lose


let char_to_move = function
  | "A" -> Rock
  | "B" -> Paper
  | "C" -> Scissors

let char_to_outcome = function
  | "X" -> Lose
  | "Y" -> Draw
  | "Z" -> Win

let trick_score = function
  | _, Rock -> 1
  | _, Paper -> 2
  | _, Scissors -> 3

let moves_to_outcome = function
    | Scissors, Rock | Rock, Paper | Paper, Scissors -> Win
    | Rock, Rock | Paper, Paper | Scissors, Scissors -> Draw
    | Rock, Scissors | Paper, Rock | Scissors, Paper -> Lose

let outcome_to_score = function
  | Win -> 6
  | Draw -> 3
  | Lose -> 0

let my_move = function
  | Rock, Win | Paper, Draw | Scissors, Lose -> Paper
  | Paper, Win | Scissors, Draw | Rock, Lose -> Scissors
  | Scissors, Win | Rock, Draw | Paper, Lose -> Rock

let parseRound line =
  let char_move :: char_outcome :: _ = String.split_on_char ' ' line in  (* Only possible case*)
  let oponent_move = char_to_move char_move in
  let desired_outcome = char_to_outcome char_outcome in
  let my_move = my_move (oponent_move, desired_outcome) in
  trick_score (oponent_move, my_move) + outcome_to_score desired_outcome


let () =
  read_file filename parseRound
  |> List.fold_left (+) 0
  |> print_int
