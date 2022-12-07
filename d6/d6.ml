let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      let result = input_line chan in
      lines := result :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines (* put back in order*)

(* https://discuss.ocaml.org/t/ocaml-beginner-how-do-i-check-if-there-are-duplicate-elements-in-a-list/3377 *)
let rec exist elem lst =
  match lst with
  | [] -> false
  | hd::tl -> elem = hd || exist elem tl

let rec dupExist lst =
  match lst with
  | [] -> false
  | hd::tl -> (exist hd tl) || dupExist tl

let solve_line line =
  let rec loop i b =
    let answer = i + 1 in
    if i = String.length line - 1 then answer
    else
      let c = String.get line i in
      Queue.add c b;
      if i >= 4 then Queue.pop b |> ignore;
      if i >= 4 && Queue.to_seq b |> List.of_seq |> dupExist |> not
      then answer
      else loop (i + 1) b in
  loop 0 (Queue.create ())

let () =
  "input"
  |> read_file
  |> List.map solve_line
  |> List.fold_left (+) 0
  |> print_int; print_newline ()
