(* utils *)
let pipe f g x = g (f x)
let ( >> ) = pipe

(* types *)
type crate = char
type stack = crate list
type dock = stack array

type instruction = int * int * int

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

let split_list (l: string list): string list * string list =
  let rec loop acc = function
    | "" :: rest -> acc, rest
    | x :: rest -> loop (x :: acc) rest  in
  loop [] l


let parse_instruction =
  String.split_on_char ' '
  >> function
  | _ :: nb :: _ :: from_stack :: _ :: to_stack :: [] ->
    int_of_string nb, int_of_string from_stack, int_of_string to_stack

(* Parse initial state *)
let nb_stacks = 9

let parse_stacks s =
  let s = List.tl s in (* drop num line *)
  let dock: dock = Array.make nb_stacks [] in
  let parse_line line = 
    for i = 0 to nb_stacks - 1 do
      let pos = 1 + i * 4 in
      let content = String.get line pos in
      match content with
      | ' ' -> ()
      | x -> dock.(i) <- x :: dock.(i)
      done in
  List.iter parse_line s; dock

(* Parse instruction *)
(* s, instruction -> state *)
let perform (state: dock) (instruction: instruction): dock =
  let qt, from_stack, to_stack = instruction in
  for _ = 1 to qt do
    (* deque *)
    let crate = List.hd state.(from_stack - 1) in
    state.(to_stack - 1) <- crate :: state.(to_stack - 1);
    state.(from_stack - 1) <- List.tl state.(from_stack - 1);
  done; state

let rec last = function
  | [] -> raise Not_found
  | [x] -> x
  | _ :: tl -> last tl

let file_name = "input"

let () =
  let stacks, instructions = read_file file_name |> split_list in
  let instructions' = List.map parse_instruction instructions in
  let stacks' = parse_stacks stacks in
  List.fold_left perform stacks' instructions'
  |> Array.map List.hd
  |> (Array.to_seq >> String.of_seq)
  |> print_string; print_newline ()

(*CFFHVVHNC*)
