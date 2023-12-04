let tee x f =
  f x; x

let ( |>! ) = tee

type commands =
  | Cd of string
  | Ls
  | FileDescription of int
  | DirDescription of string

exception UnkownToken

let lexer line =
  String.split_on_char ' ' line
  |> function
  | "$" :: "cd" :: dir :: [] ->
    Cd dir
  | "$" :: "ls" :: [] ->
    Ls
  | "dir" :: dir :: [] ->
    DirDescription dir
  | size :: _name :: [] ->
    FileDescription (int_of_string size)
  | _ -> raise UnkownToken

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

type file_sys =
  | File of int
  | Dir of string * file_sys list

exception NotADir

let cons a b =
  match b with
  | Dir (name, b) -> Dir (name, (a :: b))
  | File _ -> raise NotADir

let dir_name = function
  | Dir (name, _) -> name
  | File _ -> raise NotADir

let dir_content = function
  | Dir (_, content) -> content
  | File _ -> raise NotADir

exception IgnoredCase

let rec read_dir (content: file_sys) = function
  | [] -> content, []
  | cmd :: cmds ->
    (* cmds shouldn't be an immutable stack *)
    match cmd with
    | Cd dir ->
      if String.equal ".." dir
      then content, cmds
      else
        (* New directory *)
        let new_dir = (Dir (dir, [])) in
        let new_dir_content, cmds = read_dir new_dir cmds in
        let content = cons new_dir_content content in
        read_dir content cmds
    | FileDescription size ->
      let file = File size in
      let content = cons file content in
      read_dir content cmds
    | _ -> raise IgnoredCase

let parser cmds =
  let ignore_cases =
    List.filter (function
      | DirDescription _ (* Not cool *)
      | Ls -> false
      | _ -> true
    )
  in
  ignore_cases cmds
  |> read_dir (Dir ("Ignore",[]))
  |> fst
  |> dir_content
  |> List.hd

let sum2 = List.fold_left (fun (a,b) (a',b') -> a + a', b + b') (0, 0)

let rec solve = function
  | File size -> size, 0
  | Dir (name, content) ->
    let size, sub_sum = List.map solve content |> sum2 in
    let sum = sub_sum + if size <= 100_000 then size else 0 in
    size, sum

let () =
  read_file "input" lexer
  |> parser
  |> solve
  |> snd
  |> print_int; print_newline ()

(* 1491614 *)
