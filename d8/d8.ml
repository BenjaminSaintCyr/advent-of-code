(* SQUARE *)

let read_file_list filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      let result = input_line chan |> String.to_seq |> List.of_seq |> List.map (fun x -> int_of_char x - 48) in
      lines := result :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines (* put back in order*)

let rec transpose list = match list with
  | []             -> []
  | []   :: xss    -> transpose xss
  | (x::xs) :: xss ->
    (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

let check l =
  let rec loop acc max_prev = function
    | [] -> List.rev acc
    | hd :: tl ->
      let acc, max_prev =
        if hd > max_prev
        then 1 :: acc, hd (* visible *)
        else 0 :: acc, max_prev (* hidden *)
      in
      loop acc max_prev tl
  in
  loop [] (-1) l

let int_or = (fun x y -> if x == 1 || y == 1 then 1 else 0)

let check_merge = List.map2 int_or

let check_both l = check_merge (List.rev l |> check |> List.rev) (check l)

let check_merge2 = List.map2 check_merge

let check_all l =
  let horz = List.map check_both l in
  let vert = transpose l |> List.map check_both |> transpose in
  check_merge2 horz vert

let sum = List.fold_left (+) 0

let sum2d l = List.map sum l |> sum

let () =
  read_file_list "input"
  |> check_all
  |> sum2d
  |> Printf.printf "%d\n"
