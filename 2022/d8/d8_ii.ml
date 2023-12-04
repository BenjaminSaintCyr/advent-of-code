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
  let rec count_score height score = function
    | [] -> score
    | hd :: tl ->
      if height <= hd then score + 1
      else count_score height (score + 1) tl
  in
  let rec loop prev line_score = function
    | [] -> List.rev line_score
    | hd :: tl ->
      let score = count_score hd 0 prev in
      loop (hd :: prev) (score :: line_score) tl
  in
  loop [] [] l

let int_or = (fun x y -> if x == 1 || y == 1 then 1 else 0)

let check_merge = List.map2 Int.mul

let check_both l = check_merge (List.rev l |> check |> List.rev) (check l)

let check_merge2 = List.map2 check_merge

let check_all l =
  let horz = List.map check_both l in
  let vert = transpose l |> List.map check_both |> transpose in
  check_merge2 horz vert


let max_list = List.fold_left Int.max Int.min_int

let max2d l = List.map max_list l |> max_list

let () =
  read_file_list "input"
  |> check_all
  |> max2d
  |> Printf.printf "%d\n"
