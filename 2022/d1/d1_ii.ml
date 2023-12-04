let filename = "input"

let top = List.fold_left (max) Int.min_int

(* O(n*l) could O(l) *)
let top_n n l =
  let rec topper i l acc =
    if i >= n then acc
    else
      let top_element = top l in
      let l' = (List.filter ((!=) top_element) l) in
      let i' = (i + 1) in
      let acc' = (top_element :: acc) in
      topper i' l' acc' in
  topper 0 l []

let sum_elfs filename =
  let lines = ref [] in
  let acc = ref 0 in
  let chan = open_in filename in
  try
    while true; do
      lines :=
        input_line chan
        |> int_of_string_opt
        |> function
        | Some x -> acc := !acc + x; !lines
        | None -> lines := !acc :: !lines; acc := 0; !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    !lines

let () =
  sum_elfs filename
  |> top_n 3
  |> List.fold_left (+) 0
  |> print_int
