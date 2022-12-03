let filename = "input"

let (||>) (a, b) f = f a b

let (|>!) x sideEffect = sideEffect x; x

let solve filename = 
  let lines = ref (0, 0) in
  let chan = open_in filename in
  try
    while true; do
      lines :=
        input_line chan
        |> int_of_string_opt
        |> function
        | Some x ->
          let (a, b) = !lines in
          (x + a, b)
        | None -> (0, !lines ||> max)
    done; snd !lines
  with End_of_file ->
    close_in chan;
    snd !lines

let () =
  solve filename
  |> print_int
