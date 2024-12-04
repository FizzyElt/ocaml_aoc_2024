let check_road board (row, col) step (step_x, step_y) =
  let rec find acc x y step =
    if step = 0
    then acc
    else (
      try
        let c = board.(x).[y] in
        find (c :: acc) (x + step_x) (y + step_y) (step - 1)
      with
      | Invalid_argument _ -> acc)
  in
  find [] row col step |> List.rev |> List.to_seq |> String.of_seq
;;
