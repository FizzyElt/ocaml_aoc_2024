open Lib

let check_four_road board row col =
  let count = ref 0 in
  let direction = [ 1, 1; -1, 1; -1, -1; 1, -1 ] in
  direction
  |> List.iter (fun (m_x, m_y) ->
    try
      let start_row = row + m_x in
      let start_col = col + m_y in
      let c = board.(start_row).[start_col] in
      if c = 'M'
         && Common.check_road board (start_row, start_col) 3 (m_x * -1, m_y * -1) = "MAS"
      then count := !count + 1
    with
    | Invalid_argument _ -> ());
  !count
;;

let result board =
  let count = ref 0 in
  board
  |> Array.iteri (fun row line ->
    line
    |> String.iteri (fun col c ->
      if c = 'A' && check_four_road board row col >= 2 then count := !count + 1));
  !count
;;

let () =
  Sys.argv.(1)
  |> File.read_list_of_line Fun.id
  |> List.to_seq
  |> Array.of_seq
  |> result
  |> print_int
;;
