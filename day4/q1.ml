open Lib
open Day4lib

let check_all_road board row col =
  let direction = [ 1, 0; 1, 1; 0, 1; -1, 1; -1, 0; -1, -1; 0, -1; 1, -1 ] in
  direction
  |> List.filter (fun (m_x, m_y) ->
    Common.check_road board (row, col) 4 (m_x, m_y) = "XMAS")
  |> List.length
;;

let result board =
  let count = ref 0 in
  board
  |> Array.iteri (fun row line ->
    line
    |> String.iteri (fun col c ->
      if c = 'X' then count := !count + check_all_road board row col));
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
