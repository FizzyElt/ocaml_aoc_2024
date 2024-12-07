open Lib
open Day6lib.Common

let rec move (x, y) dir board =
  let m_x, m_y = next_pos (x, y) dir in
  try
    let c = board.(m_x).(m_y) in
    if c = '#'
    then move (x, y) (turn_right dir) board
    else (
      board.(x).(y) <- 'X';
      move (m_x, m_y) dir board)
  with
  | Invalid_argument _ -> board
;;

let result (list : string list) =
  let board =
    list |> List.map (fun line -> line |> String.to_seq |> Array.of_seq) |> Array.of_list
  in
  let guard_pos = find_guard board in
  let new_board = move guard_pos Up board in
  new_board
  |> Array.fold_left
       (fun acc line -> Array.append line [| '\n' |] |> Array.append acc)
       [||]
  |> Array.to_seq
  |> Seq.filter (fun c -> c = 'X')
  |> Seq.length
  |> Int.add 1
;;

let () = Sys.argv.(1) |> File.read_list_of_line Fun.id |> result |> print_int
