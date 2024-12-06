open Lib

type direction =
  | Up
  | Down
  | Left
  | Right

let next_pos (x, y) dir =
  match dir with
  | Up -> x - 1, y
  | Down -> x + 1, y
  | Left -> x, y - 1
  | Right -> x, y + 1
;;

let turn_right dir =
  match dir with
  | Up -> Right
  | Down -> Left
  | Left -> Up
  | Right -> Down
;;

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

let find_guard board =
  let x = ref 0 in
  let y = ref 0 in
  board
  |> Array.iteri (fun row line ->
    line
    |> Array.iteri (fun col c ->
      if c = '^'
      then (
        x := row;
        y := col)));
  !x, !y
;;

let result (list : string list) =
  let board =
    list |> List.map (fun line -> line |> String.to_seq |> Array.of_seq) |> Array.of_list
  in
  let guard_pos = find_guard board in
  Printf.printf "%d, %d\n" (fst guard_pos) (snd guard_pos);
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
