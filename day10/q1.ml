open Lib
open Util
open Pair

type grid = int array array
type coord = int * int

let get_ele_by_matrix x y mat =
  try
    let res = mat.(x).(y) in
    Some res
  with
  | Invalid_argument _ -> None
;;

let find_possible_road ((x, y) : coord) (board : grid) =
  let direction = [ -1, 0; 0, 1; 1, 0; 0, -1 ] in
  let visited = ref PairSet.empty in
  let rec loop (row, col) n =
    match get_ele_by_matrix row col board with
    | None -> 0
    | Some c when c = n ->
      if n = 9 && PairSet.mem (row, col) !visited
      then 0
      else if n = 9
      then (
        visited := PairSet.add (row, col) !visited;
        1)
      else
        direction
        |> List.fold_left
             (fun acc (x, y) -> acc + loop (add_pair (row, col) (x, y)) (n + 1))
             0
    | Some _ -> 0
  in
  loop (x, y) 0
;;

let result list =
  let board =
    list
    |> List.map (fun line -> line |> String.to_seq |> Seq.map char_digit |> Array.of_seq)
    |> Array.of_list
  in
  board
  |> Array.mapi (fun row line ->
    line
    |> Array.mapi (fun col c -> if c = 0 then find_possible_road (row, col) board else 0))
  |> Array.fold_left (fun acc line -> sum_of_array line + acc) 0
;;

let () = Sys.argv.(1) |> File.read_list_of_line Fun.id |> result |> print_int
