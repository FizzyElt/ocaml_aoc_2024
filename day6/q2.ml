open Lib
open Day6lib.Common

module IntTriple = struct
  type t = int * int * direction

  let dir_order = function
    | Up -> 0
    | Down -> 1
    | Left -> 2
    | Right -> 3
  ;;

  let dir_compare d0 d1 = dir_order d0 - dir_order d1

  let compare (x0, y0, z0) (x1, y1, z1) =
    match Stdlib.compare x0 x1 with
    | 0 ->
      (match Stdlib.compare y0 y1 with
       | 0 -> dir_compare z0 z1
       | c -> c)
    | c -> c
  ;;
end

module TripleSet = Set.Make (IntTriple)

let rec move_guard grid (g_row, g_col) direction visited =
  let n_row, n_col = next_pos (g_row, g_col) direction in
  let new_visited = PairSet.add (g_row, g_col) visited in

  match get_ele_by_matrix n_row n_col grid with
  | Some '#' -> move_guard grid (g_row, g_col) (turn_right direction) new_visited
  | Some _ -> move_guard grid (n_row, n_col) direction new_visited
  | None -> new_visited
;;

let rec loop_search grid (g_row, g_col) direction visited =
  let n_row, n_col = next_pos (g_row, g_col) direction in
  let current_position = g_row, g_col, direction in

  match get_ele_by_matrix n_row n_col grid with
  | None -> 0
  | Some '#' -> loop_search grid (g_row, g_col) (turn_right direction) visited
  | Some _ when TripleSet.mem current_position visited -> 1
  | Some _ ->
    let new_visited = TripleSet.add current_position visited in
    loop_search grid (n_row, n_col) direction new_visited
;;

let make_grid (list : string list) =
  list |> List.map (fun line -> line |> String.to_seq |> Array.of_seq) |> Array.of_list
;;

let result list =
  let grid = make_grid list in
  let guard_pos = find_guard grid in
  let visited = move_guard grid guard_pos Up PairSet.empty in
  let visited = PairSet.remove guard_pos visited in

  visited
  |> PairSet.elements
  |> List.fold_left
       (fun acc (row, col) ->
          grid.(row).(col) <- '#';
          let count = loop_search grid guard_pos Up TripleSet.empty in
          grid.(row).(col) <- '.';
          acc + count)
       0
;;

let () =
  let file = Sys.argv.(1) in
  let list = File.read_list_of_line Fun.id file in
  let res = result list in
  Printf.printf "%d\n" res
;;
