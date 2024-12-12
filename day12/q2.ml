open Lib
open Lib.Pair
open Lib.Util

let direction = [ 1, 0; 0, 1; -1, 0; 0, -1 ]

(* T *)
let find_corners_neighbor_three ch (row, col) direction grid =
  (* 凸點 *)
  let bump = direction |> List.fold_left add_pair (0, 0) in

  direction
  |> List.fold_left
       (fun acc delta ->
          if delta = bump
          then acc
          else (
            let new_row, new_col = add_pair (row, col) (add_pair bump delta) in
            match get_ele_by_matrix new_row new_col grid with
            | Some c when c = ch -> acc
            | Some _ -> acc + 1
            | None -> acc))
       0
;;

(* X *)
let find_corners_neighbor_four ch (row, col) grid =
  let direction = [ -1, 1; 1, 1; 1, -1; -1, -1 ] in
  direction
  |> List.filter (fun delta ->
    let new_row, new_col = add_pair (row, col) delta in
    match get_ele_by_matrix new_row new_col grid with
    | Some c when c = ch -> false
    | _ -> true)
  |> List.length
;;

(* |, -, L *)
let find_corners_neighbor_two ch (row, col) direction grid =
  let delta_x, delta_y = direction |> List.fold_left add_pair (0, 0) in
  if delta_x = 0 && delta_y = 0
  then 0
  else (
    match get_ele_by_matrix (row + delta_x) (col + delta_y) grid with
    | Some c when c = ch -> 1
    | Some _ -> 2
    | None -> failwith "impossible")
;;

let find_corners ch (row, col) grid =
  let neighbors =
    direction
    |> List.filter (fun (x, y) ->
      let row, col = add_pair (row, col) (x, y) in
      match get_ele_by_matrix row col grid with
      | Some c when c = ch -> true
      | _ -> false)
  in

  let neighbor_len = List.length neighbors in
  if neighbor_len = 0
  then 4
  else if neighbor_len = 1
  then 2
  else if neighbor_len = 3
  then find_corners_neighbor_three ch (row, col) neighbors grid
  else if neighbor_len = 4
  then find_corners_neighbor_four ch (row, col) grid
  else find_corners_neighbor_two ch (row, col) neighbors grid
;;

let cal_perimeters_and_area ch (row, col) (grid : char array array) =
  let visited = ref PairSet.empty in
  let rec loop (row, col) =
    if PairSet.mem (row, col) !visited
    then 0
    else (
      match get_ele_by_matrix row col grid with
      | None -> 0
      | Some c when c != ch -> 0
      | Some _ ->
        visited := PairSet.add (row, col) !visited;
        let corners = find_corners ch (row, col) grid in
        direction
        |> List.fold_left
             (fun acc (x, y) -> acc + loop (add_pair (row, col) (x, y)))
             corners)
  in
  let perimeters = loop (row, col) in
  let area = PairSet.cardinal !visited in

  (* clean area *)
  !visited |> PairSet.iter (fun (row, col) -> grid.(row).(col) <- '.');
  perimeters * area
;;

let result grid =
  let total = ref 0 in
  grid
  |> Array.iteri (fun row line ->
    line
    |> Array.iteri (fun col c ->
      if c != '.' then total := !total + cal_perimeters_and_area c (row, col) grid));
  !total
;;

let () =
  let file = Sys.argv.(1) in
  let list =
    File.read_list_of_line (fun line -> line |> String.to_seq |> Array.of_seq) file
  in
  let grid = Array.of_list list in
  let res = result grid in
  Printf.printf "%d\n" res
;;
