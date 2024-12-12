open Lib
open Lib.Pair
open Lib.Util

let cal_perimeters_and_area ch (row, col) (grid : char array array) =
  let direction = [ 1, 0; 0, 1; -1, 0; 0, -1 ] in
  let visited = ref PairSet.empty in
  let rec loop (row, col) =
    if PairSet.mem (row, col) !visited
    then 0
    else (
      match get_ele_by_matrix row col grid with
      | None -> 1
      | Some c when c != ch -> 1
      | Some _ ->
        visited := PairSet.add (row, col) !visited;
        direction
        |> List.fold_left (fun acc (x, y) -> acc + loop (add_pair (row, col) (x, y))) 0)
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
