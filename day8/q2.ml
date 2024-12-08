open Lib
open Day8lib.Common

let find_all_antinodes pos_a pos_b grid =
  let rec loop_out_of_bound pos diff operator acc =
    let next_row, next_col = operator pos diff in
    match get_ele_by_matrix next_row next_col grid with
    | Some _ ->
      loop_out_of_bound (next_row, next_col) diff operator ((next_row, next_col) :: acc)
    | None -> acc
  in

  let diff = sub_pair pos_a pos_b in
  (pos_a :: pos_b :: loop_out_of_bound pos_a diff add_pair [])
  @ loop_out_of_bound pos_b diff sub_pair []
;;

let get_all_antinodes list grid =
  let rec all_uniq_pairs acc list =
    match list with
    | [] -> acc
    | x :: xs ->
      let antennas_pairs = xs |> List.map (fun y -> x, y) in
      all_uniq_pairs (antennas_pairs @ acc) xs
  in

  let uniq_antennas_pairs = all_uniq_pairs [] list in
  uniq_antennas_pairs
  |> List.fold_left
       (fun acc (pair_a, pair_b) -> find_all_antinodes pair_a pair_b grid @ acc)
       []
  |> PairSet.of_list
  |> PairSet.elements
;;

let find_all_antennas (grid : char array array) =
  let antennas_map = ref CharMap.empty in
  grid
  |> Array.iteri (fun row line ->
    line
    |> Array.iteri (fun col c ->
      if c != '.'
      then
        antennas_map
        := match CharMap.find_opt c !antennas_map with
           | None -> CharMap.add c [ row, col ] !antennas_map
           | Some list -> CharMap.add c ((row, col) :: list) !antennas_map));
  !antennas_map
;;

let set_all_antinodes (grid : char array array) =
  let antennas_map = find_all_antennas grid in
  let antennas_pairs = CharMap.to_list antennas_map in

  antennas_pairs
  |> List.map (fun (_, points) -> get_all_antinodes points grid)
  |> List.iter (fun points ->
    points
    |> List.iter (fun (row, col) ->
      match get_ele_by_matrix row col grid with
      | Some c when c != '#' -> grid.(row).(col) <- '#'
      | _ -> ()));

  grid
;;

let result (list : string list) =
  let grid =
    list |> List.map (fun line -> line |> String.to_seq |> Array.of_seq) |> Array.of_list
  in
  let new_grid = set_all_antinodes grid in
  new_grid
  |> Array.map (fun line ->
    line |> Array.fold_left (fun acc c -> if c = '#' then acc + 1 else acc) 0)
  |> Array.fold_left ( + ) 0
;;

let () = Sys.argv.(1) |> File.read_list_of_line Fun.id |> result |> print_int
