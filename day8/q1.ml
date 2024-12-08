open Lib
open Day8lib.Common

let get_antinodes pos_a pos_b =
  let diff = sub_pair pos_a pos_b in
  let ll = add_pair pos_a diff in
  let rr = sub_pair pos_b diff in
  [ ll; rr ]
;;

let get_all_antinodes list =
  let rec loop acc list =
    match list with
    | [] -> acc
    | x :: xs ->
      let antennas_pairs = xs |> List.map (fun y -> x, y) in
      loop (antennas_pairs @ acc) xs
  in
  let uniq_antennas_pairs = loop [] list in
  uniq_antennas_pairs
  |> List.fold_left (fun acc (pair_a, pair_b) -> get_antinodes pair_a pair_b @ acc) []
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
  |> List.map (fun (c, points) -> c, get_all_antinodes points)
  |> List.iter (fun (_, points) ->
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
  |> Array.fold_left
       (fun acc line ->
          line |> Array.fold_left (fun acc c -> if c = '#' then acc + 1 else acc) acc)
       0
;;

let () = Sys.argv.(1) |> File.read_list_of_line Fun.id |> result |> print_int
