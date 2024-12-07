open Lib
open Day1lib
module IntMap = Map.Make (Int)

let create_num_count list =
  let acc = IntMap.empty in
  List.fold_left
    (fun acc x ->
       match IntMap.find_opt x acc with
       | Some n -> IntMap.add x (n + 1) acc
       | None -> IntMap.add x 1 acc)
    acc
    list
;;

let result pair_of_list =
  let left_list, right_list = pair_of_list in
  let right_map = create_num_count right_list in
  List.fold_left
    (fun acc num ->
       match IntMap.find_opt num right_map with
       | Some c -> (num * c) + acc
       | None -> acc)
    0
    left_list
;;

let () =
  Sys.argv.(1)
  |> File.read_list_of_line Common.parse_line
  |> Common.split_two_list
  |> result
  |> print_int
;;
