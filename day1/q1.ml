open Lib

let sum_of_diff pair_of_list =
  let left_list, right_list = pair_of_list in
  let left_list = List.sort Int.compare left_list in
  let right_list = List.sort Int.compare right_list in
  List.fold_left2 (fun acc left right -> acc + abs (left - right)) 0 left_list right_list
;;

let () =
  Sys.argv.(1)
  |> File.read_list_of_line Common.parse_line
  |> Common.split_two_list
  |> sum_of_diff
  |> print_int
;;
