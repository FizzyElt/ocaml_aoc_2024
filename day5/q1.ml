open Lib
open Common

let result list =
  let list = list |> List.filter (fun line -> String.length line > 0) in
  let a, b = list |> List.partition (fun line -> String.contains line '|') in
  let order_rules_map = Common.make_map a in
  let page_nums = List.map get_num_of_list b in
  List.fold_left
    (fun acc list ->
       let correct = is_correct order_rules_map list in
       if correct then acc + get_middle_num list else acc)
    0
    page_nums
;;

let () = Sys.argv.(1) |> File.read_list_of_line Fun.id |> result |> print_int
