open Lib
open Day5lib.Common

let reorder list order_rules_map =
  let rec insert acc nums x =
    match acc with
    | [] -> [ x ]
    | y :: ys ->
      if List.exists (fun a -> a = y) nums then x :: y :: ys else y :: insert ys nums x
  in
  let rec loop acc l =
    match l with
    | [] -> acc
    | x :: xs ->
      let nums = IntMap.find_opt x order_rules_map |> Option.value ~default:[] in
      loop (insert acc nums x) xs
  in
  loop [] list
;;

let result list =
  let list = list |> List.filter (fun line -> String.length line > 0) in
  let a, b = list |> List.partition (fun line -> String.contains line '|') in
  let order_rules_map = make_map a in
  let page_nums = List.map get_num_of_list b in
  page_nums
  |> List.fold_left
       (fun acc list ->
          let correct = is_correct order_rules_map list in
          if not correct
          then (
            let reordered = reorder list order_rules_map in
            acc + get_middle_num reordered)
          else acc)
       0
;;

let () = Sys.argv.(1) |> File.read_list_of_line Fun.id |> result |> print_int
