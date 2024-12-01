open Lib

let parse_line line =
  line
  |> String.split_on_char ' '
  |> List.to_seq
  |> Seq.filter (fun x -> x <> "")
  |> Seq.take 2
  |> Seq.map int_of_string
  |> List.of_seq
;;

let split_two_list list =
  let rec loop acc list =
    let left_list, right_list = acc in
    match list with
    | [] -> acc
    | x :: tl ->
      let left, right =
        match x with
        | [ a; b ] -> a, b
        | _ -> failwith "not valid input"
      in
      loop (left :: left_list, right :: right_list) tl
  in
  loop ([], []) list
;;

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
  |> File.read_list_of_line parse_line
  |> split_two_list
  |> result
  |> print_int
;;
