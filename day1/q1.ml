open Lib

let parse_line line =
  line
  |> String.split_on_char ' '
  |> List.filter (fun x -> x <> "")
  |> List.to_seq
  |> Seq.take 2
  |> List.of_seq
  |> List.map int_of_string
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

let sum_of_diff pair_of_list =
  let left_list, right_list = pair_of_list in
  let left_list = List.sort Int.compare left_list in
  let right_list = List.sort Int.compare right_list in
  List.fold_left2 (fun acc left right -> acc + abs (left - right)) 0 left_list right_list
;;

let () =
  Sys.argv.(1)
  |> File.read_list_of_line parse_line
  |> split_two_list
  |> sum_of_diff
  |> print_int
;;
