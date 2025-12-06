let parse_line line =
  line
  |> String.split_on_char ' '
  |> List.to_seq
  |> Seq.filter (fun x -> x <> "")
  |> Seq.take 2
  |> Seq.map int_of_string
  |> List.of_seq
;;

let list_to_pair list =
  match list with
  | [ a; b ] -> a, b
  | _ -> failwith "not valid input"
;;

let split_two_list list =
  let rec loop acc list =
    let left_list, right_list = acc in
    match list with
    | [] -> acc
    | x :: tl ->
      let left, right = list_to_pair x in
      loop (left :: left_list, right :: right_list) tl
  in
  loop ([], []) list
;;
