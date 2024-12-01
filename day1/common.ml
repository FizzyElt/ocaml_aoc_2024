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
