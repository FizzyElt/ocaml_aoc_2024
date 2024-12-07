open Lib

let opts = [ '+'; '*'; '|' ]

let operate opt a b =
  match opt with
  | '+' -> a + b
  | '*' -> a * b
  | '|' -> string_of_int a ^ string_of_int b |> int_of_string
  | _ -> failwith "invalid operation"
;;

let evaluate ints origin =
  let rec loop acc l =
    match l with
    | [] -> acc = origin
    | x :: xs -> List.exists (fun opt -> loop (operate opt acc x) xs) opts
  in
  match ints with
  | [] -> false
  | [ x ] -> origin = x
  | [ x; y ] -> loop x [ y ]
  | x :: xs -> loop x xs
;;

let parse_line line =
  let split_line = String.split_on_char ':' line in
  let origin_num = split_line |> List.hd |> int_of_string in
  let nums =
    List.nth split_line 1
    |> String.split_on_char ' '
    |> List.filter (fun s -> s <> "")
    |> List.map (fun n -> n |> String.trim |> int_of_string)
  in
  origin_num, nums
;;

let result =
  List.fold_left
    (fun acc (total, nums) ->
       let valid_num = if evaluate nums total then total else 0 in
       valid_num + acc)
    0
;;

let () =
  let file = Sys.argv.(1) in
  let list = File.read_list_of_line parse_line file in
  let res = result list in
  print_int res
;;
