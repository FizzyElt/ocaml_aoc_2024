module IntMap = Map.Make (Int)

let get_middle_num list =
  let len = List.length list in
  List.nth list (len / 2)
;;

let get_pair_num line =
  line
  |> String.split_on_char '|'
  |> fun l ->
  match l with
  | [ a; b ] -> int_of_string a, int_of_string b
  | _ -> failwith "not valid input"
;;

let get_num_of_list line = line |> String.split_on_char ',' |> List.map int_of_string

let make_map list =
  list
  |> List.fold_left
       (fun acc line ->
          let left, right = get_pair_num line in
          match IntMap.find_opt left acc with
          | Some xs -> IntMap.add left (right :: xs) acc
          | None -> IntMap.add left [ right ] acc)
       IntMap.empty
;;

let is_valid order_rules_map num list =
  if list = []
  then true
  else
    list
    |> List.exists (fun x ->
      order_rules_map
      |> IntMap.find_opt x
      |> Option.map (List.exists (fun y -> y = num))
      |> Option.value ~default:false)
    |> not
;;

let is_correct order_rules_map num_list =
  let rec loop acc head tail =
    if tail = []
    then acc
    else (
      let valid = is_valid order_rules_map head tail in
      if valid then loop acc (List.hd tail) (List.tl tail) else false)
  in
  loop true (List.hd num_list) (List.tl num_list)
;;
