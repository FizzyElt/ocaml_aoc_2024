open Lib

let rec find_all_matches reg str pos =
  try
    let result = Str.search_forward reg str pos in
    let matched = Str.matched_string str in
    matched :: find_all_matches reg str (result + 1)
  with
  | Not_found -> []
;;

let mul_regexp = Str.regexp {|mul([0-9]+,[0-9]+)|}

let mul_two_nums str =
  let reg = Str.regexp {|\([0-9]+\)|} in
  let rest = Str.substitute_first reg (fun _ -> "") str in
  let l = Str.matched_string str |> int_of_string in
  let _ = Str.substitute_first reg (fun _ -> "") rest in
  let r = Str.matched_string rest |> int_of_string in
  l * r
;;

let () =
  Sys.argv.(1)
  |> File.read_file
  |> fun str ->
  find_all_matches mul_regexp str 0
  |> List.fold_left (fun acc str -> mul_two_nums str + acc) 0
  |> print_int
;;
