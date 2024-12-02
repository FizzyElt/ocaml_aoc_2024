open Lib

let check_all_case_safe list =
  if Common.is_safe list
  then true
  else
    List.init (List.length list) Fun.id
    |> List.exists (fun i ->
      let remove_one_list = List.filteri (fun j _ -> j <> i) list in
      Common.is_safe remove_one_list)
;;

let result list = list |> List.filter check_all_case_safe |> List.length
let () = Sys.argv.(1) |> File.read_list_of_line Common.parse_line |> result |> print_int
