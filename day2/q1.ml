open Lib
open Day2lib

let result list = list |> List.filter Common.is_safe |> List.length
let () = Sys.argv.(1) |> File.read_list_of_line Common.parse_line |> result |> print_int
