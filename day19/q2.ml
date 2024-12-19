open Lib
open Lib.Util
module StringMap = Map.Make (String)

let make_is_possible (towels : string list) =
  let memo = ref StringMap.empty in

  let rec is_possible design =
    if StringMap.mem design !memo
    then StringMap.find design !memo
    else if design = ""
    then 1
    else (
      let res =
        towels
        |> List.fold_left
             (fun acc towel ->
                if String.starts_with design ~prefix:towel
                then
                  (let design_len = String.length design in
                   let towel_len = String.length towel in
                   is_possible (String.sub design towel_len (design_len - towel_len)))
                  + acc
                else acc)
             0
      in
      memo := StringMap.add design res !memo;
      res)
  in
  is_possible
;;

let parse_input (input : string) =
  let split_input = Str.split (Str.regexp "\n\n") input in
  let towels = List.nth split_input 0 |> Str.split (Str.regexp ", ") in
  let designs = List.nth split_input 1 |> String.split_on_char '\n' in

  towels, designs
;;

let () =
  let file = Sys.argv.(1) in
  let str = File.read_file file in
  let towels, designs = parse_input str in
  let is_possible = make_is_possible towels in
  let count = designs |> List.map is_possible |> sum_of_list in
  Printf.printf "%d\n" count
;;
