open Lib

let split_two str =
  let len = String.length str in
  let mid = len / 2 in
  let left = String.sub str 0 mid in
  let right = String.sub str mid (len - mid) in
  [ left; int_of_string right |> string_of_int ]
;;

let rec blink (list : string list) times =
  if times = 0
  then List.length list
  else
    list
    |> List.fold_left
         (fun acc str ->
            let new_times = times - 1 in
            let count =
              match str with
              | "0" -> blink [ "1" ] new_times
              | s when String.length s mod 2 = 0 -> blink (split_two s) new_times
              | s -> blink [ int_of_string s * 2024 |> string_of_int ] new_times
            in
            acc + count)
         0
;;

let () =
  let file = Sys.argv.(1) in
  let str = File.read_file file in
  let list = String.split_on_char ' ' str in
  let count = blink list 25 in
  Printf.printf "%d\n" count
;;
