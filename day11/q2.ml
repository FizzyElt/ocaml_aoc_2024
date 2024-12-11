open Lib

module StrPairs = struct
  type t = string * int

  let compare (x0, y0) (x1, y1) =
    match Stdlib.compare x0 x1 with
    | 0 -> Stdlib.compare y0 y1
    | c -> c
  ;;
end

module PairsMap = Map.Make (StrPairs)

let split_two str =
  let len = String.length str in
  let mid = len / 2 in
  let left = String.sub str 0 mid in
  let right = String.sub str mid (len - mid) in
  [ left; int_of_string right |> string_of_int ]
;;

let blink (list : string list) times =
  let memo = ref PairsMap.empty in
  let rec loop (list : string list) times =
    if times = 0
    then List.length list
    else
      list
      |> List.fold_left
           (fun acc str ->
              let new_times = times - 1 in
              let count =
                match PairsMap.find_opt (str, times) !memo with
                | Some n -> n
                | None ->
                  let result =
                    match str with
                    | "0" -> loop [ "1" ] new_times
                    | s when String.length s mod 2 = 0 -> loop (split_two s) new_times
                    | s -> loop [ int_of_string s * 2024 |> string_of_int ] new_times
                  in
                  memo := PairsMap.add (str, times) result !memo;
                  result
              in

              acc + count)
           0
  in
  loop list times
;;

let () =
  let file = Sys.argv.(1) in
  let str = File.read_file file in
  let list = String.split_on_char ' ' str in
  let count = blink list 75 in
  Printf.printf "%d\n" count
;;
