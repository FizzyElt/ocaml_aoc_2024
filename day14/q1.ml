open Lib.Util
open Lib.Pair
open Lib

let get_digit_str str =
  str
  |> String.to_seq
  |> Seq.filter (fun c -> is_digit c || c = '-')
  |> String.of_seq
  |> int_of_string
;;

let width = 101
let height = 103
let width_half = width / 2
let height_half = height / 2
let times_pair (x, y) times = x * times, y * times

let round a b =
  let c = a mod b in
  if c < 0 then c + b else c
;;

let round_pair (x, y) (w, h) = round x w, round y h
let is_in_quadrant_one (x, y) = x >= 0 && x < width_half && y >= 0 && y < height_half
let is_in_quadrant_two (x, y) = x > width_half && x < width && y >= 0 && y < height_half

let is_in_quadrant_three (x, y) =
  x >= 0 && x < width_half && y > height_half && y < height
;;

let is_in_quadrant_four (x, y) =
  x > width_half && x < width && y > height_half && y < height
;;

let get_pair str =
  let split_str = String.split_on_char ',' str in
  let left = List.nth split_str 0 in
  let right = List.nth split_str 1 in
  get_digit_str left, get_digit_str right
;;

let parse_line line =
  let split_line = String.split_on_char ' ' line in
  let position = List.nth split_line 0 |> get_pair in
  let velocity = List.nth split_line 1 |> get_pair in
  let offset = times_pair velocity 100 in
  let new_position =
    add_pair position offset |> fun pair -> round_pair pair (width, height)
  in

  (* Printf.printf
    "pos: (%d %d) vel: (%d %d) new_pos: (%d %d)\n"
    (fst position)
    (snd position)
    (fst velocity)
    (snd velocity)
    (fst new_position)
    (snd new_position); *)
  new_position
;;

let result (list : (int * int) list) =
  let quadrant_one = List.filter is_in_quadrant_one list |> List.length in
  let quadrant_two = List.filter is_in_quadrant_two list |> List.length in
  let quadrant_three = List.filter is_in_quadrant_three list |> List.length in
  let quadrant_four = List.filter is_in_quadrant_four list |> List.length in
  (* Printf.printf "%d %d %d %d\n" quadrant_one quadrant_two quadrant_three quadrant_four; *)
  quadrant_one * quadrant_two * quadrant_three * quadrant_four
;;

let () =
  let file = Sys.argv.(1) in
  let list = File.read_list_of_line parse_line file in
  (* List.iter (fun (x, y) -> Printf.printf "%d %d\n" x y) list; *)
  let res = result list in
  Printf.printf "%d\n" res
;;
