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
let times_pair (x, y) times = x * times, y * times

let round a b =
  let c = a mod b in
  if c < 0 then c + b else c
;;

let round_pair (x, y) (w, h) = round x w, round y h

let get_pair str =
  let split_str = String.split_on_char ',' str in
  let left = List.nth split_str 0 in
  let right = List.nth split_str 1 in
  get_digit_str left, get_digit_str right
;;

let get_new_position position velocity times =
  let offset = times_pair velocity times in
  let new_position =
    add_pair position offset |> fun pair -> round_pair pair (width, height)
  in
  new_position
;;

let parse_line line =
  let split_line = String.split_on_char ' ' line in
  let position = List.nth split_line 0 |> get_pair in
  let velocity = List.nth split_line 1 |> get_pair in

  position, velocity
;;

let make_board () = Array.make_matrix height width '.'

let set_points list board =
  List.iter (fun (x, y) -> board.(y).(x) <- '#') list;
  board
;;

let print_board board =
  board
  |> Array.iter (fun line ->
    line |> Array.iter (fun c -> Printf.printf "%c" c);
    Printf.printf "\n");
  Printf.printf "\n"
;;

let render list times =
  let board = make_board () in
  let new_list = list |> List.map (fun (pos, vel) -> get_new_position pos vel times) in
  let board = set_points new_list board in
  Printf.printf "%d\n" times;
  print_board board;
  ()
;;

let times = 60
let base = 1301
let start = base + (times * 101)
let max = base + ((times + 20) * 101)

(* 29076 *)
(* 8270 *)

let rec loop list times =
  render list times;
  if times = max then () else loop list (times + 101)
;;

(* 680 710 *)
let () =
  let file = Sys.argv.(1) in
  let list = File.read_list_of_line parse_line file in
  loop list start;
  ()
;;
