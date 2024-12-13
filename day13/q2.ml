open Lib
open Lib.Util

let price = 10000000000000
let zero_char_int = int_of_char '0'
let nine_char_int = int_of_char '9'
let is_digit c = c >= zero_char_int && c <= nine_char_int

let get_digit_str str =
  str
  |> String.to_seq
  |> Seq.filter (fun c -> is_digit (int_of_char c))
  |> String.of_seq
  |> int_of_string
;;

let cal_matrix (a1, b1) (a2, b2) = (a1 * b2) - (b1 * a2)

let solve_equation (a1, b1) (a2, b2) (c1, c2) =
  let base = cal_matrix (a1, b1) (a2, b2) in
  let x = cal_matrix (c1, b1) (c2, b2) in
  let y = cal_matrix (a1, c1) (a2, c2) in

  match x mod base, y mod base with
  | 0, 0 -> x / base, y / base
  | _, _ -> 0, 0
;;

let split_input str =
  let list = String.split_on_char '\n' str in
  let a_button, b_button, c_result = List.nth list 0, List.nth list 1, List.nth list 2 in
  let a1, b1 =
    a_button
    |> String.split_on_char ','
    |> fun l ->
    let a = List.nth l 0 in
    let b = List.nth l 1 in
    get_digit_str a, get_digit_str b
  in
  let a2, b2 =
    b_button
    |> String.split_on_char ','
    |> fun l ->
    let a = List.nth l 0 in
    let b = List.nth l 1 in
    get_digit_str a, get_digit_str b
  in
  let c1, c2 =
    c_result
    |> String.split_on_char ','
    |> fun l ->
    let a = List.nth l 0 in
    let b = List.nth l 1 in
    get_digit_str a, get_digit_str b
  in
  (a1, a2), (b1, b2), (c1 + price, c2 + price)
;;

let chunk_list str = Str.split (Str.regexp "\n\n") str

let () =
  let str = File.read_file Sys.argv.(1) in
  let list = chunk_list str in
  let res =
    list
    |> List.map split_input
    |> List.map (fun (a, b, c) -> solve_equation a b c)
    |> List.mapi (fun i (x, y) -> (x * 3) + y)
    |> sum_of_list
  in
  Printf.printf "%d, %d\n" res Int.max_int
;;
