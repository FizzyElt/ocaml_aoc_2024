let get_ele_by_matrix x y mat =
  try
    let res = mat.(x).(y) in
    Some res
  with
  | Invalid_argument _ -> None
;;

let char_digit c = int_of_char c - int_of_char '0'
let sum_of_array arr = Array.fold_left ( + ) 0 arr
let sum_of_list list = List.fold_left ( + ) 0 list
let is_digit c = c >= '0' && c <= '9'

let get_digit_str str =
  str
  |> String.to_seq
  |> Seq.filter (fun c -> is_digit c)
  |> String.of_seq
  |> int_of_string
;;
