type sort =
  | Non
  | Increasing
  | Decreasing

let parse_line line = line |> String.split_on_char ' ' |> List.map int_of_string
let increasing_or_decreasing a b = if a > b then Increasing else Decreasing

let is_in_range a b =
  let diff = a - b in
  abs diff <= 3 && abs diff > 0
;;

let is_safe list =
  let rec loop prev list =
    match prev, list with
    | _, [ _ ] | _, [] -> true
    | Non, a :: b :: tl ->
      if is_in_range a b
      then (
        let t = increasing_or_decreasing a b in
        loop t (b :: tl))
      else false
    | Increasing, a :: b :: tl ->
      let t = increasing_or_decreasing a b in
      if t = Increasing && is_in_range a b then loop t (b :: tl) else false
    | Decreasing, a :: b :: tl ->
      let t = increasing_or_decreasing a b in
      if t = Decreasing && is_in_range a b then loop t (b :: tl) else false
  in
  loop Non list
;;
