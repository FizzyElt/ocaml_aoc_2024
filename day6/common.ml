type direction =
  | Up
  | Down
  | Left
  | Right

let direction_order = function
  | Up -> 0
  | Down -> 1
  | Left -> 2
  | Right -> 3
;;

let direction_compare d0 d1 = direction_order d0 - direction_order d1

let next_pos (x, y) dir =
  match dir with
  | Up -> x - 1, y
  | Down -> x + 1, y
  | Left -> x, y - 1
  | Right -> x, y + 1
;;

let turn_right dir =
  match dir with
  | Up -> Right
  | Down -> Left
  | Left -> Up
  | Right -> Down
;;

module IntPairs = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match Stdlib.compare x0 x1 with
    | 0 -> Stdlib.compare y0 y1
    | c -> c
  ;;
end

module PairSet = Set.Make (IntPairs)

let get_ele_by_matrix x y mat =
  try
    let res = mat.(x).(y) in
    Some res
  with
  | Invalid_argument _ -> None
;;

let find_guard (grid : char array array) =
  let r = ref 0 in
  let c = ref 0 in
  grid
  |> Array.iteri (fun row line ->
    line
    |> Array.iteri (fun col ch ->
      if ch = '^'
      then (
        r := row;
        c := col)));
  !r, !c
;;
