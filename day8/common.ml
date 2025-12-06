module CharMap = Map.Make (Char)

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

let sub_pair (x0, y0) (x1, y1) = x0 - x1, y0 - y1
let add_pair (x0, y0) (x1, y1) = x0 + x1, y0 + y1
