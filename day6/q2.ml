open Lib

module IntPairs = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match Stdlib.compare x0 x1 with
    | 0 -> Stdlib.compare y0 y1
    | c -> c
  ;;
end

module PairsSet = Set.Make (IntPairs)

type direction =
  | Up
  | Down
  | Left
  | Right

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

let rec find_row (x, y) board offset =
  try
    let c = board.(x + offset).(y) in
    if c = '#' then Some (x, y) else find_row (x + offset, y) board offset
  with
  | Invalid_argument _ -> None
;;

let rec find_col (x, y) board offset =
  try
    let c = board.(x).(y + offset) in
    if c = '#' then Some (x, y) else find_col (x, y + offset) board offset
  with
  | Invalid_argument _ -> None
;;

let get_next_turn_pos (x, y) board dir =
  match dir with
  | Up -> find_row (x, y) board (-1)
  | Down -> find_row (x, y) board 1
  | Left -> find_col (x, y) board (-1)
  | Right -> find_col (x, y) board 1
;;

let is_loop (origin_x, origin_y) board dir =
  let rec loop (x, y) dir set =
    let next_dir = turn_right dir in
    match get_next_turn_pos (x, y) board next_dir with
    | Some (next_x, next_y) ->
      if (next_x, next_y) = (x, y)
      then false
      else if List.exists (fun (a, b) -> (a, b) = (next_x, next_y)) set
      then true
      else loop (next_x, next_y) next_dir ((next_x, next_y) :: set)
    | None -> false
  in
  match get_next_turn_pos (origin_x, origin_y) board dir with
  | Some (x, y) -> loop (x, y) dir [ x, y ]
  | None -> false
;;

let rec move (x, y) dir board visited =
  let m_x, m_y = next_pos (x, y) dir in
  try
    let c = board.(m_x).(m_y) in
    if c = '#'
    then move (x, y) (turn_right dir) board visited
    else (
      let new_visited = PairsSet.add (m_x, m_y) visited in
      move (m_x, m_y) dir board new_visited)
  with
  | Invalid_argument _ -> visited
;;

let check_all_move_loop (x, y) dir board = is_loop (x, y) board dir

let find_guard board =
  let x = ref 0 in
  let y = ref 0 in
  board
  |> Array.iteri (fun row line ->
    line
    |> Array.iteri (fun col c ->
      if c = '^'
      then (
        x := row;
        y := col)));
  !x, !y
;;

let result (list : string list) =
  let board =
    list |> List.map (fun line -> line |> String.to_seq |> Array.of_seq) |> Array.of_list
  in
  let guard_pos = find_guard board in
  let visited =
    move guard_pos Up board PairsSet.empty
    (* [ fst guard_pos, snd guard_pos, Up
      ; fst guard_pos, snd guard_pos, Down
      ; fst guard_pos, snd guard_pos, Left
      ; fst guard_pos, snd guard_pos, Right
      ] *)
  in
  visited
  |> PairsSet.elements
  |> List.fold_left
       (fun acc (x, y) ->
          if guard_pos = (x, y)
          then acc
          else (
            board.(x).(y) <- '#';
            let loop = check_all_move_loop guard_pos Up board in
            board.(x).(y) <- '.';
            if loop
            then
              (* Printf.printf "%d, %d\n" x y; *)
              acc + 1
            else acc))
       0
;;

let () = Sys.argv.(1) |> File.read_list_of_line Fun.id |> result |> print_int
