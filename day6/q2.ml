open Core
open Lib

module IntTripleComparator = struct
  module T = struct
    type t = int * int * int

    let compare (a1, b1, c1) (a2, b2, c2) =
      match Int.compare a1 a2 with
      | 0 ->
        (match Int.compare b1 b2 with
         | 0 -> Int.compare c1 c2
         | other -> other)
      | other -> other
    ;;

    let sexp_of_t (a, b, c) =
      Sexp.List [ Int.sexp_of_t a; Int.sexp_of_t b; Int.sexp_of_t c ]
    ;;

    let t_of_sexp = function
      | Sexp.List [ a; b; c ] -> Int.t_of_sexp a, Int.t_of_sexp b, Int.t_of_sexp c
      | _ -> failwith "Invalid S-expression for int * int * int"
    ;;
  end

  include T
  include Comparator.Make (T)
end

module IntTripleSet = Set.Make (IntTripleComparator)

let directions = [| -1, 0; 0, 1; 1, 0; 0, -1 |]
let next_direction dir = if dir >= 3 then 0 else dir + 1
let add_pairs (x1, y1) (x2, y2) = x1 + x2, y1 + y2

let find_char_coordinates search_char grid =
  let find_row row_index row =
    let row_length = String.length row in
    let rec find_col col_index acc =
      if col_index >= row_length
      then acc
      else (
        let acc =
          if Char.equal row.[col_index] search_char
          then (row_index, col_index) :: acc
          else acc
        in
        find_col (col_index + 1) acc)
    in
    find_col 0 []
  in
  let rec find_in_grid row_index rows acc =
    match rows with
    | [] -> List.rev acc
    | row :: rest ->
      let row_coords = find_row row_index row in
      find_in_grid (row_index + 1) rest (row_coords @ acc)
  in
  find_in_grid 0 grid []
;;

let char_grid_to_string_grid charr =
  Array.to_list
    (Array.map charr ~f:(fun arr -> String.init (Array.length arr) ~f:(Array.get arr)))
;;

let find_guard map =
  let rows = Array.length map in
  let cols = Array.length map.(0) in
  let rec search_row r =
    if r >= rows
    then 0, 0
    else (
      let rec search_col c =
        if c >= cols
        then None
        else if Char.equal map.(r).(c) '^'
        then Some (r, c)
        else search_col (c + 1)
      in
      match search_col 0 with
      | None when r + 1 < rows -> search_row (r + 1)
      | None -> 0, 0
      | Some pos -> pos)
  in
  search_row 0
;;

let rec loopsearch map (gx, gy) direction (prepos : IntTripleSet.t) =
  let nx, ny = add_pairs (gx, gy) directions.(direction) in
  let current_position = gx, gy, direction in
  if nx >= Array.length map || ny >= Array.length map.(0) || nx < 0 || ny < 0
  then 0
  else if Set.mem prepos current_position
  then 1
  else if Char.equal map.(nx).(ny) '#'
  then loopsearch map (gx, gy) (next_direction direction) prepos
  else (
    let updated_prepos = Set.add prepos current_position in
    loopsearch map (nx, ny) direction updated_prepos)
;;

let rec move_guard map (gx, gy) direction =
  let nx, ny = add_pairs (gx, gy) directions.(direction) in
  let _ = map.(gx).(gy) <- 'X' in
  if nx >= Array.length map || ny >= Array.length map.(0) || nx < 0 || ny < 0
  then map
  else if Char.equal map.(nx).(ny) '#'
  then move_guard map (gx, gy) (next_direction direction)
  else move_guard map (nx, ny) direction
;;

let construct_char_grid (s : string list) =
  List.map s ~f:(fun s -> Stdlib.String.to_seq s |> Stdlib.Array.of_seq) |> Array.of_list
;;

let gen_blocker_maps (map : char array array) =
  let newblockers = find_char_coordinates 'X' (char_grid_to_string_grid map) in
  List.map newblockers ~f:(fun (x, y) ->
    let blockedmap = Array.map ~f:(fun map -> Array.copy map) map |> Array.copy in
    let _ = blockedmap.(x).(y) <- '#' in
    blockedmap)
;;

let result list =
  let map = construct_char_grid list in
  let guardstart = find_guard map in
  let newmap = move_guard map guardstart 0 in
  let _ = newmap.(fst guardstart).(snd guardstart) <- '^' in
  let blocked_maps = gen_blocker_maps newmap in
  let res =
    List.map blocked_maps ~f:(fun map ->
      let prepos = IntTripleSet.empty in
      loopsearch map guardstart 0 prepos)
  in
  List.fold_left res ~init:0 ~f:(fun acc i -> acc + i)
;;

let () =
  let args = Sys.get_argv () in
  let file = args.(1) in
  let list = File.read_list_of_line Fun.id file in
  let res = result list in
  print_int res
;;
