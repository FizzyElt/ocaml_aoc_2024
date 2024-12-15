open Lib.Util
open Lib.Pair
open Lib

let direction c =
  match c with
  | '^' -> 0, -1
  | 'v' -> 0, 1
  | '>' -> 1, 0
  | '<' -> -1, 0
  | _ -> failwith "invalid direction"
;;

let is_movable_vertical (x, y) (offset : int * int) (grid : char array array) =
  let rec loop (x, y) =
    match get_ele_by_matrix y x grid with
    | Some c when c = '[' ->
      loop (add_pair (x, y) offset) && loop (add_pair (x + 1, y) offset)
    | Some c when c = ']' ->
      loop (add_pair (x, y) offset) && loop (add_pair (x - 1, y) offset)
    | Some c when c = '#' -> false
    | Some c when c = '.' -> true
    | _ -> false
  in
  loop (add_pair (x, y) offset)
;;

let move_vertical (x, y) (offset : int * int) (grid : char array array) =
  let rec loop (prev_x, prev_y) (x, y) =
    match get_ele_by_matrix y x grid with
    | Some c when c = '.' ->
      let prev_value = grid.(prev_y).(prev_x) in
      grid.(y).(x) <- prev_value;
      grid.(prev_y).(prev_x) <- '.'
    | Some c when c = '[' ->
      loop (x, y) (add_pair (x, y) offset);
      loop (x + 1, y) (add_pair (x + 1, y) offset);
      let prev_value = grid.(prev_y).(prev_x) in
      grid.(y).(x) <- prev_value;
      grid.(prev_y).(prev_x) <- '.'
    | Some c when c = ']' ->
      loop (x, y) (add_pair (x, y) offset);
      loop (x - 1, y) (add_pair (x - 1, y) offset);
      let prev_value = grid.(prev_y).(prev_x) in
      grid.(y).(x) <- prev_value;
      grid.(prev_y).(prev_x) <- '.'
    | _ -> ()
  in
  loop (x, y) (add_pair (x, y) offset)
;;

let is_movable_horizontal (x, y) offset grid =
  let rec loop (x, y) =
    match get_ele_by_matrix y x grid with
    | Some c when c = '.' -> true
    | Some c when c = '#' -> false
    | Some c when c = '[' || c = ']' -> loop (add_pair (x, y) offset)
    | _ -> false
  in
  loop (add_pair (x, y) offset)
;;

let move_horizontal (x, y) (offset : int * int) (grid : char array array) =
  let rec loop (prev_x, prev_y) (x, y) =
    match get_ele_by_matrix y x grid with
    | Some c when c = '.' ->
      let prev_value = grid.(prev_y).(prev_x) in
      grid.(y).(x) <- prev_value
    | Some _ ->
      loop (x, y) (add_pair (x, y) offset);
      let prev_value = grid.(prev_y).(prev_x) in
      grid.(y).(x) <- prev_value
    | _ -> ()
  in
  loop (x, y) (add_pair (x, y) offset);
  grid.(y).(x) <- '.'
;;

let process (grid : char array array) (fish_x, fish_y) (steps : char list) =
  let rec loop steps fish =
    match steps with
    | [] -> ()
    | h :: t ->
      let dir = direction h in

      (match h with
       | '^' | 'v' ->
         if is_movable_vertical fish dir grid
         then (
           move_vertical fish dir grid;
           loop t (add_pair fish dir))
         else loop t fish
       | '>' | '<' ->
         if is_movable_horizontal fish dir grid
         then (
           move_horizontal fish dir grid;
           loop t (add_pair fish dir))
         else loop t fish
       | _ -> ())
  in
  loop steps (fish_x, fish_y);

  grid
;;

let find_fish (grid : char array array) =
  let coord = ref (0, 0) in
  grid
  |> Array.iteri (fun y line ->
    line |> Array.iteri (fun x c -> if c = '@' then coord := x, y));
  !coord
;;

let parse_input (input : string) =
  let split_input = Str.split (Str.regexp "\n\n") input in
  let grid =
    List.nth split_input 0
    |> String.split_on_char '\n'
    |> List.map (fun line -> line |> String.to_seq |> Array.of_seq)
    |> Array.of_list
  in
  let steps =
    List.nth split_input 1
    |> String.to_seq
    |> Seq.filter (fun c -> c != '\n')
    |> List.of_seq
  in

  grid, steps
;;

let scale grid =
  grid
  |> Array.map (fun line ->
    line
    |> Array.fold_left
         (fun acc c ->
            let l =
              match c with
              | '.' -> [| '.'; '.' |]
              | 'O' -> [| '['; ']' |]
              | '#' -> [| '#'; '#' |]
              | '@' -> [| '@'; '.' |]
              | _ -> [| c; c |]
            in
            Array.append acc l)
         [||])
;;

let result (input : string) =
  let grid, steps = parse_input input in
  let scale_grid = scale grid in
  let fish_coord = find_fish scale_grid in
  let new_grid = process scale_grid fish_coord steps in
  print_grid new_grid;
  let res = ref 0 in
  new_grid
  |> Array.iteri (fun y line ->
    line |> Array.iteri (fun x c -> if c = '[' then res := !res + (y * 100) + x));
  !res
;;

let () =
  let file = Sys.argv.(1) in
  let input = File.read_file file in
  let res = result input in
  Printf.printf "%d\n" res
;;
