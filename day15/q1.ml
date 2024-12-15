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

let find_slot (x, y) (offset : int * int) (grid : char array array) =
  let rec loop (x, y) =
    match get_ele_by_matrix y x grid with
    | Some c when c = '.' -> Some (x, y)
    | Some c when c = '#' -> None
    | Some c when c = 'O' -> loop (add_pair (x, y) offset)
    | _ -> None
  in
  loop (add_pair (x, y) offset)
;;

let move (x, y) (offset : int * int) (grid : char array array) =
  let fish = grid.(y).(x) in
  let next_pos = add_pair (x, y) offset in
  match find_slot (x, y) offset grid with
  | Some (slot_x, slot_y) ->
    if (slot_x, slot_y) = next_pos
    then (
      (* @.O -> .@O *)
      grid.(slot_y).(slot_x) <- fish;
      grid.(y).(x) <- '.';
      slot_x, slot_y)
    else (
      (* @OO. -> .@OO *)
      grid.(slot_y).(slot_x) <- 'O';
      grid.(snd next_pos).(fst next_pos) <- fish;
      grid.(y).(x) <- '.';
      next_pos)
  | None -> x, y
;;

let process (grid : char array array) (fish_x, fish_y) (steps : char list) =
  let rec loop steps fish =
    match steps with
    | [] -> ()
    | h :: t ->
      let dir = direction h in
      let new_fish = move fish dir grid in
      loop t new_fish
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

let result (input : string) =
  let grid, steps = parse_input input in
  let fish_coord = find_fish grid in
  let new_grid = process grid fish_coord steps in
  print_grid new_grid;
  let res = ref 0 in
  new_grid
  |> Array.iteri (fun y line ->
    line |> Array.iteri (fun x c -> if c = 'O' then res := !res + (y * 100) + x));
  !res
;;

let () =
  let file = Sys.argv.(1) in
  let input = File.read_file file in
  let res = result input in
  Printf.printf "%d\n" res
;;
