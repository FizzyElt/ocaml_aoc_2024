open Lib

let repeat n c = List.init n (fun _ -> c)

(* index_num, start, end *)
let create_file_blocks (list : int list) =
  let rec loop list start n is_free acc =
    match list with
    | [] -> acc
    | c :: cs ->
      if is_free
      then loop cs (start + c) n (not is_free) ((-1, start, start + c) :: acc)
      else loop cs (start + c) (n + 1) (not is_free) ((n, start, start + c) :: acc)
  in
  loop list 0 0 false []
;;

let flat_blocks (list : (int * int * int) list) =
  list
  |> List.fold_left
       (fun acc (index, start_index, end_index) ->
          repeat (end_index - start_index) index @ acc)
       []
;;

let find_first_slot arr (range_start, range_end) count =
  let rec loop i start_at c =
    if c = count
    then start_at
    else if i >= range_end
    then -1
    else if arr.(i) = -1
    then loop (i + 1) (i - c) (c + 1)
    else loop (i + 1) (-1) 0
  in
  loop range_start (-1) 0
;;

let set_blocks (blocks : int array) (n, s, e) =
  let rec loop i blocks =
    if i >= e
    then blocks
    else (
      blocks.(i) <- n;
      loop (i + 1) blocks)
  in
  loop s blocks
;;

let move_blocks (blocks : int array) (file_blocks : (int * int * int) list) =
  file_blocks
  |> List.iter (fun (i, s, e) ->
    if i != -1
    then (
      let count = e - s in
      let start_at = find_first_slot blocks (0, s) count in
      if start_at != -1
      then (
        let _ = set_blocks blocks (i, start_at, start_at + count) in
        let _ = set_blocks blocks (-1, s, e) in
        ())));

  blocks
;;

let result str =
  let blocks =
    create_file_blocks
      (str
       |> String.to_seq
       |> Seq.map (fun c -> int_of_char c - int_of_char '0')
       |> List.of_seq)
  in
  let flatted_blocks = flat_blocks blocks |> Array.of_list in

  let moved_blocks = move_blocks flatted_blocks blocks in

  moved_blocks
  |> Array.to_seq
  |> Seq.fold_lefti (fun acc i c -> if c = -1 then acc else acc + (c * i)) 0
;;

let () =
  let file = Sys.argv.(1) in
  let str = File.read_file file in
  result str |> print_int
;;
