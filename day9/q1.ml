open Lib

let repeat n c = List.init n (fun _ -> c)

let create_file_blocks (list : int list) =
  let rec loop acc list n is_free =
    match list with
    | [] -> acc
    | c :: cs ->
      if is_free
      then loop (acc @ repeat c (-1)) cs n (not is_free)
      else loop (acc @ repeat c n) cs (n + 1) (not is_free)
  in
  loop [] list 0 false
;;

let move_blocks (list : int list) =
  let arr = Array.of_list list in
  let rec loop left right arr =
    if left >= right
    then arr
    else if arr.(left) != -1
    then loop (left + 1) right arr
    else if arr.(right) = -1
    then loop left (right - 1) arr
    else (
      let temp = arr.(right) in
      arr.(right) <- -1;
      arr.(left) <- temp;
      loop (left + 1) (right - 1) arr)
  in
  loop 0 (Array.length arr - 1) arr
;;

let result str =
  let blocks =
    create_file_blocks
      (str
       |> String.to_seq
       |> Seq.map (fun c -> int_of_char c - int_of_char '0')
       |> List.of_seq)
  in
  let moved_blocks = move_blocks blocks in
  moved_blocks
  |> Array.to_seqi
  |> Seq.fold_left (fun acc (i, n) -> if n != -1 then acc + (i * n) else acc) 0
;;

let () =
  let file = Sys.argv.(1) in
  let str = File.read_file file in
  result str |> print_int
;;
