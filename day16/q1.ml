open Lib
open Lib.Pair

let directions = [| -1, 0; 0, 1; 1, 0; 0, -1 |]
let end_point = 0, 0

let bfs grid =
  let lowest_score = ref Int.max_int in
  let queue = Queue.create () in
  let rec while_loop queue =
    if Queue.is_empty queue
    then ()
    else (
      let (row, col), dir, score = Queue.pop queue in
      if score > !lowest_score
      then while_loop queue
      else if end_point = (row, col)
      then (
        lowest_score := min !lowest_score score;
        while_loop queue)
      else (
        directions
        |> Array.iter (fun delta ->
          let new_row, new_col = add_pair (row, col) delta in

          if grid.(new_row).(new_col) = '#'
          then ()
          else if add_pair dir delta = (0, 0)
          then ()
          else (
            let cost = score + if sub_pair delta dir = (0, 0) then 1 else 1001 in
            ()));
        while_loop queue))
  in
  while_loop queue;
  ()
;;

let () = ()
