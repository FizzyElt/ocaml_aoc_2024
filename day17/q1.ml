let get_operands num register =
  match num with
  | 0 | 1 | 2 | 3 -> num
  | 4 -> register.(0)
  | 5 -> register.(1)
  | 6 -> register.(2)
  | _ -> failwith "invalid operands"
;;

(* 0 *)
let adv (operand : int) register =
  let a_register = register.(0) in
  let combo = 2.0 ** float_of_int operand |> int_of_float in
  let result = a_register / combo in

  register.(0) <- result;
  ()
;;

(* 1 *)
let bxl (operand : int) register =
  let b_register = register.(1) in
  let result = b_register lxor operand in
  register.(1) <- result;
  ()
;;

(* 2 *)
let bst (operand : int) register =
  let result = if operand < 0 then (operand mod 8) + 8 else operand mod 8 in
  register.(1) <- result;
  ()
;;

(* 3 *)
let jnz (operand : int) register =
  let a_register = register.(0) in
  if a_register = 0 then None else Some operand
;;

(* 4 *)
let bxc register =
  let b_register = register.(1) in
  let c_register = register.(2) in
  let result = b_register lxor c_register in
  register.(1) <- result;
  ()
;;

(* 5 *)
let out (operand : int) =
  let result =
    string_of_int (if operand < 0 then (operand mod 8) + 8 else operand mod 8)
  in
  result
;;

(* 6 *)
let bdv (operand : int) register =
  let a_register = register.(0) in
  let combo = 2.0 ** float_of_int operand |> int_of_float in
  let result = a_register / combo in
  register.(1) <- result;
  ()
;;

(* 7 *)
let cdv (operand : int) register =
  let a_register = register.(0) in
  let combo = 2.0 ** float_of_int operand |> int_of_float in
  let result = a_register / combo in
  register.(2) <- result;
  ()
;;

let process program registers =
  let len = Array.length program in
  let output = ref [] in
  let rec loop n registers =
    if n >= len - 1
    then ()
    else (
      let instruction = program.(n) in
      let operand = program.(n + 1) in
      let combo = get_operands operand registers in
      match instruction with
      | 0 ->
        adv combo registers;
        loop (n + 2) registers
      | 1 ->
        bxl operand registers;
        loop (n + 2) registers
      | 2 ->
        bst combo registers;
        loop (n + 2) registers
      | 3 ->
        (match jnz operand registers with
         | Some n -> loop n registers
         | None -> loop (n + 2) registers)
      | 4 ->
        bxc registers;
        loop (n + 2) registers
      | 5 ->
        output := out combo :: !output;
        loop (n + 2) registers
      | 6 ->
        bdv combo registers;
        loop (n + 2) registers
      | 7 ->
        cdv combo registers;
        loop (n + 2) registers
      | _ -> failwith "invalid instruction")
  in
  loop 0 registers;
  !output |> List.rev |> String.concat ","
;;

let () =
  (* 2; 4; 1; 1; 7; 5; 1; 5; 4; 0; 0; 3; 5; 5; 3; 0 *)
  let program = [| 2; 4; 1; 1; 7; 5; 1; 5; 4; 0; 0; 3; 5; 5; 3; 0 |] in
  let _register = [| 64196994; 0; 0 |] in
  let register = [| 16312245875300; 0; 0 |] in
  let res = process program register in
  Printf.printf "%s" res
;;
