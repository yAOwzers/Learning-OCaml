(* part 1 *)

(* borrowing dict tree from part 9 *)
type ('k, 'v) dict_tree = 
  | Lf
  | Br of int * 'k * 'v option * ('k, 'v) dict_tree * ('k, 'v) dict_tree
;;

let rec height (t : ('k, 'v) dict_tree) =
  match t with
  | Lf -> 0
  | Br (h, _, _, _, _) -> h
;;

let max v1 v2 = 
  if v1 > v2 then v1
  else v2
;;

let update_height = function
  | Lf -> 0
  | Br (_, _, _, l, r) -> 1 + max (height l) (height r)
;;

(* Should take O(log m) time on wishy-washy average *)
let rec lookup (tr : ('k, 'v) dict_tree) k = 
  match tr with
  | Lf -> None
  | Br (_, k', v, l, r) -> 
    if k = k' then v (*found key*)
    else if k < k' then lookup l k (*go left since smaller*)
    else lookup r k
  ;;


(* Should take O(log m) time on wishy-washy average *)
let rec insert (tr : ('k, 'v) dict_tree) k v =
  match tr with
  | Lf -> Br (1, k, Some v, Lf, Lf) (* insert at the end *)
  | Br (_, k', v', l, r) -> 
    if k = k' then Br (height tr, k, Some v, Lf, Lf)
    else if k < k' then 
      let new_left = insert l k v in
      let new_height = update_height (Br (0, k', v', new_left, r)) in
      Br (new_height, k', v', insert l k v, r)
    else 
      let new_right = insert r k v in
      let new_height = update_height (Br (0, k', v', l, new_right)) in
      Br (new_height, k', v', l, new_right)
;;

let rec print_tree ?(indent="") = function
  | Lf -> Printf.printf "%sLf\n" indent
  | Br (_, key, value, left, right) ->
      Printf.printf "%sBr (key: %s, value: %s)\n" indent (string_of_int key) 
        (match value with
         | Some v -> v
         | None -> "None");
      let new_indent = indent ^ "  " in
      print_tree ~indent:new_indent left;
      print_tree ~indent:new_indent right
  ;;

let cache f =
  let ref_dict_tree = ref Lf in
  fun n ->
    match lookup !ref_dict_tree n with
    | Some value -> value
    | None ->
      let f_res = f n in
      ref_dict_tree := insert !ref_dict_tree n f_res;
      f_res
    ;;

let rec fib n = if n = 0 || n = 1 then 1 else fib (n-1) + fib (n-2)

let cached_fib = cache fib;;

    
let () =
  Printf.printf "fib 35: %d\n" (cached_fib 35);
  Printf.printf "fib 40: %d\n" (cached_fib 40);
  (* Second time for each value will be instant! *)
  Printf.printf "fib 35: %d\n" (cached_fib 35);
  Printf.printf "fib 40: %d\n" (cached_fib 40);
;;


(* part 2 *)

let array_fold_left f acc arr =
  let rec aux acc idx = 
    if idx < Array.length arr then
      let new_acc = f acc arr.(idx) in
      aux new_acc (idx + 1)
    else 
      acc
    in
    aux acc 0
  ;;


let sum_array arr = 
  let sum = array_fold_left ( + ) 0 arr in
  Printf.printf "Sum of array elements: %d\n" sum;
  sum
;;

let example_array = [|1; 2; 3; 4; 5|];;
let result = sum_array example_array;;

let array_fold_right f arr acc = 
  let rec aux acc idx =
    if idx < 0 then
      acc
    else
      aux (f acc arr.(idx)) (idx-1)
    in
    aux acc ((Array.length arr) - 1)
  ;;

let sum_array_right arr = 
    let sum = array_fold_right ( + ) arr 0 in
    Printf.printf "Sum of array elements: %d\n" sum;
    sum
  ;;
  
  let right_example_array = [|1; 2; 3; 4; 5|];;
  let right_result = sum_array_right right_example_array;;
  