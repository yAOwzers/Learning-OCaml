(* Suppose we have a slow function f : 'a -> 'b, which we may wish to run multiple
times. If our function is purely functional – that is, if it will always produce the same output
when given the same input – then we can speed up subsequent calls by remembering the result.
Please implement cache : ('a -> 'b) -> 'a -> 'b that, when given an input
function f creates a new function that checks to see if f has been called before on a given input
and then if so returns the result immediately (technically, in O(log m) time, where m is the
number of distinct inputs that have been called so far). Hint: reference + dictionary.
For example, suppose we have the (very inefficient) Fibonacci function from assignment 1:
let rec fib n = if n = 0 || n = 1 then 1
else fib (n-1) + fib (n-2);;
Then if we run cache on fib:
let cache_fib = cache fib;;
And then run cache_fib on two slow inputs:
(* These will both take a while... *)
# cache_fib 35
- : int = 14930352
# cache_fib 40
- : int = 165580141
(* Second time for each value will be instant! *)
# cache_fib 35
- : int = 14930352
# cache_fib 40
- : int = 165580141
Additional Hint: you’ll need to play around with the “timing” of when to create a ref cell when
you define cache. When should the ref be created? The solution is short but a little tricky. *)

(* return a reference dictionary *)

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

let ref_dict_tree = ref Lf;;

let cache f n =
  let result = lookup !ref_dict_tree n in
  match result with
  | Some value -> value
  | None -> 
    let f_res = f n in
    ref_dict_tree := insert !ref_dict_tree n f_res;
    f_res
  ;;

let rec fib n = if n = 0 || n = 1 then 1 else fib (n-1) + fib (n-2)

let cached_fib = cache fib

    
let () =
  Printf.printf "fib 35: %d\n" (cached_fib 35);
  Printf.printf "fib 40: %d\n" (cached_fib 40);
  (* Second time for each value will be instant! *)
  Printf.printf "fib 35: %d\n" (cached_fib 35);
  Printf.printf "fib 40: %d\n" (cached_fib 40)
;;