(* Your goal is to fill in the “...”. The first “…” should be identical to the book in chapter 10. The
second should be to handle the new language expressions “Or”, “Not”, and “IfThenElse”.
Note: since our toy language only has the type “int”, we are using an encoding in which “0”
means “false” and anything else means “true”. This encoding is also used in some real
programming languages such as C, Java, etc *)

type expr =
| Num of int
| Add of expr * expr
| Sub of expr * expr
| Mult of expr * expr
| Div of expr * expr
| And of expr * expr
| Or of expr * expr
| Not of expr
| IfThenElse of expr * expr * expr;;

let rec evaluate e = match e with
| Num x -> x
| Add (e1, e2) -> evaluate e1 + evaluate e2
| Sub (e1, e2) -> evaluate e1 - evaluate e2
| Mult (e1, e2) -> evaluate e1 * evaluate e2
| Div (e1, e2) -> evaluate e1 / evaluate e2
| And (e1, e2) ->
if evaluate e1 <> 0 &&
evaluate e2 <> 0
then 1 else 0
| Or (e1, e2) ->
  if evaluate e1 <> 0 ||
    evaluate e2 <> 0
  then 1 else 0
| Not e1 -> 
  if evaluate e1 = 0 then 1 else 0
| IfThenElse (cond, e1, e2) ->
  if evaluate cond <> 0 then evaluate e1 else evaluate e2
;;

let expr1 = Add (Num 1, Num 2)             (* 1 + 2 *)
let expr2 = IfThenElse (Num 0, Num 1, Num 2) (* if 0 then 1 else 2 *)
let expr3 = And (Num 1, Num 0)             (* 1 and 0 *)
let expr4 = Or (Num 1, Num 0)              (* 1 or 0 *)
let expr5 = Not (Num 0)                    (* not 0 *)

let () =
  print_endline (string_of_int (evaluate expr1)); (* Output: 3 *)
  print_endline (string_of_int (evaluate expr2)); (* Output: 2 *)
  print_endline (string_of_int (evaluate expr3)); (* Output: 0 *)
  print_endline (string_of_int (evaluate expr4)); (* Output: 1 *)
  print_endline (string_of_int (evaluate expr5)); (* Output: 1 *)
;;

(* (2 marks of extra credit) Please implement fold_right using fold_left:
let fold_right = ... fold_left ...
Note: this is a difficult problem. To get credit you should only use “fold_left” once, and
(other than within the standard definition of fold_left) you should not use “let rec”
.
That said, you have been given all of the tools required. Good luck! *)

(* let fold_right = ... List.fold_left ...;; *)

(* fold_left definition *)
let rec fold_left f acc = function
  | [] -> acc
  | h :: t -> fold_left f (f acc h) t
;;

let rec fold_right f acc = function
  | [] -> acc
  | h :: t -> f h (fold_right f t acc);;

let list_length l =
  List.fold_left (fun acc _ -> acc + 1) 0 l


let reverse lst =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: acc) t
  in 
  aux [] lst
;;

(* implementation *)
let fold_right f lst acc =
  let reversed_lst = reverse lst in
  fold_left f acc reversed_lst
;;

let () =
  let rst = fold_right (+) [1;2;3;4] 0 in
  Printf.printf "The result is: %d\n" rst
;;


(* (1 mark) Suppose you have a list. Please write the function partition : ‘a -> ‘a list
-> (‘a list * ‘a list) that divides the input list into two output lists, the first of
which is all of the items less than or equal to the first argument and the second of which is all of
the items strictly greater than the first argument. For example:
# partition 4 [3;5;7;2;1;3;9];;
- : int list * int list = ([3; 2; 1; 3], [5; 7; 9])
You do not have to worry about the order of items within the output li *)

let partition n lst =
  let rec aux (l, r) = function
    | [] -> (l, r)
    | h::t -> if h <= n then aux (h::l, r) t 
      else aux (l, h::r) t
  in
  aux ([], []) lst
;;

let print_int_list lst = 
  let rec aux lst = 
    match lst with
    | [] -> ()
    | [x] -> Printf.printf "%d" x
    | h::t -> print_int h; print_string " "; aux t
  in
  Printf.printf "[";
  aux lst;
  Printf.printf "]"
;;

let print_int_list_tuple (lst1, lst2) = 
  Printf.printf "(";
  print_int_list lst1;
  Printf.printf ", ";
  print_int_list lst2;
  Printf.printf ")\n"
;;

let () =
  let result = (partition 4 [3;5;7;2;1;3;9])
in 
print_int_list_tuple result
;;

(* (1 mark) Write a function “remove” that removes all occurrences of a given element from a list.
(1 mark) Consider the abstract data type of a “set”, with the following operations:
type 'a set = ...
let empty_set : 'a set = ...
let singleton : 'a -> 'a set = ...
let element_of : 'a -> 'a set -> bool = ...
let union : 'a set -> 'a set -> 'a set = ...
let intersection : 'a set -> 'a set -> 'a set = ...
Please fill in the “...” above with an implementation. In case the names are not clear, the
operations should do the following:
• “singleton x” creates a new set with just the element x in it, i.e. {x}
• “element_of x S” tests whether x is in the set S
• “union S1 S2” returns a set with all of the elements in S1 or S2 (or in both). Elements
that are in both sets should only appear once in the union, not multiple times.
“intersection S1 S2” returns a set with all of the elements in both S1 and S2 *)