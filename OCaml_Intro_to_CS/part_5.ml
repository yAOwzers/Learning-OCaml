(* part 1 *)
(* 
Write a function
that allows such a reverse lookup using the list-of-pairs-based dictionaries from chapter 8. That
is, write reverse_lookup : 'a -> ('b * 'a) list -> 'b such that e.g.
# let dict = [(1, 2) ; (3, 4) ; (5, 8)];;
val dict : (int * int) list = [(1, 2); (3, 4); (5, 8)]
# reverse_lookup 4 
*)

let rec reverse_lookup num = function
  | [] -> 0
  | (x, y) :: t -> if num = y then x else reverse_lookup num t
;;

let dict = [(1, 2) ; (3, 4); (5, 8);];;

let () = 
  Printf.printf "Value of 4: %d\n" (reverse_lookup 4 dict);;

let () = 
  Printf.printf "Value of 2: %d\n" (reverse_lookup 2 dict);;

let () = 
  Printf.printf "Value of 1: %d\n" (reverse_lookup 1 dict);;


(* part 2
   (1 mark) Use fold_left or fold_right (previously cunningly called meld_left and
meld, respectively) to (re-)implement
filter : (‘a -> bool) -> ‘a list -> ‘a list
That is, you should write something like:
let filter f l = fold_left ...
or
let filter f l = fold_right ...
Make sure you use the correct fold… among other things, the following example should work:
# filter (fun n -> n mod 2 = 1) [1 ; 2 ; 3 ; 4 ; 5];;
- : int list = [1; 3; 5]
*)

let filter f l = 
  let validation acc x =
    if f x then x :: acc
    else acc
  in
  List.fold_left validation [] l
;;

let print_list lst = 
  let str = String.concat ", " (List.map string_of_int lst) in
  Printf.printf "[%s]\n" str
;;

let result = filter (fun n -> n mod 2 = 1) [1 ; 2 ; 3 ; 4 ; 5];;

Printf.printf "Result: %s\n" (String.concat ", " (List.map string_of_int result))
;;
(* 
   (1 mark, plus 1 mark of extra credit) Please write flatten : ‘a list list -> ‘a
list that flattens lists of lists into single lists along the line of the following example:
# flatten [[1;3;5];[2;4;6];[-1;-2;-3];[0];[3;1];[4;1;5;1]]
- : int list = [1; 3; 5; 2; 4; 6; -1; -2; -3; 0; 3; 1; 4; 1; 5; 1]
Preserve the order of items in the list. Moreover, your solution should run in O(n) time, where n
is the total number of items in the list of lists. To get this runtime, be careful to put everything
together in the right order.
(1 mark of extra credit) your implementation for flatten should be tail recursive.
*)

(*
let flattern lst =
  let rec aux acc = function
    | [] -> acc
    | [] :: t -> aux acc t
    | (h :: t) :: t' -> aux (h :: acc) (t @ t')
  in
  List.rev (aux [] lst)
;;

let result = flatten [[1;3;5];[2;4;6];[-1;-2;-3];[0];[3;1];[4;1;5;1]];;

let rec print_list = function
  [] -> ()
  | e :: l -> print_int e; print_string " " ; print_list l
;;

print_list result;;
*)


(*
(2 marks) Write complex_paren_match : char list -> bool that takes a list of
“complex” parentheses – that is, beyond “(“ and “)” also including two additional opening
parentheses “[“ and “{“ and their matching closing parentheses “]” and “}” – and computes
whether all of the parentheses are correctly matched. If you are given a list that contains a
character that is not a parenthesis you should raise an exception. You may find the following
tests useful to think about (the first two should fail while the last one passes, and yes I have
other secret tests):
let test1 = ['['; '('; ']'; ')'];;
let test2 = ['('; ')'; '['; ')'];;
let test3 = ['['; '{'; '}'; '('; ')'; ']'; '('; '{'; '}';
'['; ']'; ')';];;
*)

exception InvalidParenthesis of char

let complex_paren_match lst =
  let is_opening = function 
    | '(' | '{' | '[' -> true
    | _ -> false
  in
  let is_closing = function
    | ')' | '}' | ']' -> true
    | _ -> false
  in
  let matching_pair = function
    | '[' -> ']'
    | '(' -> ')'
    | '{' -> '}'
    | _ -> failwith "Invalid Parenthesis" 
  in
  let rec aux stack = function
    | [] -> List.length stack = 0
    | h :: t ->
        if is_opening h then
          aux (h :: stack) t
        else if is_closing h then
          match stack with
          | [] -> false
          | top :: rest -> 
              if h = matching_pair top then
                  aux rest t
                else
                  false
        else
          raise (InvalidParenthesis h) 
  in
  aux [] lst
;;

let test1 = ['['; '('; ']'; ')'];;
let test2 = ['('; ')'; '['; ')'];;
let test3 = ['['; '{'; '}'; '('; ')'; ']'; '('; '{'; '}'; '['; ']'; ')';];;

let () =
  print_endline (string_of_bool (complex_paren_match test1));  
  print_endline (string_of_bool (complex_paren_match test2)); 
  print_endline (string_of_bool (complex_paren_match test3));
;;
