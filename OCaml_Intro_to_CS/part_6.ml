(*
Suppose you have two functions f : ‘a -> ‘b and g :
‘c -> ‘a. Define their functional composition as follows:
  *)

let compose f g = fun x -> f (g x);;

let inc x = x + 1;;

let double x = x + x;;

let h = compose inc double;;

let result = h 5 ;;

let () =
  Printf.printf "result of compose inc double 5: %d\n" result;; 

(*
Please implement the compose_list function. Note: if the list is empty,
then please return the argument (that is, be an identity function). In other words:
# compose_list [] 7;;
- : int = 7
*)

let dec x = x - 1;;

let compose_list lst x =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> h (aux acc t)
  in
  aux x lst
;;

let operations = [inc ; double ; dec];;

(* expected should be 13 *)

let () = 
  let res = compose_list operations 7 in
  Printf.printf "Result of compose_list operations: %d\n" res;

  let empty_res = compose_list [] 7 in
  Printf.printf "Result of compose_list operations with empty array: %d\n" empty_res;

  let acc_res_zero = compose_list [(fun x -> 0)] 7 in
  Printf.printf "Result of compose_list operations with func to zero: %d\n" acc_res_zero;;

(* for the extra 1 mark, using fold_left and fold_right *)

let rec fold_left f acc lst =
  match lst with
  | [] -> acc
  | h :: t -> fold_left f (f acc h) t
;;

let rec fold_right f lst acc =
  match lst with
  | [] -> acc
  | h :: t -> f h (fold_right f t acc)
;;

let reverse lst =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: acc) t
  in
  aux [] lst
;;

let compose_list_fl func_lst x =
  let f = fun x -> x in
  let composed_func = fold_left compose f func_lst in
  composed_func x
;;

let fl_res = compose_list_fl operations 7;;

let () = 
  Printf.printf "Result of compose_list_fl operations: %d\n" fl_res;;

let compose_list_fr func_lst x =
  let f = fun x -> x in
  let composed_func = fold_right compose func_lst f in
  composed_func x
;;

let fr_res = compose_list_fr operations 7;;

let () = 
  Printf.printf "Result of compose_list_fr operations: %d\n" fr_res;;

(*
Your task: implement mapi : (int -> 'a -> 'b) -> 'a list -> 'b list.
  *)

let print_list lst = 
  let str = String.concat ", " (List.map string_of_int lst) in
  Printf.printf "[%s]\n" str
;;

(* let print_list lst =
  List.iter (Printf.printf "%d\n") lst *)

let mapi f lst =
  let rec aux acc index = function
  | [] -> 
    print_list acc;
    acc
  | h :: t -> 
    Printf.printf "%d\n" h;
    aux ((f index h) :: acc) (index + 1) t
  in
 reverse (aux [] 0 lst)
;; 

let () = 
  let mapi_res = mapi (fun i a -> if i mod 2 = 0 then a + 1 else a - 1) [1;2;3;4;5] in
  print_list mapi_res
;;

(* mapi using fold_left/fold_right *)
let mapi_fl f l =
  let result, _ = fold_left (fun (acc, index) elem -> 
    let new_elem = f index elem in
    (new_elem :: acc, index + 1)
    ) ([], 0) l
  in
  reverse result
;;

let list_length lst = 
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (acc + 1) lst
in
aux 0 lst
;;

let mapi_fr f l = 
  let length = list_length l in
  let result, _ = fold_right (fun elem (acc, index) ->
    let new_elem = f index elem in
    (new_elem :: acc, index - 1)
    ) l ([], length - 1)
  in
  result
;;

let () = 
  let mapi_fl_res = mapi_fl (fun i a -> if i mod 2 = 0 then a + 1 else a - 1) [1;2;3;4;5] in
  print_list mapi_fl_res;

  let mapi_fr_res = mapi_fr (fun i a -> if i mod 2 = 0 then a + 1 else a - 1) [1;2;3;4;5] in
  print_list mapi_fr_res
;;


(*
(2 marks, plus 1 mark of extra credit) Suppose we wish to generate a decent-sized test for the
complex parenthesis matching problem. In particular we want a function generate_tests :
int -> char list list that generates all of the tests of length 2n (where n is the input
argument) that have the following structure. The first n characters should be open parentheses,
and the last n characters should be the appropriate matching closed parentheses. For example,
here is the output for my function when n=3:
*)

let combinations n = 
  let rec aux acc k =
    if k = 0 then acc
    else
      aux (List.concat [
        List.map (fun l -> '(' :: l) acc;
        List.map (fun l -> '{' :: l) acc;
        List.map (fun l -> '[' :: l) acc
      ]) (k - 1)
    in
    aux [[]] n
  ;;

let matching_parenthesis open_parenthesis =
  List.map (function
  | '(' -> ')'
  | '{' -> '}'
  | '[' -> ']'
  | _ -> failwith "Invalid character"
  ) open_parenthesis
;;


let generate_tests n =
  (* first to have a list of all brackets, then run through the various permutations of the brackets in a BFS way *)
  (* in python it will be something like def backtrack(temp, list, result) *)
  let open_combinations = combinations n in
  List.map (fun open_parenthesis ->
    open_parenthesis @ (matching_parenthesis open_parenthesis)
  ) open_combinations
  ;;

(* let string_of_char_list cl =
  String.concat "" (List.map (String.make 1) cl)
;;

let print_combinations n =
  let combs = combinations n in
  List.iter (fun comb -> print_endline (string_of_char_list comb)) combs
;;

print_combinations 3;; *)

let () =
  let tests = generate_tests 3 in 
  Printf.printf "Total number of combinations: %d\n" (List.length tests);
  List.iter (fun test -> 
    List.iter (Printf.printf "%c") test;
    print_newline()
  ) tests
  ;;

(* 
  Part 4
  *)

type 'a set = 'a list;;

let empty_set : 'a set = [];;

let singleton (x : 'a): 'a set = [x];;

let element_of a lst =
  let rec aux = function
    | [] -> false
    | h :: t -> if h = a then true else aux t
  in
  aux lst
;;

let union s1 s2 =
  let result_lst = s1
  in
  let rec aux acc = function
    | [] -> acc
    | h :: t -> if element_of h acc then aux acc t else aux (h :: acc) t
  in
  aux result_lst s2
;;

let intersection s1 s2 =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> if element_of h s2 then aux (h :: acc) t else aux acc t
  in
  aux [] s1
;;

let s1 = [1 ; 2; 3; 4; 5];;
let s2 = [4; 5; 3; 2; 0];;

let union_res = union s1 s2;;
let intersection_res = intersection s1 s2;;

let () = 
  Printf.printf "Union of s1 and s2: ";;
  print_list union_res;;

let () = 
  Printf.printf "Intersection of s1 and s2: ";;
  print_list intersection_res;;

