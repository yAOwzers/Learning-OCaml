(* (2 marks) “Polish notation” is an alternative way of writing down mathematical expressions, in
which the operators go before the operands rather than in between them as normal, e.g.:
Normal: 3 + 4 Polish: + 3 4
One advantage of Polish notation is that parentheses are unnecessary; for example:
Normal: 3 + (4 * 5) Polish: + 3 * 4 5
Normal: (3 + 4) * 5 Polish: * + 3 4 5
Normal: (3 + 4) * ((5 + 6) * 7) Polish: * + 3 4 * + 5 6 7
You can read more on Wikipedia: http://en.wikipedia.org/wiki/Polish_notation.
Recall from chapter 10 the evaluator for arithmetic expressions. Your task today is to do the
same for strings corresponding to Polish notation expressions, encoded as follows:
type pn_token = Num of int | Plus | Minus | Times;;
type pn_string = pn_token list;;
Please write eval : pn_string -> int, which takes a list of Polish notation tokens and
returns the evaluated expression. Here is an example to help you test your solution: *)

(* let exm = [Plus ; Num 5 ; Plus ; Times ; Num 2 ; Num 7 ; Num 9];;
(* exm is essentially: + 5 + * 2 7 9 *)
# eval exm;;
- : int = 28
# 5 + ((2 * 7) + 9);; (* the same in more standard format *)
- : int = 28 *)

let print_stack stack = 
  Printf.printf "Current stack: [";
  List.iter (fun x -> Printf.printf "%d; " x) stack;
  Printf.printf "]\n"
;;

type pn_token = Num of int | Plus | Minus | Times;;
type pn_string = pn_token list;;

let print_token = function
  | Num n -> Printf.printf "Processing token: Num %d\n" n
  | Plus -> Printf.printf "Processsing token: Plus\n"
  | Minus -> Printf.printf "Processing token: Minus\n"
  | Times -> Printf.printf "Processing token: Times\n"
;;

let reverse lst =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: acc) t
in
aux [] lst
;;

  let eval ( tokens : pn_string) : int =
  let rec aux stack = function
    | [] -> (
      match stack with
      | [result] -> result
      | _ -> failwith "Invalid expression"
    )
    | token :: rest ->
      print_stack stack;
      print_token token;
      ( match token with 
      | Num n -> aux (n :: stack) rest
      | Plus ->
      (
        match stack with
        | x :: y :: stack' ->
          aux ((y + x) :: stack') rest
        | _ -> failwith "Invalid expression"
      )
    | Minus ->
      (
        match stack with
        | x :: y :: stack' ->
          aux ((y - x) :: stack') rest
        | _ -> failwith "Invalid expression"
      )
    | Times ->
      (
        match stack with
        | x :: y :: stack' ->
          aux ((y * x) :: stack') rest
        | _ -> failwith "Invalid expression"
      ))
        in
        let reversed_tokens = reverse tokens in
        aux [] reversed_tokens
      ;;

let exm = [Plus ; Num 5 ; Plus ; Times ; Num 2 ; Num 7 ; Num 9];;
let result = eval exm;;

Printf.printf "Result: %d\n" result;;

(* 
(2 marks) Recall the definition of trees:
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;
Write two functions, save_tree : string -> int tree -> unit and
load_tree : string -> int tree that can save and load an arbitrary tree to and
from disk using a specified file. For example:
let a_tree = Br (4, Br (3, Lf, Lf), Br (5, Lf, Lf));;
let b_tree = Br (3, Lf, Br (5, Br (4, Lf, Lf), Lf));;
save_tree "my_a_tree" a_tree;;
let a_tree' = load_tree "my_a_tree";;
save_tree "my_b_tree" b_tree;;
let b_tree' = load_tree "my_b_tree";;
The loaded trees should have exactly the same structure as the saved trees:
# a_tree = a_tree'
- : bool = true
# b_tree = b_tree'
- : bool = true
You may not assume any properties of the trees (e.g., that they are sorted or balanced or etc.). 
*)

type 'a tree =
 | Lf
 | Br of 'a * 'a tree * 'a tree
;;

let a_tree = Br (4, Br (3, Lf, Lf), Br (5, Lf, Lf));;
let b_tree = Br (3, Lf, Br (5, Br (4, Lf, Lf), Lf));;

let rec read_dict () = 
  try
    let i = read_int () in (* read int*)
      if i = 0 then [] else (* if zero, we are done*)
        let name = read_line () in (* otherwise, read a name *)
          (i, name) :: read_dict () (* build a dict entry, fetch another *)
  with
    Failure _ ->
      print_string "This is not a valid integer. Please try again. ";
      print_newline ();
      read_dict ()
      ;;
(* 
let entry_to_channel ch (k, v) =
  output_string ch (string_of_int k);
  output_char ch '\n';
  output_string ch v;
  output_char ch '\n'
;;

let dictionary_to_channel ch d = 
  List.iter (entry_to_channel ch) d
;;

let dictionary_to_file filename dict =
  let ch = open_out filename in
    dictionary_to_channel ch dict;
    close_out ch
;;

dictionary_to_file "file.txt" (read_dict ());; *)

let save_tree (filename : string) (tr :'a tree) =
  let rec save_node ch = function
    | Lf -> output_string ch "Lf\n"
    | Br (x, l, r) ->
      Printf.fprintf ch "Br ( %d ,\n" x;
      save_node ch l;
      save_node ch r;
      (* output_string ch ")\n" *)
    in
    let channel = open_out filename in
    save_node channel tr;
    close_out channel
  ;;

(*   
  let entry_to_channel ch k =
    output_string ch (string_of_int k);
    output_char ch '\n';
  in
  let rec read_tree ch = function
    | Lf -> ()
    | Br (x, l, r) ->
      read_tree ch l;
      entry_to_channel ch x;
      read_tree ch r
  in
  let channel = open_out filename in
  read_tree channel tr;
  close_out channel
;; *)


save_tree "my_a_tree" a_tree;;
save_tree "my_b_tree" b_tree;;

(* let entry_of_channel ch =
  let number = input_line ch in
    let name = input_line ch in
      (int_of_string number, name)
;;

let rec dictionary_of_channel ch = 
  try
    let e = entry_of_channel ch in
      e :: dictionary_of_channel ch
  with
    End_of_file -> []
;;

let dictionary_of_file filename = 
  let ch = open_in filename in
    let dict = dictionary_of_channel ch in
      close_in ch;
      dict
;; *)

(* let list_length lst = 
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (acc + 1) t
in
aux 0 lst
;;

let partition_list l r lst = 
  let rec aux curr acc = function
    | [] -> acc
    | h :: t -> if curr >= l && curr < r then aux (curr + 1) (h :: acc) t else aux (curr + 1) acc t
in
aux 0 [] lst
;;

let index_of lst index =
  let rec aux curr = function
    | [] -> 0
    | h :: t -> if curr = index then h else aux (curr + 1) t
in 
aux 0 lst
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
  Printf.printf "]\n"
;;

let load_tree (filename : string) =
  let entry_of_channel ch = 
    let number = input_line ch in
    (int_of_string number)
  in
  let rec create_preorder_arr ch =
    try
      let e = entry_of_channel ch in
        e :: create_preorder_arr ch
    with
      End_of_file -> []

  in 
  let rec create_tree = function
    | [] -> Lf
    | [x] -> 
      Printf.printf "value of element: %d\n" x;
      Br (x, Lf, Lf)
    | lst ->
      let n = list_length lst in
      Printf.printf "length: %d\n" n;
      let m = n / 2 in
      Printf.printf "middle: %d\n" m;
      let l = partition_list 0 m lst in
      let r = partition_list (m + 1) n lst in
      let x = index_of lst m in
      Printf.printf "x: %d\n" x;
      print_int_list l;
      print_int_list r;
      Br (x, create_tree l, create_tree r)
  in
  let channel = open_in filename in
  let inorder_arr = create_preorder_arr channel in
  close_in channel;
  print_int_list inorder_arr;
  create_tree inorder_arr
  ;; *)


let load_tree (filename : string) : int tree = 
  let channel = open_in filename in

  let rec parse_node () = 
    match input_line channel with
    | "Lf" -> Lf
    | line ->
      match String.split_on_char ' ' line with
      | ["Br"; "("; x; ","] ->
        let value = int_of_string x in
        let l = parse_node () in
        let r = parse_node () in
        Br (value, l, r)
      | _ -> 
        Printf.printf "line: %s\n" line;
        failwith "Invalid tree format"
  in
  let tree = parse_node () in
  close_in channel;
  tree
;;

let print_string_arr arr =
  Array.iter (fun str -> Printf.printf "%s\n" str) arr
;;

let () =
  let value = String.split_on_char ' ' "Br (4," in
  print_string_arr (Array.of_list value)
;;

let a_tree' = load_tree "my_a_tree";;
let b_tree' = load_tree "my_b_tree";;

let rec print_tree = function
  | Lf -> ()
  | Br (value, left, right) ->
    print_string "Br (";
    print_int value;
    print_string ", ";
    print_tree left;
    print_string ", ";
    print_tree right;
    print_string ")"
  ;;

let () =
  print_tree a_tree';
  print_newline ();
  print_tree b_tree';
  print_newline ()
;;

let test_tree = Br (16,
Br (8,
Br (4, Br (2, Br (1, Lf, Lf), Br (3, Lf, Lf)),
Br (6, Br (5, Lf, Lf), Br (7, Lf, Lf))),
Br (12, Br (10, Br (9, Lf, Lf), Br (11, Lf, Lf)),
Br (14, Br (13, Lf, Lf), Br (15, Lf, Lf)))),
Br (24,
Br (20, Br (18, Br (17, Lf, Lf), Br (19, Lf, Lf)),
Br (22, Br (21, Lf, Lf), Br (23, Lf, Lf))),
Br (28, Br (26, Br (25, Lf, Lf), Br (27, Lf, Lf)),
Br (30, Br (29, Lf, Lf), Br (31, Lf, Lf)))))
;;

save_tree "test_tree" test_tree;;
let test_tree' = load_tree "test_tree";;

let () =
  Printf.printf "a tree = 'a tree: %s\n" (if a_tree = a_tree' then "true" else "false");
  Printf.printf "b tree = 'b tree: %s\n" (if b_tree = b_tree' then "true" else "false");
  Printf.printf "test_tree = 'test_tree: %s\n" (if test_tree = test_tree' then "true" else "false")
;;

