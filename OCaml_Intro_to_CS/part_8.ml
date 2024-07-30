(*Recall the problem of removing the duplicates from a list.
Please write another version of remove_duplicates that takes a list of integers and removes
any duplicates. For example:
# remove_duplicates [5; 1 ; 1 ; 3 ; 1 ; 5 ; 3 ; 1 ; 5 ; 17];;
- : int list = [5; 1; 3; 17]
This time you should take no more than O(n * log n) time and also preserve the order.
*)

let reverse lst =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: acc) t
in 
aux [] lst;;


let element_of x hm =
  let rec aux = function
    | [] -> false
    | h :: t -> if h = x then true else aux t
  in aux hm
;;

let remove_duplicates lst = 
  let rec aux acc = function
    | [] -> acc
    | h :: t -> if element_of h acc then aux acc t else aux (h :: acc) t
  in
  let init = []
  in
  reverse (aux init lst)
;;

let print_list lst = 
  let str = String.concat ", " (List.map string_of_int lst) in
  Printf.printf "[%s]\n" str
;;

let () = 
  let result = remove_duplicates [5; 1 ; 1 ; 3 ; 1 ; 5 ; 3 ; 1 ; 5 ; 17]
  in 
  print_list result
;;


(* Please write tree_map : (‘a -> ‘b) -> ‘a tree -> ‘b tree that applies a
function to each element of a tree. *)

type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree
;;

let tree_map f tree =
  let rec aux = function
    | Leaf -> Leaf
    | Node (l, x, r) -> Node (aux l, f x, aux r)
in
aux tree
;;

let show_tree tree =
  let rec aux = function
    | Leaf -> "Leaf"
    | Node (l, x, r) -> Printf.sprintf "Node (%s, %d, %s)" (aux l) x (aux r)
  in
  aux tree
;;

let () = 
  let tree = Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf))
  in
  let result = tree_map (fun x -> x + 1) tree
  in
  Printf.printf "%s\n" (show_tree result)
;;

(* Suppose you have an ordered/sorted tree. Please write
listify : ‘a tree -> ‘a list
that creates a sorted list. *)


let listify : 'a tree -> 'a list = 
  let rec aux acc = function 
  | Leaf -> acc
  | Node (l, x, r) -> aux (x :: (aux acc r)) l
in
aux []
;;

let () = 
  let tree = Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf))
  in
  let result = listify tree
  in 
  print_list result
;;

(* Suppose you have a sorted list. Please write the function
treeify : ‘a list -> ‘a tree that builds a perfectly balanced sorted tree (other than perhaps a few
missing leaves in the bottom level if there are not enough elements to make a perfectly balanced
tree). For example: 
let t = treeify
[1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27;28;29;30;31]
*)

(* convert a sorted list to a binary tree *)

type 'a treeV2 =
  | Lf
  | Br of 'a * 'a treeV2 * 'a treeV2

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

let list_length lst = 
  let rec aux acc = function
    | [] -> acc
    | _ :: t -> aux (acc + 1) t
in
aux 0 lst;;

let index_of lst index =
  let rec aux acc = function
    | [] -> 0
    | h :: t -> if acc = index then h else aux (acc + 1) t
in
aux 0 lst;;

(* [first:last) *)
let partition_list l r lst =
  let rec aux curr acc = function
    | [] -> acc
    | h :: t -> if curr >= l && curr < r then aux (curr + 1) (h :: acc) t else aux (curr + 1) acc t
in
reverse (aux 0 [] lst);;

let treeify lst = 
  let rec aux = function
    | [] -> Lf
    | [x] -> Br (x, Lf, Lf)
    | lst -> 
      let n = list_length lst in
      let m = n / 2 in (* main node *)
      let l = partition_list 0 m lst in
      let r = partition_list (m+1) n lst in
      let x = index_of lst m in
      Br ( x, aux l, aux r)
    in
    aux lst;;

let tree_lst = [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27;28;29;30;31]

let t = treeify tree_lst;;

let () =
  print_tree t;
  print_newline ()
  ;;

let max v1 v2 = 
  if v1 > v2 then v1 else v2
;;

let height tree_struct = 
  let rec aux counter = function
    | Lf -> counter
    | Br (_, l, r) -> 
      let left_height = aux (counter + 1) l in
      let right_height = aux (counter + 1) r in
      max left_height right_height
    in
    aux 0 tree_struct
  ;;

let () = 
  let tree_height = height t in
  Printf.printf "Height of the tree: %d\n" tree_height
;;

