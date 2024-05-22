(*Recall the problem of removing the duplicates from a list.
Please write another version of remove_duplicates that takes a list of integers and removes
any duplicates. For example:
# remove_duplicates [5; 1 ; 1 ; 3 ; 1 ; 5 ; 3 ; 1 ; 5 ; 17];;
- : int list = [5; 1; 3; 17]
This time you should take no more than O(n * log n) time and also preserve the order.
*)


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
  List.rev (aux init lst)
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


